# Load packages
library(tidyverse)
library(data.table)
library(lubridate)
library(cmdstanr)
library(posterior)
library(readxl)
library(zoo)
library(abind)
library(splitstackshape)
setwd("~/Documents/GitHub/nowcasting_covid19/code")
# Import data
dat <- read_csv("../data/covid_deaths.csv")

evaluate_nowcast <- function(model, now) {

  FHM_ICU <- read_excel(paste0("../data/FoHM/Folkhalsomyndigheten_Covid19_", now, ".xlsx"),
    sheet = "Antal intensivvårdade per dag"
  )

  FHM_cases <- read_excel(paste0("../data/FoHM/Folkhalsomyndigheten_Covid19_", now, ".xlsx"),
    sheet = "Antal per dag region"
  ) %>% select(Statistikdatum, Totalt_antal_fall)

  now <- as.Date(now)
  
  dat_mod <- dat %>%
    filter(rep_date <= now)
  # Estimate nowcast model based on the previous 8 weeks
  start <- now - 8 * 7 + 1
  # max delay
  D_max <- 35

  # Create time series data frame
  ts <- FHM_ICU %>%
    select(date = Datum_vårdstart, n_icu = Antal_intensivvårdade) %>%
    left_join(FHM_cases %>% select(date = Statistikdatum, n_cases = Totalt_antal_fall)) %>%
    mutate(
      mean_7_c = rollmean(n_cases, k = 7, fill = NA, align = "center"),
      mean_7_c_lag = lag(mean_7_c, 7, fill = NA),
      lead_ind_cases = log(lag(mean_7_c, 19, fill = NA)),
      mean_7_i = rollmean(n_icu, 7, fill = NA, align = "center"),
      mean_7_i_lag = lag(mean_7_i, 7, fill = NA),
      lead_ind_icu = log(lag(mean_7_i, 14, fill = NA)),
      lead_ind_icu_f = lag(mean_7_i-mean_7_i_lag, 10, fill = NA),
      ratio_i = lag(mean_7_i / mean_7_i_lag, 7),
      ratio_c = log(lag(mean_7_c / mean_7_c_lag, 7))) %>%
    filter(
      date <= now,
      date >= start - 1
    ) 

  prepare_data_list <- function(dat,
                                now,
                                begin_date = start,
                                D = D_max) {
    # Nowcast dates range
    nc_range <- range(begin_date, now)

    # Reporting triangle
    cat("Building reporting triangle...\n")
    t02s <- seq(begin_date, now, by = "1 day")
    cap_T <- length(t02s)

    n <- matrix(NA, nrow = cap_T, ncol = cap_T, dimnames = list(
      as.character(t02s),
      NULL
    ))
    timeDelay <- function(d1, d2) {
      as.numeric(d2 - d1)
    }
    dat <- dat %>% mutate(delay = rep_date - death_date)

    for (t in 1:cap_T) {
      data.att <- dat[which(dat$death_date == t02s[t]), ]
      for (d in 0:(cap_T - t)) {
        n[t, d + 1] <- sum(data.att$delay == d)
      }
    }
   cat("No. cases: ", sum(n, na.rm = TRUE), "\n")
    nLongDelay <- rowSums(n[,(D + 1):cap_T], na.rm = T)
    if (any(nLongDelay > 0)) {
      warning(paste(sum(nLongDelay), " cases with a delay longer than D=",
        D, " days forced to have a delay of D days.\n",
        sep = ""
      ))
      n <- n[, 1:(D + 1)]
      n[, (D + 1)] <- n[, (D + 1)] + nLongDelay
    } else {
      n <- n[, 1:(D + 1)]
    } 
    # Replace unobserved dates with 0
    n[is.na(n)] <- 0

    ## Weekday W
    ## Additional part of the discrete survival model design matrix W = [ W_cp W_extra]
    # Make designmatrix for weekdays
    ## Make a factor for each "Wed", "Thu", "Fri" ("Tue" is reference)
    wdays <- 4:6
    # Create the extension of the W matrix
    W_wd <- array(NA,
      dim = c(cap_T, D + 1, length(wdays)),
      dimnames = list(as.character(t02s), paste("delay", 0:D, sep = ""), wdays)
    )

    # Loop over all times and lags
    for (t in seq_len(cap_T)) {
      for (w in seq_len(length(wdays))) {
        W_wd[t, , w] <- as.numeric(lubridate::wday(t02s[t] + 0:D, label = F) == wdays[w])
      }
    }
    
    # Changepoints (2 weeks)
    ddChangepoint <- sort((seq(now, begin_date + 14, by = "-2 weeks") + 1)[-1])
    
    W_cp <- array(NA,
                  dim = c(cap_T, D + 1, length(ddChangepoint)),
                  dimnames = list(
                    as.character(t02s),
                    paste("delay", 0:D, sep = ""),
                      as.character(ddChangepoint)
                    )
                  )
    
    for (t in 1:cap_T) {
      for (i in 1:length(ddChangepoint)) {
        if (i < length(ddChangepoint)) {
          W_cp[t, , i] <- as.numeric((t02s[t] + 0:D) > ddChangepoint[i] &
                                           (t02s[t] + 0:D) <= ddChangepoint[i] + 14)
        } else {
          # Dummy effect between two-week changepoints, distribution stays constant after last cp
          W_cp[t, , i] <- as.numeric((t02s[t] + 0:D) > ddChangepoint[i])
        } 
      }
    }
    
    for (t in 1:T + 1) {
      W_wd[t, , ][t(apply(W_wd[t, , ], 1, function(x) x - sd(x) < 0))] <- 0
    }
    
    W_wd_cp <- abind(W_wd, W_cp, along = 3)

    # Create Z matrix indicating non-reporting weekdays and public holidays 
    #("Mon", "Sat", "Sun" and holidays are one, else zero)
    holidays_list <- c("2020-10-31", "2020-12-24", "2020-12-25", "2020-12-26", 
                       "2020-12-31", "2021-01-01", "2021-01-06","2021-04-02", 
                       "2021-04-04", "2021-05-01", "2021-05-13","2021-05-23",
                       "2021-06-06","2021-06-26")
    
    Z <- array(NA,
      dim = c(length(t02s), D + 1),
      dimnames = list(as.character(t02s), paste("delay", 0:D, sep = ""))
    )

    # Loop over all times and lags
    for (t in seq_len(length(t02s))) {
      for (d in 0:D) {
        Z[t, d + 1] <- as.numeric(lubridate::wday(t02s[t] + d, label = F) %in% c(2, 7, 1)) 
        if(as.character(t02s[t] + d) %in% holidays_list){
          Z[t, d + 1] <- 1
        }
      }
    }

    # Combine all data/preprocessed objects into list
    list(
      cap_T = cap_T,
      maxDelay = D,
      n_wd = length(wdays),
      rT = n,
      W_wd = W_wd,
      W_wd_cp = W_wd_cp,
      t02s = t02s,
      Z = Z
    )
  }

  # Prepare the data
  prep_dat_list <- prepare_data_list(
    dat = dat_mod,
    now = now,
    begin_date = start,
    D = D_max # maximum delay
  )

  stan_mod <- substr(model, 1, 5)
  mod <- cmdstanr::cmdstan_model(paste0("./stan_models/", stan_mod, ".stan"))
  
  if(model == "mod_a"){
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay+1)
      ),
      seed = 1142,
      chains = 4,
      adapt_delta = 0.9,
      parallel_chains = 4
    )
    save_res <- c("N", "p", "logLambda")
  }
  
  if(model %in% c("mod_a_cp", "mod_a_ph")){
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay+1)
      ),
      seed = 1142,
      chains = 4,
      adapt_delta = 0.9,
      parallel_chains = 4
    )
    save_res <- c("N", "p", "logLambda")
  }
  
  
  if(model == "mod_b"){
  samples <- mod$sample(
    data = list(
      T = prep_dat_list$cap_T,
      D = prep_dat_list$maxDelay,
      r = prep_dat_list$rT,
      lead_ind = ts$lead_ind_icu,
      k_wd_haz = dim(aperm(prep_dat_list$W_wd, c(2, 1, 3)))[3],
      W_wd = aperm(prep_dat_list$W_wd, c(2, 1, 3)),
      Z = prep_dat_list$Z,
      alpha = rep(1, prep_dat_list$maxDelay +1)
    ),
    seed = 1142,
    adapt_delta = 0.9,
    chains = 4,
    parallel_chains = 4
  )
  save_res <- c("N", "p", "beta_0", "beta_1", "logLambda")
  }
  
  if(model %in% c("mod_b_cp", "mod_b_ph")){
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind = ts$lead_ind_icu,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay +1)
      ),
      seed = 1142,
      chains = 4,
      adapt_delta = 0.98,
      parallel_chains = 4
    )
    save_res <- c("N", "p", "beta_0", "beta_1", "logLambda")
  }
  
  if(model == "mod_b_cases"){
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind = ts$lead_ind_cases,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay +1)
      ),
      seed = 1142,
      chains = 4,
      adapt_delta = 0.95,
      parallel_chains = 4
    )
    save_res <- c("N", "p", "beta_0", "beta_1", "logLambda")
  }
  
  if(model == "mod_c"){
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind_1 = ts$lead_ind_icu,
        lead_ind_2 = ts$lead_ind_cases,
        adapt_delta = 0.9,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay + 1)
      ),
      seed = 1142,
      chains = 4,
      parallel_chains = 4
    )
    save_res <- c("N", "p", "beta_0", "beta_1", "beta_2", "logLambda")
  }
  
  if(model == "mod_c_cp"){
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind_1 = ts$mean_7_i_lag,
        lead_ind_2 = ts$mean_7_c_lag,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        adapt_delta = 0.9,
        alpha = rep(1, prep_dat_list$maxDelay + 1)
      ),
      seed = 1142,
      chains = 4,
      parallel_chains = 4
    )
    save_res <- c("N", "p", "beta_0", "beta_1", "beta_2", "logLambda")
  }
  
  if(model == "mod_d"){
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind = ts$ratio_i,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay + 1)
      ),
      seed = 1142,
      chains = 4,
      parallel_chains = 4,
      adapt_delta = 0.98,
      max_treedepth = 15
    )
    warn <- names(warnings()) 
    save_res <- c("N", "p", "beta_0", "beta_1", "logLambda")
  }
  
  if(model %in% c("mod_d_new", "mod_d_ph")){
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind = ts$lead_ind_icu_f,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay + 1)
      ),
      seed = 1142,
      chains = 4,
      parallel_chains = 4,
      adapt_delta = 0.98,
      max_treedepth = 15
    )
    warn <- names(warnings()) 
    save_res <- c("N", "p", "beta_0", "beta_1","logLambda")
  }
  
  if(model == "mod_e"){
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind_1 = ts$ratio_i,
        lead_ind_2 = ts$ratio_c,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay + 1)
      ),
      seed = 1142,
      chains = 4,
      parallel_chains = 4,
      adapt_delta = 0.95,
      max_treedepth = 15
    )
    warn <- names(warnings()) 
    save_res <- c("N", "p", "beta_0", "beta_1", "beta_2", "logLambda")
  }
  
  
  if(model %in% c("mod_f", "mod_f_ph")){
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind = ts$lead_ind_icu_f,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay + 1)),
      seed = 1142,
      chains = 4,
      parallel_chains = 4,
      adapt_delta = 0.97,
      max_treedepth = 15
    )
    warn <- names(warnings()) 
    save_res <- c("N", "p", "beta_1", "logLambda")
  }
  # Save warnings and results
  samples$cmdstan_diagnose() %>% 
    as.data.frame() %>%
    write_csv(paste0("../results/warnings/", model, "_", now, ".csv"))

  samples$summary(c("phi", "sigma", save_res)) %>%
    as.data.frame() %>%
    write_csv(paste0("../results/summary/", model, "_", now, ".csv"))

  for (i in 1:length(save_res)) {
    samples$draws(save_res[i]) %>%
      as.data.frame() %>%
      gather(key, value) %>%
      mutate(key = str_remove(key, "..")) %>%
      pivot_wider(value, key, values_fn = list) %>%
      flatten() %>%
      as.data.frame() %>%
      write_csv(file = paste0("../results/", save_res[i], "/", save_res[i], "_", model, "_", now, ".csv"))
  }
}

# Restrict dataset to a specific nowcast date
rep_dates <- dat %>%
  select(rep_date) %>%
  filter(rep_date >= "2020-09-15") %>%  #, rep_date <= "2021-05-31")  %>%
  distinct() %>% 
  t() %>%
  as.vector()


#for(i in 101:120){
date <- "2020-11-17" #rep_dates[i]
model_spec <- "mod_f_ph"
lapply(model_spec , evaluate_nowcast, date)
#}

