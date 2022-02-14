# Load packages
library(tidyverse)
library(data.table)
library(cmdstanr)
library(readxl)
library(zoo)
library(splitstackshape)

# Import data, rename and filter to reporting data after 2020-10-01
dat <- read_csv("../data/covid_deaths_latest.csv") %>% 
  filter(date > as_date("2020-04-01"), n_diff > 0) %>%
  mutate(rep_date = publication_date, death_date = date) %>%
  select(death_date, n_diff, rep_date) %>%
  expandRows(., "n_diff") %>%
  mutate(rep_date_wd = wday(rep_date, label = F)) %>%
  filter(rep_date>=ymd("2020-07-01"))

# Restrict dataset to a specific nowcast date
rep_dates <- dat %>% select(rep_date) %>%
  filter(rep_date >= "2020-09-29") %>% #, rep_date <= "2021-04-16") %>% 
  distinct()  %>% t() %>% as.vector() 

now <- ymd("2021-05-20")

dat_mod = dat %>%
  filter(rep_date <= now)
# Estimate nowcast model based on the previous 10 weeks
start = now - 8 * 7 + 1
# max delay 
D_max <- 35


FHM_ICU <- read_excel(paste0("../data/FoHM/Folkhalsomyndigheten_Covid19_", now+1, ".xlsx"), 
                      sheet = "Antal intensivvårdade per dag") 

FHM_cases <- read_excel(paste0("../data/FoHM/Folkhalsomyndigheten_Covid19_", now+1, ".xlsx"),
                        sheet = "Antal per dag region") %>% select(Statistikdatum, Totalt_antal_fall)


# Create time series data frame
ts <- FHM_ICU %>% select(date = Datum_vårdstart, n_icu = Antal_intensivvårdade) %>% 
  left_join(FHM_cases %>% select(date = Statistikdatum, n_cases = Totalt_antal_fall)) %>% 
  mutate(
    sum_7_c = rollsum(n_cases, k = 7, fill = NA, align = 'right'),
    sum_7_c_lag = lag(sum_7_c, k = 7, fill = NA),
    sum_7_i = rollsum(n_icu, k = 7, fill = NA, align = 'right'),
    sum_7_i_lag = lag(sum_7_i, k = 7, fill = NA),
    diff_sum7_i = as.numeric(diff(as.zoo(sum_7_i), lag= 7, na.pad=T)),
    sum_7_log_i = log(rollsum(n_icu, k = 7, fill = NA, align = 'right')),
    diff_sum7_log_i = as.numeric(diff(as.zoo(sum_7_log_i), lag= 7, na.pad=T)),
    ratio_i = log(lag(sum_7_i/sum_7_i_lag, 1)),
    ratio_c = log(lag(sum_7_c/sum_7_c_lag, 7)),
    diff_c = sum_7_c-sum_7_c_lag) %>%
  filter(date <= now,
         date >= start)


dat_mod %>% group_by(date=death_date) %>% summarise(n_death=n()) %>% 
  filter(date>=ymd(start)) %>% right_join(tibble(date=seq(start, now, "1 day"))) %>%
  mutate(n_death=replace_na(n_death, 0)) %>%
  left_join(ts %>% select(date, sum_7_i_lag)) %>% ggplot() + geom_line(aes(date, n_death)) + geom_line(aes(date, sum_7_i_lag/7))


prepare_data_list = function(dat,
                             now,
                             begin_date = start,
                             D = D_max,
                             lag = 0) {
  # Nowcast dates range
  nc_range <- range(begin_date, now)
  
  sts <- linelist2sts(
    as.data.frame(dat),
    dateCol="death_date",
    aggregate.by = "1 day",
    dRange = nc_range)
  
  
  # Reporting triangle
  cat("Building reporting triangle...\n")
  t02s <- seq(begin_date, now, by = "1 day")
  T <- length(t02s) - 1
  
  n <- matrix(NA, nrow = T + 1, ncol = T, dimnames = list(as.character(t02s),
                                                          NULL))
  timeDelay <- function(d1, d2) {
    as.numeric(d2 - d1)
  }
  dat = dat %>% mutate(delay = rep_date - death_date)
  
  for (t in 0:T) {
    data.att <- dat[which(dat$death_date == t02s[t+1]), ]
    for (x in 1:(T - t)) {
      n[t + 1, x] <- sum(data.att$delay == x)
    }
  }
  cat("No. cases: ", sum(n, na.rm = TRUE), "\n")
  nLongDelay <- apply(n[, (D) + seq_len(T - D), drop = FALSE],
                      1, sum, na.rm = TRUE)
  if (any(nLongDelay > 0)) {
    warning(paste(sum(nLongDelay), " cases with a delay longer than D=",
                  D, " days forced to have a delay of D days.\n", sep = ""))
    n <- n[, 1:(D)]
    n[, (D)] <- n[, (D)] + nLongDelay
  } else {
    n <- n[, 1:(D)]
  }
  # Replace unobserved dates with 0
  n[is.na(n)] <- 0
  
  ## Weekday W
  ## Additional part of the discrete survival model design matrix W = [ W_cp W_extra]
  # Make designmatrix for weekdays
  ## Make a factor for each "Wed", "Thu", "Fri" ("Tue" is reference)
  wdays <- 4:6
  # Create the extension of the W matrix
  Wextra <- array(NA, dim=c(length(t02s), D, length(wdays)),
                  dimnames=list(as.character(t02s), paste("delay",1:D,sep=""), wdays))
  
  ddChangepoint <- sort((seq(now, begin_date+14, by = "-2 weeks")+1)[-1])
  
  # Loop over all times and lags
  for (t in seq_len(length(t02s))) {
    for (w in seq_len(length(wdays))) {
      Wextra[t,, w] <- as.numeric(lubridate::wday(t02s[t] + 1:D, label=FALSE) == wdays[w])
      #Deviation coding
      Wextra[t,, w] <- Wextra[t,, w] - as.numeric(lubridate::wday(t02s[t] + 1:D, label=FALSE) == 3)
    }
  }
  
  W_wd = array(NA, dim = c(T + 1, D, length(ddChangepoint) + length(wdays)),
               dimnames = list(as.character(t02s),
                               paste("delay", 1:D, sep = ""),
                               c(as.character(ddChangepoint),
                                 as.character(wdays))))
  for (t in 0:T) {
    for (i in 1:length(ddChangepoint)) {
      # W_wd[t + 1, ,i] = pmax(0, as.numeric((t02s[t+1] + 1:D) - ddChangepoint[i]))/(T+1) # linear effect
      if(i<length(ddChangepoint)) {
        W_wd[t + 1, ,i] = as.numeric((t02s[t+1] + 1:D) > ddChangepoint[i] &
                                       (t02s[t+1] + 1:D) <= ddChangepoint[i] + 14)
      } else {
        W_wd[t + 1, ,i] = as.numeric((t02s[t+1] + 1:D) > ddChangepoint[i])
      }# Dummy effect between two-week changepoints, distribution stays constant after last cp
      
    }
    W_wd[t+1, , (length(ddChangepoint)+1):(length(ddChangepoint) + length(wdays))] = Wextra[t+1,,]
    
    # Effect coding - change point effect
    for(j in 1:D){
      if(sum(W_wd[t+1, j, 1:length(ddChangepoint)])==0){
        W_wd[t+1, j, 1:length(ddChangepoint)] = rep(-1,length(ddChangepoint))
      }
    }
  }
  
  
  for (t in 0:T) {
    
    W_wd[t+1,,][t(apply(W_wd[t+1,,], 1, function(x) x - sd(x) < 0))] <- 0
  }
  
  # Create Z matrix indicating non-reporting weekdays ("Mon", "Sat", "Sun" are one, else zero)
  Z <- array(NA, dim=c(length(t02s), D),
             dimnames=list(as.character(t02s), paste("delay",1:D,sep="")))
  
  # Loop over all times and lags
  for (t in seq_len(length(t02s))) {
    for (d in 1:D) {
      Z[t,d] <- as.numeric(lubridate::wday(t02s[t]+d, label=F) %in% c(2, 7, 1))
    }
  }
  
  # Additional time series
  
  
  # Choose transformation 
  x <- ts$ratio_i 
  # Standardise
  #x <- x/sd(x)
  
  # Combine all data/preprocessed objects into list
  list(T = T+1,
       maxDelay = D,
       n_cp = length(ddChangepoint),
       n_wextra = length(wdays),
       rT = n,
       W_wd=W_wd,
       eta_mu_wd = rep(0, length(ddChangepoint) + length(wdays)),
       eta_sd_wd = c(rep(0.01, length(ddChangepoint)),
                     rep(0.5, length(wdays))),
       t02s = t02s,
       Z = Z,
       x = x)
}

# Prepare the data
prep_dat_list = prepare_data_list(dat = dat_mod,
                                  now = now,
                                  begin_date = start,
                                  D = D_max # maximum delay 35 days
)

mod_b=cmdstanr::cmdstan_model("./stan_models/mod_b.stan")

samples = mod_b$sample(data = list(T=prep_dat_list$T,
                                   D=prep_dat_list$maxDelay,
                                   r=prep_dat_list$rT,
                                   lead_ind = ts$sum_7_i_lag,
                                   k_wd_haz = dim(aperm(prep_dat_list$W_wd, c(2,1,3)))[3],
                                   W_wd = aperm(prep_dat_list$W_wd, c(2,1,3)),
                                   Z=prep_dat_list$Z,
                                   alpha=rep(1, prep_dat_list$maxDelay)), 
                       seed = 1142, 
                       chains = 4, 
                       parallel_chains = 4)

names(warnings()) %>% as.data.frame() %>% write_csv("../results/warnings/stan_model.csv")

sum = samples$summary(c("beta_0", "beta_1", "phi", "sigma"))
sum

samples$summary("N") %>% cbind(tibble(date=seq(now-D_max+1, now, "1 day"))) %>%
  left_join(dat %>% group_by(date=death_date) %>% summarise(truth=n())) %>%
  left_join(dat %>% filter(rep_date<=now) %>% group_by(date=death_date) %>% 
              summarise(reported=n())) %>%
  left_join(ts %>% mutate(exp = exp(as.numeric(sum[1,3]) + as.numeric(sum[2,3])*sum_7_i_lag)) %>% select(date, exp)) %>%
  ggplot() + geom_col(aes(date, reported)) +
  geom_line(aes(date, median)) +
  geom_ribbon(aes(date, ymin=q5, ymax=q95), alpha=.2) +
  geom_line(aes(date, exp), lty=2) +
  geom_line(aes(date, truth), col="green", lty=2)




samples$draws("N")

N_draws <- samples$draws("N")[,,1]
N_draws

# Save results
save_res <- c("N", "p", "p_d1_pr", "beta_1", "beta_0", "logLambda")
for(i in 1:length(save_res)){
  write_csv(as.data.frame(res_m1[save_res[i]]), file = paste0("../results/", save_res[i],"/", save_res[i],"_", model,"_", start, "_now_", now, ".csv"))
}

samples$draws[1:25,c(1,3),1]




