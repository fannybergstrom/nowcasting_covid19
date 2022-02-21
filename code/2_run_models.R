# Load packages
library(tidyverse)
library(data.table)
library(lubridate)
library(cmdstanr)
library(posterior)
library(readxl)
library(zoo)
library(splitstackshape)

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
      mean_7_c = rollmean(n_cases, k = 7, fill = NA, align = "right"),
      mean_7_c_lag = lag(mean_7_c, k = 7, fill = NA),
      mean_7_i = rollmean(n_icu, k = 7, fill = NA, align = "right"),
      mean_7_i_lag = lag(mean_7_i, k = 7, fill = NA),
      diff_mean7_i = as.numeric(diff(as.zoo(mean_7_i), lag = 7, na.pad = T)),
      ratio_i = log(lag(mean_7_i / mean_7_i_lag, 1)),
      ratio_c = log(lag(mean_7_c / mean_7_c_lag, 7))
    ) %>%
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
    T <- length(t02s) - 1

    n <- matrix(NA, nrow = T + 1, ncol = T, dimnames = list(
      as.character(t02s),
      NULL
    ))
    timeDelay <- function(d1, d2) {
      as.numeric(d2 - d1)
    }
    dat <- dat %>% mutate(delay = rep_date - death_date)

    for (t in 0:T) {
      data.att <- dat[which(dat$death_date == t02s[t + 1]), ]
      for (d in 1:(T - t)) {
        n[t + 1, d] <- sum(data.att$delay == d)
      }
    }
    cat("No. cases: ", sum(n, na.rm = TRUE), "\n")
    nLongDelay <- apply(n[, (D) + seq_len(T - D), drop = FALSE],
      1, sum,
      na.rm = TRUE
    )
    if (any(nLongDelay > 0)) {
      warning(paste(sum(nLongDelay), " cases with a delay longer than D=",
        D, " days forced to have a delay of D days.\n",
        sep = ""
      ))
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
    Wextra <- array(NA,
      dim = c(length(t02s), D, length(wdays)),
      dimnames = list(as.character(t02s), paste("delay", 1:D, sep = ""), wdays)
    )

    ddChangepoint <- sort((seq(now, begin_date + 14, by = "-2 weeks") + 1)[-1])

    # Loop over all times and lags
    for (t in seq_len(length(t02s))) {
      for (w in seq_len(length(wdays))) {
        Wextra[t, , w] <- as.numeric(lubridate::wday(t02s[t] + 1:D, label = FALSE) == wdays[w])
        # Deviation coding
        Wextra[t, , w] <- Wextra[t, , w] - as.numeric(lubridate::wday(t02s[t] + 1:D, label = FALSE) == 3)
      }
    }

    W_wd <- array(NA,
      dim = c(T + 1, D, length(ddChangepoint) + length(wdays)),
      dimnames = list(
        as.character(t02s),
        paste("delay", 1:D, sep = ""),
        c(
          as.character(ddChangepoint),
          as.character(wdays)
        )
      )
    )
    for (t in 0:T) {
      for (i in 1:length(ddChangepoint)) {
        # W_wd[t + 1, ,i] = pmax(0, as.numeric((t02s[t+1] + 1:D) - ddChangepoint[i]))/(T+1) # linear effect
        if (i < length(ddChangepoint)) {
          W_wd[t + 1, , i] <- as.numeric((t02s[t + 1] + 1:D) > ddChangepoint[i] &
            (t02s[t + 1] + 1:D) <= ddChangepoint[i] + 14)
        } else {
          W_wd[t + 1, , i] <- as.numeric((t02s[t + 1] + 1:D) > ddChangepoint[i])
        } # Dummy effect between two-week changepoints, distribution stays constant after last cp
      }
      W_wd[t + 1, , (length(ddChangepoint) + 1):(length(ddChangepoint) + length(wdays))] <- Wextra[t + 1, , ]

      # Effect coding - change point effect
      for (j in 1:D) {
        if (sum(W_wd[t + 1, j, 1:length(ddChangepoint)]) == 0) {
          W_wd[t + 1, j, 1:length(ddChangepoint)] <- rep(-1, length(ddChangepoint))
        }
      }
    }

    for (t in 1:T + 1) {
      W_wd[t, , ][t(apply(W_wd[t, , ], 1, function(x) x - sd(x) < 0))] <- 0
    }

    # Create Z matrix indicating non-reporting weekdays ("Mon", "Sat", "Sun" are one, else zero)
    Z <- array(NA,
      dim = c(length(t02s), D),
      dimnames = list(as.character(t02s), paste("delay", 1:D, sep = ""))
    )

    # Loop over all times and lags
    for (t in seq_len(length(t02s))) {
      for (d in 1:D) {
        Z[t, d] <- as.numeric(lubridate::wday(t02s[t] + d, label = F) %in% c(2, 7, 1))
      }
    }

    # Combine all data/preprocessed objects into list
    list(
      T = T + 1,
      maxDelay = D,
      n_cp = length(ddChangepoint),
      n_wextra = length(wdays),
      rT = n,
      W_wd = W_wd,
      eta_mu_wd = rep(0, length(ddChangepoint) + length(wdays)),
      eta_sd_wd = c(
        rep(0.01, length(ddChangepoint)),
        rep(0.5, length(wdays))
      ),
      t02s = t02s,
      Z = Z,
      x = NA
    )
  }

  # Prepare the data
  prep_dat_list <- prepare_data_list(
    dat = dat_mod,
    now = now,
    begin_date = start,
    D = D_max # maximum delay
  )

  mod_b <- cmdstanr::cmdstan_model(paste0("./stan_models/", model, ".stan"))

  samples <- mod_b$sample(
    data = list(
      T = prep_dat_list$T,
      D = prep_dat_list$maxDelay,
      r = prep_dat_list$rT,
      lead_ind = ts$mean_7_i_lag,
      k_wd_haz = dim(aperm(prep_dat_list$W_wd, c(2, 1, 3)))[3],
      W_wd = aperm(prep_dat_list$W_wd, c(2, 1, 3)),
      Z = prep_dat_list$Z,
      alpha = rep(1, prep_dat_list$maxDelay)
    ),
    seed = 1142,
    chains = 4,
    parallel_chains = 4
  )

  # Save warnings and results
  names(warnings()) %>%
    as.data.frame() %>%
    write_csv(paste0("../results/warnings/", model, "_", now, ".csv"))

  samples$summary(c("beta_0", "beta_1", "phi", "sigma", "N")) %>%
    as.data.frame() %>%
    write_csv(paste0("../results/summary/", model, "_", now, ".csv"))

  save_res <- c("N", "p", "beta_1", "beta_0", "logLambda")
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
  filter(rep_date >= "2020-10-01", rep_date <= "2021-05-31") %>%
  distinct() %>%
  t() %>%
  as.vector()

date <- ymd("2021-01-08") #rep_dates[20]
model <- "mod_b"
evaluate_nowcast(model, date)
