#### This document is for functions used to perform the Nowcasting 
#### and summarizing the results 

## Load packages
pacman::p_load(
  tidyverse, data.table, lubridate, readxl, readr, ggpubr,
  zoo, splitstackshape, cmdstanr, posterior, abind, wesanderson,
  scoringRules, scales, ggsci, extrafont, patchwork, viridis,
  surveillance
)


## Function for computing reporting triangle and prepaire data
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
  nLongDelay <- rowSums(n[, (D + 1):cap_T], na.rm = T)
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

  # Weekday W
  # Additional part of the discrete survival model design matrix W = [ W_cp W_extra]
  # Make designmatrix for weekdays
  # Make a factor for each "Wed", "Thu", "Fri" ("Tue" is reference)
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

  # Changepoints (4 weeks)
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
  # ("Mon", "Sat", "Sun" and holidays are one, else zero)
  holidays_list <- c(
    "2020-10-31", "2020-12-24", "2020-12-25", "2020-12-26",
    "2020-12-31", "2021-01-01", "2021-01-06", "2021-04-02",
    "2021-04-04", "2021-05-01", "2021-05-13", "2021-05-23",
    "2021-06-06", "2021-06-26", "2021-11-06", "2021-12-25",
    "2021-12-26", "2022-01-01", "2022-01-06", "2022-04-15",
    "2022-04-17", "2022-04-18", "2022-05-01", "2022-05-26",
    "2022-06-05", "2022-06-06", "2022-06-25", "2022-11-05",
    "2022-12-25", "2022-12-26"
  )

  Z <- array(NA,
    dim = c(length(t02s), D + 1),
    dimnames = list(as.character(t02s), paste("delay", 0:D, sep = ""))
  )

  # Loop over all times and lags
  for (t in seq_len(length(t02s))) {
    for (d in 0:D) {
      Z[t, d + 1] <- as.numeric(lubridate::wday(t02s[t] + d, label = F) %in% c(2, 7, 1))
      if (as.character(t02s[t] + d) %in% holidays_list) {
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


## Function for performing the nowcasts and saving results
evaluate_nowcast <- function(dat, model, now, D_max = 35) {

  # Start running time
  start_time <- Sys.time()

  # Import additional data sourses
  FHM_ICU <- read_excel(paste0("./data/fohm/fohm_covid19_", now, ".xlsx"),
    sheet = "Antal intensivvårdade per dag"
  )

  FHM_cases <- read_excel(paste0("./data/fohm/fohm_covid19_", now, ".xlsx"),
    sheet = "Antal per dag region"
  ) %>% select(Statistikdatum, Totalt_antal_fall)

  now <- as.Date(now)

  dat_mod <- dat %>%
    filter(rep_date <= now) %>%
    expandRows(., "n")

  # Estimate nowcast model based on the previous 8 weeks
  start <- now - 8 * 7 + 1

  # Create time series data frame
  ts <- FHM_ICU %>%
    select(date = Datum_vårdstart, n_icu = Antal_intensivvårdade) %>%
    left_join(FHM_cases %>% select(date = Statistikdatum, n_cases = Totalt_antal_fall)) %>%
    mutate(
      mean_7_c = rollmean(n_cases, k = 7, fill = NA, align = "center"),
      mean_7_c_lag = lag(mean_7_c, 7, fill = NA),
      lead_ind_cases_l = lag(mean_7_c, 19, fill = NA),
      mean_7_i = rollmean(n_icu, 7, fill = NA, align = "center"),
      mean_7_i_lag = lag(mean_7_i, 7, fill = NA),
      lead_ind_icu_l = lag(mean_7_i, 14, fill = NA),
      lead_ind_icu_rl = lag((mean_7_i - mean_7_i_lag), 7, fill = NA),
      lead_ind_cases_rl = lag(mean_7_c - mean_7_c_lag, 12, fill = NA)
    ) %>%
    filter(
      date <= now,
      date >= start - 1
    )

  # Prepare the data
  prep_dat_list <- prepare_data_list(
    dat = dat_mod,
    now = now,
    begin_date = start,
    D = D_max # maximum delay
  )
  
  # Get stan model  
  stan_mod <- str_remove(model, "_cases")
  mod <- cmdstanr::cmdstan_model(paste0("./code/stan_models/", stan_mod, ".stan"))

  if (model %in% c("mod_r", "mod_r2")) {
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay + 1)
      ),
      seed = 1142,
      chains = 4,
      adapt_delta = 0.98,
      parallel_chains = 4,
      max_treedepth = 15
    )
    save_res <- c("N", "p", "logLambda")
  }

  if (model == "mod_l") {
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind = ts$lead_ind_icu_l,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay + 1)
      ),
      seed = 1142,
      chains = 4,
      adapt_delta = 0.98,
      parallel_chains = 4
    )
    save_res <- c("N", "p", "beta_0", "beta_1", "logLambda")
  }

  if (model == "mod_l_cases") {
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind = ts$lead_ind_cases_l,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay + 1)
      ),
      seed = 1142,
      chains = 4,
      adapt_delta = 0.95,
      parallel_chains = 4
    )
    save_res <- c("N", "p", "beta_0", "beta_1", "logLambda")
  }

  if (model == "mod_l_2") {
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind_1 = ts$lead_ind_icu_l,
        lead_ind_2 = ts$lead_ind_cases_l,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        adapt_delta = 0.99,
        alpha = rep(1, prep_dat_list$maxDelay + 1)
      ),
      seed = 4142,
      chains = 4,
      parallel_chains = 4,
      max_treedepth = 15
    )
    save_res <- c("N", "p", "beta_0", "beta_1", "beta_2", "logLambda")
  }

  if (model == "mod_rl") {
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind = ts$lead_ind_icu_rl,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay + 1)
      ),
      seed = 2142,
      chains = 4,
      parallel_chains = 4,
      adapt_delta = 0.99,
      max_treedepth = 15
    )
    save_res <- c("N", "p", "beta_1", "logLambda")
  }

  if (model == "mod_rl_cases") {
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind = ts$lead_ind_cases_rl,
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
    save_res <- c("N", "p", "beta_1", "logLambda")
  }

  if (model == "mod_rl_2") {
    samples <- mod$sample(
      data = list(
        T = prep_dat_list$cap_T,
        D = prep_dat_list$maxDelay,
        r = prep_dat_list$rT,
        lead_ind_1 = ts$lead_ind_icu_rl,
        lead_ind_2 = ts$lead_ind_cases_rl,
        k_wd_haz = dim(aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)))[3],
        W_wd = aperm(prep_dat_list$W_wd_cp, c(2, 1, 3)),
        Z = prep_dat_list$Z,
        alpha = rep(1, prep_dat_list$maxDelay + 1)
      ),
      seed = 1142,
      chains = 4,
      parallel_chains = 4,
      adapt_delta = 0.99,
      max_treedepth = 15
    )
    save_res <- c("N", "p", "beta_1", "beta_2", "logLambda")
  }

  end_time <- Sys.time()

  # Save warnings and results
  samples$cmdstan_diagnose() %>%
    as.data.frame() %>%
    write_csv(paste0("./results/warnings/", model, "_", now, ".csv"))

  samples$summary(c("phi", "sigma", save_res)) %>%
    as.data.frame() %>%
    mutate(run_time = difftime(end_time, start_time, units = "mins")) %>%
    write_csv(paste0("./results/summary/", model, "_", now, ".csv"))

  for (j in 1:length(save_res)) {
    samples$draws(save_res[j]) %>%
      as.data.frame() %>%
      gather(key, value) %>%
      mutate(key = str_split(key, "\\.", simplify = T)[, 2]) %>%
      pivot_wider(names_from = key, values_from = value, values_fn = list) %>%
      flatten() %>%
      as.data.frame() %>%
      write_csv(file = paste0("./results/", save_res[j], "/", save_res[j], "_", model, "_", now, ".csv"))
  }
}


## Function for creating results table
med_and_quantiles <- function(sample_list) {
  df_med <- c()
  for (i in 1:length(sample_list)) {
    l <- sample_list[[i]]
    now <- ymd(rep_dates[i])
    start <- now - 7 * 8 + 1
    post <- tibble(
      date = seq(start, now, "1 day"),
      med = apply(l, 2, median),
      q5 = apply(l, 2, function(x) quantile(x, .025)),
      q95 = apply(l, 2, function(x) quantile(x, .975))
    ) %>%
      mutate(now = now, delay = 55:0)
    df_med <- bind_rows(df_med, post)
  }
  df_med %>% filter(delay <= 35)
}


##### Function for calculating scores and PI coverage
calculate_scores <- function(N_list, m_delay = 6, rep_date = rep_dates) {
  res_rmse <- res_crps <- res_logs <- pi_75 <- pi_90 <- pi_95 <- matrix(NA, length(N_list), (m_delay + 1))
  for (i in 1:length(N_list)) {
    for (j in 1:(m_delay + 1)) {
      v <- N_list[[i]][, (56 + 1 - j)] %>% unlist()
      truth <- retro_truth %>%
        filter(date == as.Date(rep_date[i]) - j + 1) %>%
        select(n_true_retro) %>%
        unlist()
      res_rmse[i, j] <- sqrt((median(v) - truth)^2)
      res_logs[i, j] <- logs_sample(y = truth, dat = v)
      res_crps[i, j] <- crps_sample(y = truth, dat = v)
      pi_75[i, j] <- between(truth, quantile(v, .125), quantile(v, .875))
      pi_90[i, j] <- between(truth, quantile(v, .05), quantile(v, .95))
      pi_95[i, j] <- between(truth, quantile(v, .025), quantile(v, .975))
    }
  }
  list(
    rmse = res_rmse,
    logs = res_logs,
    crps = res_crps,
    pi_75 = pi_75,
    pi_90 = pi_90,
    pi_95 = pi_95
  )
}

