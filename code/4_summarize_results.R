# This document is for summarizing the nowcasting results

# Load packages
pacman::p_load(
  tidyverse, data.table, lubridate, surveillance, 
  readxl, readr, zoo, splitstackshape, cmdstanr,
  posterior, abind, scoringRules)

# Import data
dat <- read_csv("./data/covid_deaths.csv")

# Restrict dataset to a specific nowcast date
rep_dates <- list.files(path = paste0("./data/fohm/")) %>% 
  str_extract("\\d+-\\d+-\\d+") %>%  
  as.data.frame %>% 
  filter(. >= "2020-09-15") %>%
  distinct() %>% 
  t() %>%
  as.vector()

retro_truth <- dat %>% group_by(date=death_date) %>%
  summarise(n_true_retro=sum(n))

list_files <- function(parameter, mod){
  files <- list.files(path = paste0("./results/", parameter))
  files[str_detect(files, mod)]
}

# List files
n <- 116
files_mod_a <- list_files("N", "mod_a_ph")[21:(21+n)]
files_mod_b <- list_files("N", "mod_b_cp")[21:(21+n)]
files_mod_b_c <- list_files("N", "mod_b_cases")[1:n]
files_mod_c <- list_files("N", "mod_c_ph")[1:n]
files_mod_d <- list_files("N", "mod_d_new2")[21:(21+n)]
files_mod_d_c <- list_files("N", "mod_d_new_cases")[1:n]
files_mod_e <- list_files("N", "mod_e_ph")[1:n]


rep_dates <- files_mod_a %>% 
  str_extract("\\d+-\\d+-\\d+") %>%  
  as.data.frame %>% 
  filter(. >= "2020-09-15") %>%
  distinct() %>% 
  t() %>%
  as.vector()


# Read files
N_mod_a <- lapply(paste0("./results/N/", files_mod_a), read_csv)
N_mod_b <- lapply(paste0("./results/N/", files_mod_b), read_csv)
#N_mod_b_c <- lapply(paste0("./results/N/", files_mod_b_c), read_csv)
#N_mod_c <- lapply(paste0("./results/N/", files_mod_c), read_csv)
N_mod_d <- lapply(paste0("./results/N/", files_mod_d), read_csv)
#N_mod_d_c <- lapply(paste0("./results/N/", files_mod_d_c), read_csv)
#N_mod_e <- lapply(paste0("./results/N/", files_mod_e), read_csv)

# Function for creating results table
med_and_quantiles <- function(sample_list){
  df_med <- c()
  for(i in 1:length(sample_list)){
    l <-sample_list[[i]]
    now <- ymd(rep_dates[i])
    start <- now - 7 * 8 + 1
    post = tibble(date = seq(start, now, "1 day"),
                  med = apply(l, 2, median),
                  q5 = apply(l, 2, function(x) quantile(x, .025)),
                  q95 = apply(l, 2, function(x) quantile(x, .975))) %>% 
      mutate(now = now, delay = 55:0)
    df_med <- bind_rows(df_med, post)
  }
  df_med %>% filter(delay <= 35)
}

N_a_df <-med_and_quantiles(N_mod_a)
N_b_df <-med_and_quantiles(N_mod_b)
N_d_df <-med_and_quantiles(N_mod_d)

##### Scores
calculate_scores <- function(N_list, m_delay = 7){
  res_rmse <- res_crps <- res_logs <- pi_75 <- pi_90 <- pi_95 <- matrix(NA, length(N_list), m_delay)
  for(i in 1:length(N_list)){
    for(j in 1:m_delay){
      v <- N_list[[i]][,(56+1-j)] %>% unlist()
      truth <- retro_truth %>% filter(date == as.Date(rep_dates[i])-j+1) %>% select(n_true_retro) %>% unlist()
      res_rmse[i,j] <- sqrt((median(v)-truth)^2)
      res_logs[i,j] <- logs_sample(y =  truth, dat = v + rnorm(1000, 0, 0.4)) 
      res_crps[i,j] <- crps_sample(y = truth, dat = v)
      pi_75[i,j] <- between(truth, quantile(v, .125), quantile(v, .875))
      pi_90[i,j] <- between(truth, quantile(v, .05), quantile(v, .95))
      pi_95[i,j] <- between(truth, quantile(v, .025), quantile(v, .975))
    }
  }
  list(rmse = res_rmse,
       logs = res_logs,
       crps = res_crps,
       pi_75 = pi_75,
       pi_90 = pi_90,
       pi_95 = pi_95
       
       )
}

Nres_mod_a <- calculate_scores(N_mod_a)
Nres_mod_b <- calculate_scores(N_mod_b) 
Nres_mod_d <- calculate_scores(N_mod_d) 

c(Nres_mod_a$rmse %>% mean(), 
  Nres_mod_b$rmse %>% mean(), 
  Nres_mod_d$rmse %>% mean())

c(Nres_mod_a$logs %>% mean(),
  Nres_mod_b$logs %>% mean(),
  Nres_mod_d$logs %>% mean())

c(Nres_mod_a$crps %>% mean(),
  Nres_mod_b$crps %>% mean(), 
  Nres_mod_d$crps %>% mean())

c(Nres_mod_a$pi_75 %>% mean(),
  Nres_mod_b$pi_75 %>% mean(),
  Nres_mod_d$pi_75 %>% mean())

c(Nres_mod_a$pi_90 %>% mean(),
  Nres_mod_b$pi_90 %>% mean(),
  Nres_mod_d$pi_90 %>% mean())

c(Nres_mod_a$pi_95 %>% mean(),
  Nres_mod_b$pi_95 %>% mean(),
  Nres_mod_d$pi_95 %>% mean())

# Create results table
max_delay <- 0
err_df <- N_a_df %>% filter(delay == max_delay) %>% select(date, med_a = med, q5_a = q5, q95_a = q95) %>% 
  full_join(N_b_df %>% filter(delay == max_delay) %>% select(date, med_b = med, q5_b = q5, q95_b = q95)) %>% 
  #full_join(N_b_c_df %>% filter(delay == max_delay) %>% select(date, med_b_c,q5_b_c,q95_b_c)) %>% 
  #full_join(N_c_df %>% filter(delay == max_delay) %>% select(date, med_c,q5_c,q95_c)) %>% 
  full_join(N_d_df %>% filter(delay == max_delay) %>% select(date, med_d = med, q5_d = q5, q95_d = q95)) %>% 
  #left_join(N_d_c_df %>% filter(delay == max_delay) %>% select(date, med_d_c,q5_d_c,q95_d_c)) %>% 
  #full_join(N_e_df %>% filter(delay == max_delay) %>% select(date, med_e,q5_e,q95_e)) %>% 
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=sum(n))) %>% 
  mutate(err_a_7 = apply(Nres_mod_d$rmse, 1, mean),
         err_b_7 = apply(Nres_mod_b$rmse, 1, mean),
         #err_b_c_7 = apply(err_b_c_7, 1, mean),
         #err_c_7 = apply(err_c_7, 1, mean),
         err_d_7 =  apply(Nres_mod_a$rmse, 1, mean),
         #err_d_c_7 = apply(err_d_c_7, 1, mean),
         #err_e_7 = apply(err_e_7, 1, mean),
         log_a_7 =  apply(Nres_mod_a$logs, 1, mean),
         log_b_7 = apply(Nres_mod_b$logs, 1, mean),
         #log_b_c_7 = apply(log_b_c_7, 1, mean),
         #log_c_7 = apply(log_c_7, 1, mean),
         log_d_7 = apply(Nres_mod_d$logs, 1, mean),
         #log_d_c_7 = apply(log_d_c_7, 1, mean),
         #log_e_7 = apply(log_e_7, 1, mean),
         crps_a_7 = apply(Nres_mod_a$crps, 1, mean),
         crps_b_7 = apply(Nres_mod_b$crps, 1, mean),
         #crps_b_c = crps_b_c,
         #crps_b_c_7 = apply(crps_b_c_7, 1, mean),
         #crps_c_7 = apply(crps_c_7, 1, mean),
         crps_d_7 = apply(Nres_mod_d$crps, 1, mean)#,
         #crps_d_c_7 = apply(crps_d_c_7, 1, mean),
         #crps_e_7 = apply(crps_e_7, 1, mean)
    )

write_csv(err_df, "./results/results_20221027.csv")


# Decreasing score
mod_a_all <- calculate_scores(N_mod_a, 36) 
mod_b_all <- calculate_scores(N_mod_b, 36)
mod_d_all <- calculate_scores(N_mod_d, 36) 

mod_a_all$logs[abs(mod_a_all$logs)>10000] = 0.4 
mod_b_all$logs[abs(mod_b_all$logs)>10000] = 0.6 
mod_d_all$logs[abs(mod_d_all$logs)>10000] = 0.3
vec <- mod_a_all$log %>% as.vector()
vec[vec>0]
err_by_delay_df <- tibble(
  delay = 0:35,
  crps_a = mod_a_all$crps %>% apply(2,mean),
  crps_b = mod_b_all$crps %>% apply(2,mean),
  crps_d = mod_d_all$crps %>% apply(2,mean),
  logs_a = mod_a_all$logs %>% apply(2,mean),
  logs_b = mod_b_all$logs %>% apply(2,mean),
  logs_d = mod_d_all$logs %>% apply(2,mean),
  rmse_a = mod_a_all$rmse %>% apply(2,mean),
  rmse_b = mod_b_all$rmse %>% apply(2,mean),
  rmse_d = mod_d_all$rmse %>% apply(2,mean)
)

write_csv(err_by_delay_df, "./results/error_by_delay_20221028.csv")

# Beta_0 for mod l
files_mod_b <- list_files("beta_0", "mod_b")[1:117]

# Read files
beta_0_mod_b <- lapply(paste0("../results/beta_0/", files_mod_b), read_csv)
beta_0_df <- med_and_quantiles(beta_0_mod_b)
max_delay <- 0
beta_0_df <- beta_0_df %>% filter(delay == max_delay)
write_csv(beta_0_df, "../results/results_beta_0.csv")


# Beta_1 for mod l and rl
files_mod_b <- list_files("beta_1", "mod_b")[1:117]
files_mod_d <- list_files("beta_1", "mod_d")[1:117]

# Read files
beta_1_mod_b <- lapply(paste0("../results/beta_1/", files_mod_b), read_csv)
beta_1_mod_d <- lapply(paste0("../results/beta_1/", files_mod_d), read_csv)

# Create results beta table
beta_1_df <- med_and_quantiles(beta_1_mod_b)
write_csv(beta_1_df %>% filter(delay == max_delay), 
          "../results/results_beta_1_mod_b.csv")

beta_1_df_d <- N_d_df %>% filter(delay == max_delay) 
write_csv(beta_1_df_d, "../results/results_beta_1_mod_d.csv")

