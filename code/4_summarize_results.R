#### This document is for summarizing the nowcasting results
#### used for plots and tables.

# Load packages and functions
source("./code/2_functions.r")

## Import data
dat <- read_csv("./data/covid_deaths.csv")

## Restrict dataset to a specific nowcast date
rep_dates <- list.files(path = paste0("./data/fohm/")) %>%
  str_extract("\\d+-\\d+-\\d+") %>%
  as.data.frame() %>%
  filter(. >= "2020-09-15") %>%
  distinct() %>%
  t() %>%
  as.vector()

retro_truth <- dat %>%
  group_by(date = death_date) %>%
  summarise(n_true_retro = sum(n))

list_files <- function(parameter, mod) {
  files <- list.files(path = paste0("./results/", parameter))
  files[str_detect(files, mod)]
}

## List files
s <- 21
n <- 116
files_mod_a <- list_files("N", "mod_a_ph")[s:(s + n)]
files_mod_b <- list_files("N", "mod_b_cp")[s:(s + n)]
files_mod_b_c <- list_files("N", "mod_b_cases")[s:(s + n)]
files_mod_c <- list_files("N", "mod_c_ph")[s:(s + n)]
files_mod_d <- list_files("N", "mod_d_new2")[s:(s + n)]
files_mod_d_c <- list_files("N", "mod_d_new_cases")[s:(s + n)]
files_mod_e <- list_files("N", "mod_e_ph")[s:(s + n)]

rep_dates <- files_mod_a %>%
  str_extract("\\d+-\\d+-\\d+") %>%
  as.data.frame() %>%
  filter(. >= "2020-09-15") %>%
  distinct() %>%
  t() %>%
  as.vector()


## Read files
N_mod_a <- lapply(paste0("./results/N/", files_mod_a), read_csv)
N_mod_b <- lapply(paste0("./results/N/", files_mod_b), read_csv)
N_mod_b_c <- lapply(paste0("./results/N/", files_mod_b_c), read_csv)
N_mod_c <- lapply(paste0("./results/N/", files_mod_c), read_csv)
N_mod_d <- lapply(paste0("./results/N/", files_mod_d), read_csv)
N_mod_d_c <- lapply(paste0("./results/N/", files_mod_d_c), read_csv)
N_mod_e <- lapply(paste0("./results/N/", files_mod_e), read_csv)


N_a_df <- med_and_quantiles(N_mod_a)
N_b_df <- med_and_quantiles(N_mod_b)
N_b_c_df <- med_and_quantiles(N_mod_b_c)
N_c_df <- med_and_quantiles(N_mod_c)
N_d_df <- med_and_quantiles(N_mod_d)
N_d_c_df <- med_and_quantiles(N_mod_d_c)
N_e_df <- med_and_quantiles(N_mod_e)


Nres_mod_a <- calculate_scores(N_mod_a)
Nres_mod_b <- calculate_scores(N_mod_b)
Nres_mod_b_c <- calculate_scores(N_mod_b_c)
Nres_mod_c <- calculate_scores(N_mod_c)
Nres_mod_d <- calculate_scores(N_mod_d)
Nres_mod_d_c <- calculate_scores(N_mod_d_c)
Nres_mod_e <- calculate_scores(N_mod_e)

## Table 1
tbl1 <- bind_rows(
  rsme = c(
    Nres_mod_a$rmse %>% mean(),
    Nres_mod_b$rmse %>% mean(),
    Nres_mod_d$rmse %>% mean()
  ),
  logs = c(
    Nres_mod_a$logs %>% mean(),
    Nres_mod_b$logs %>% mean(),
    Nres_mod_d$logs %>% mean()
  ),
  crps = c(
    Nres_mod_a$crps %>% mean(),
    Nres_mod_b$crps %>% mean(),
    Nres_mod_d$crps %>% mean()
  ),
  pi_75 = c(
    Nres_mod_a$pi_75 %>% mean(),
    Nres_mod_b$pi_75 %>% mean(),
    Nres_mod_d$pi_75 %>% mean()
  ),
  pi_90 = c(
    Nres_mod_a$pi_90 %>% mean(),
    Nres_mod_b$pi_90 %>% mean(),
    Nres_mod_d$pi_90 %>% mean()
  ),
  pi95 = c(
    Nres_mod_a$pi_95 %>% mean(),
    Nres_mod_b$pi_95 %>% mean(),
    Nres_mod_d$pi_95 %>% mean()
  )
)

write_csv(tbl1, "./results/summarized_results_and_tables/table1.csv")


## Create results table
max_delay <- 0
err_df <- N_a_df %>%
  filter(delay == max_delay) %>%
  select(date, med_a = med, q5_a = q5, q95_a = q95) %>%
  full_join(N_b_df %>% filter(delay == max_delay) %>% select(date, med_b = med, q5_b = q5, q95_b = q95)) %>%
  full_join(N_b_c_df %>% filter(delay == max_delay) %>% select(date, med_b_c = med, q5_b_c = q5, q95_b_c = q95)) %>%
  full_join(N_c_df %>% filter(delay == max_delay) %>% select(date, med_c = med, q5_c = q5, q95_c = q95)) %>%
  full_join(N_d_df %>% filter(delay == max_delay) %>% select(date, med_d = med, q5_d = q5, q95_d = q95)) %>%
  left_join(N_d_c_df %>% filter(delay == max_delay) %>% select(date, med_d_c = med, q5_d_c = q5, q95_d_c = q95)) %>%
  left_join(N_e_df %>% filter(delay == max_delay) %>% select(date, med_e = med, q5_e = q5, q95_e = q95)) %>%
  left_join(dat %>% group_by(date = death_date) %>%
    summarise(n_true_retro = sum(n))) %>%
  mutate(
    err_a_7 = apply(Nres_mod_a$rmse, 1, mean),
    err_b_7 = apply(Nres_mod_b$rmse, 1, mean),
    err_d_7 = apply(Nres_mod_d$rmse, 1, mean),
    log_a_7 = apply(Nres_mod_a$logs, 1, mean),
    log_b_7 = apply(Nres_mod_b$logs, 1, mean),
    log_d_7 = apply(Nres_mod_d$logs, 1, mean),
    crps_a_7 = apply(Nres_mod_a$crps, 1, mean),
    crps_b_7 = apply(Nres_mod_b$crps, 1, mean),
    crps_d_7 = apply(Nres_mod_d$crps, 1, mean)
  )

write_csv(err_df, "./results/summarized_results_and_tables/results.csv")


## Decreasing score
mod_a_all <- calculate_scores(N_mod_a, 35)
mod_b_all <- calculate_scores(N_mod_b, 35)
mod_d_all <- calculate_scores(N_mod_d, 35)

err_by_delay_df <- tibble(
  delay = 0:35,
  crps_a = mod_a_all$crps %>% apply(2, mean),
  crps_b = mod_b_all$crps %>% apply(2, mean),
  crps_d = mod_d_all$crps %>% apply(2, mean),
  logs_a = mod_a_all$logs %>% apply(2, mean),
  logs_b = mod_b_all$logs %>% apply(2, mean),
  logs_d = mod_d_all$logs %>% apply(2, mean),
  rmse_a = mod_a_all$rmse %>% apply(2, mean),
  rmse_b = mod_b_all$rmse %>% apply(2, mean),
  rmse_d = mod_d_all$rmse %>% apply(2, mean)
)

write_csv(err_by_delay_df, "./results/summarized_results_and_tables/error_by_delay.csv")

## Beta_0 for mod l
files_mod_b <- list_files("beta_0", "mod_b")

# Read files
beta_0_mod_b <- lapply(paste0("../results/beta_0/", files_mod_b), read_csv)
beta_0_df <- med_and_quantiles(beta_0_mod_b)
max_delay <- 0
beta_0_df <- beta_0_df %>% filter(delay == max_delay)
write_csv(beta_0_df, "../results/summarized_results_and_tables/results_beta_0.csv")


## Beta_1 for mod l and rl
files_mod_b <- list_files("beta_1", "mod_b")
files_mod_d <- list_files("beta_1", "mod_d")

# Read files
beta_1_mod_b <- lapply(paste0("../results/beta_1/", files_mod_b), read_csv)
beta_1_mod_d <- lapply(paste0("../results/beta_1/", files_mod_d), read_csv)

# Create results beta table
beta_1_df <- med_and_quantiles(beta_1_mod_b)
write_csv(
  beta_1_df %>% filter(delay == max_delay),
  "../results/summarized_results_and_tables/results_beta_1_mod_b.csv"
)

beta_1_df_d <- N_d_df %>% filter(delay == max_delay)
write_csv(beta_1_df_d, "../results/summarized_results_and_tables/results_beta_1_mod_d.csv")



## Table S2
path_summary <- "./results/summary/"
files_summary <- list.files(path_summary)
files_summary <- files_summary[str_detect(files_summary, "12-30")]
summary_list <- lapply(path_summary %>% paste0(files_summary), read_csv)

# Running times
times <- c()
for (l in 1:length(files_summary)) {
  times[l] <- summary_list[[l]]$run_time[1]
}

path_N <- "./results/N/n_" %>% paste0(files_summary)
path_N <- str_remove(path_N, "placeholder")

N_list <- lapply(path_N, read_csv)

res_tabS2 <- calculate_scores(N_list = N_list, rep_date = rep("2020-12-30", length(N_list)))

# Collecting results
tabS2 <- bind_cols(
  model = files_summary %>% str_remove(".csv"),
  crps_7 = res_tabS2$crps %>% apply(1, mean),
  logs_7 = res_tabS2$logs %>% apply(1, mean),
  rmse_7 = res_tabS2$rmse %>% apply(1, mean),
  PI_75 = res_tabS2$pi_75 %>% apply(1, mean),
  PI_90 = res_tabS2$pi_90 %>% apply(1, mean),
  PI_95 = res_tabS2$pi_95 %>% apply(1, mean),
  running_times = times * 60
) %>% as.data.frame()

tabS2 %>% write_csv("./results/summarized_results_and_tables/tableS2.csv")

### Table S3

# Extract rep dates for the comparison
rep_dates_s3 <- rep_dates[(1:11) * 10]

files_mod_r <- str_c("./results/N/N_mod_a_ph_", rep_dates_s3, ".csv")
files_mod_r2 <- str_c("./results/N/N_mod_r2_", rep_dates_s3, ".csv")

N_list_s3 <- lapply(c(files_mod_r, files_mod_r2), read_csv)
scores_s3 <- calculate_scores(N_list = N_list_s3, rep_date = c(rep_dates_s3, rep_dates_s3))

T_s3 <- bind_cols(
  model  = c(rep("mod_r", 11), rep("mod_r2", 11)),
  crps_7 = scores_s3$crps %>% apply(1, mean),
  logs_7 = scores_s3$logs %>% apply(1, mean),
  rmse_7 = scores_s3$rmse %>% apply(1, mean),
  PI_75  = scores_s3$pi_75 %>% apply(1, mean),
  PI_90  = scores_s3$pi_90 %>% apply(1, mean),
  PI_95  = scores_s3$pi_95 %>% apply(1, mean)
) %>%
  as.data.frame() %>%
  group_by(model) %>%
  summarise(
    crps = mean(crps_7), logs = mean(logs_7), rmse = mean(rmse_7),
    PI_75 = mean(PI_75), PI_90 = mean(PI_90), PI_95 = mean(PI_95)
  )

# Write table
write_csv(T_s3, "./results/summarized_results_and_tables/tableS3.csv")
