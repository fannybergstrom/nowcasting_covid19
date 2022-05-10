# Load packages
library(tidyverse)
library(data.table)
library(lubridate)
library(rstan)
library(surveillance)
library(readxl)
library(zoo)
library(splitstackshape)
library(posterior)
library(bayesplot)
library(scoringRules)
library(wesanderson)
theme_set(theme_bw())
setwd("/media/fabe4028/suhome/Documents/GitHub/nowcasting_covid19/code")
wes_cols <- wes_palette("Darjeeling1", 5)
colors <- c("Lead signal (ICU)" = wes_cols[4], "Random walk" = wes_cols[5], "RW + Lead signal (ICU)" = wes_cols[2] , "True number" = wes_cols[1])

# Import data
dat <- read_csv("../data/covid_deaths.csv")

# Restrict dataset to a specific nowcast date
rep_dates <- dat %>%
  select(rep_date) %>%
  filter(rep_date >= "2020-09-15") %>% 
  distinct() %>%
  t() %>%
  as.vector()

# Plot nowcast results
D_max = 35

retro_truth <- dat %>% group_by(date=death_date) %>%
  summarise(n_true_retro=n())

list_files <- function(parameter, mod){
  files <- list.files(path = paste0("../results/", parameter))
  files[str_detect(files, mod)]
}


param <- "beta_0"

# List files
n <- 115
#files_mod_a <- list_files("N", "mod_a_cp")[1:n]
#files_mod_a <- list_files("N", "mod_a_ph")[1:n]
files_mod_b <- list_files(param, "mod_b_cp")[1:n]
#files_mod_b_c <- list_files("N", "mod_b_cases")[1:n]
#files_mod_c <- list_files("N", "mod_c")[1:n]
files_mod_d <- list_files(param, "mod_d_new2")[1:n]
#files_mod_d_ph <- list_files("N", "mod_d_log")[1:n]
#files_mod_d_new <- list_files("N", "mod_d_new")[1:n]
#files_mod_f <- list_files("N", "mod_d_ph")[1:n]

# Read files
#N_mod_a <- lapply(paste0("../results/N/", files_mod_a), read_csv)
N_mod_b <- lapply(paste0("../results/N/", files_mod_b), read_csv)
#N_mod_b_c <- lapply(paste0("../results/N/", files_mod_b_c), read_csv)
#N_mod_c <- lapply(paste0("../results/N/", files_mod_d_ph), read_csv)
N_mod_d <- lapply(paste0("../results/N/", files_mod_d), read_csv)
#N_mod_d_n <- lapply(paste0("../results/N/", files_mod_d_new), read_csv)
#N_mod_f <- lapply(paste0("../results/N/", files_mod_f), read_csv)

# Create results table
N_a_df <- c()
for(i in 1:length(N_mod_a)){
  N <-N_mod_a[[i]]
  now <- ymd(rep_dates[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_a = apply(N, 2, median),
                  q5_a = apply(N, 2, function(x) quantile(x, .025)),
                  q95_a = apply(N, 2, function(x) quantile(x, .975)),
                  type= "mod_a") %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_a_df <- bind_rows(N_a_df, post_N)
}

N_b_df <- c()
for(i in 1:length(N_mod_b)){
  N <-N_mod_b[[i]]
  now <- ymd(rep_dates[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_b = apply(N, 2, median),
                  q5_b = apply(N, 2, function(x) quantile(x, .025)),
                  q95_b = apply(N, 2, function(x) quantile(x, .975)),
                  type= "mod_b") %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_b_df <- bind_rows(N_b_df, post_N)
}

#N_b_c_df <- c()
#for(i in 1:length(N_mod_b_c)){
#  N <-N_mod_b_c[[i]]
#  now <- ymd(rep_dates[i])
#  start <- now - 7 * 8 + 1
#  
#  post_N = tibble(date = seq(start, now, "1 day"),
#                  med_b_c = apply(N, 2, median),
#                  q5_b_c = apply(N, 2, function(x) quantile(x, .025)),
#                  q95_b_c = apply(N, 2, function(x) quantile(x, .975))) %>% 
#    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
#  
#  N_b_c_df <- bind_rows(N_b_c_df, post_N)
#}


#N_c_df <- c()
#for(i in 1:length(N_mod_c)){
#  N <-N_mod_c[[i]]
#  now <- ymd(rep_dates[i])
#  start <- now - 7 * 8 + 1
#  
#  post_N = tibble(date = seq(start, now, "1 day"),
#                  med_c = apply(N, 2, median),
#                  q5_c = apply(N, 2, function(x) quantile(x, .025)),
#                  q95_c = apply(N, 2, function(x) quantile(x, .975)),
#                  type= "mod_c") %>% 
#    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
#  
#  N_c_df <- bind_rows(N_c_df, post_N)
#}

N_d_df <- c()
for(i in 1:length(N_mod_d)){
  N <-N_mod_d[[i]]
  now <- ymd(rep_dates[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_d = apply(N, 2, median),
                  q5_d = apply(N, 2, function(x) quantile(x, .025)),
                  q95_d = apply(N, 2, function(x) quantile(x, .975))) %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_d_df <- bind_rows(N_d_df, post_N)
}


#N_d_n_df <- c()
#for(i in 1:length(N_mod_d_n)){
#  N <-N_mod_d_n[[i]]
#  now <- ymd(rep_dates[i])
#  start <- now - 7 * 8 + 1
  
#  post_N = tibble(date = seq(start, now, "1 day"),
#                  med_d_n = apply(N, 2, median),
#                  q5_d_n = apply(N, 2, function(x) quantile(x, .025)),
#                  q95_d_n = apply(N, 2, function(x) quantile(x, .975))) %>% 
#    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
#  
#  N_d_n_df <- bind_rows(N_d_n_df, post_N)
#}

#N_e_df <- c()
#for(i in 1:length(N_mod_e)){
#  N <-N_mod_e[[i]]
#  now <- ymd(rep_dates[i])
#  start <- now - 7 * 8 + 1
#  
#  post_N = tibble(date = seq(start, now, "1 day"),
#                  med_e = apply(N, 2, median),
#                  q5_e = apply(N, 2, function(x) quantile(x, .025)),
#                  q95_e = apply(N, 2, function(x) quantile(x, .975))) %>% 
#    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
#  
#  N_e_df <- bind_rows(N_e_df, post_N)
#}

#N_f_df <- c()
#for(i in 1:length(N_mod_f)){
#  N <-N_mod_f[[i]]
#  now <- ymd(rep_dates[i])
#  start <- now - 7 * 8 + 1
#  
#  post_N = tibble(date = seq(start, now, "1 day"),
#                  med_f = apply(N, 2, median),
#                  q5_f = apply(N, 2, function(x) quantile(x, .025)),
 #                 q95_f = apply(N, 2, function(x) quantile(x, .975))) %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_f_df <- bind_rows(N_f_df, post_N)
}

##### Scores
log_a <- c()
crps_a <- c()
for(i in 1:(length(N_mod_a))){
  v <- N_mod_a[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_a[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_a[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

log_b <- c()
crps_b <- c()
for(i in 1:(length(N_mod_b))){
  v <- N_mod_b[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_b[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_b[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

log_b_c <- c()
crps_b_c <- c()
for(i in 1:(length(N_mod_b_c))){
  v <- N_mod_b_c[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_b_c[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_b_c[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}


log_c <- c()
crps_c <- c()
for(i in 1:length(N_mod_c)){
  v <- N_mod_c[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_c[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_c[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

log_d <- c()
crps_d <- c()
for(i in 1:length(N_mod_d)){
  v <- N_mod_d[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_d[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_d[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

log_d_n <- c()
crps_d_n <- c()
for(i in 1:length(N_mod_d)){
  v <- N_mod_d_n[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_d_n[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_d_n[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

log_e <- c()
crps_e <- c()
for(i in 1:length(N_mod_d)){
  v <- N_mod_e[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_e[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_e[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

log_f <- c()
crps_f <- c()
for(i in 1:length(N_mod_f)){
  v <- N_mod_f[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_f[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_f[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

max_delay <- 0

err_df <- N_a_df %>% filter(delay == max_delay) %>% select(date, med_a,q5_a,q95_a) %>% 
  full_join(N_b_df %>% filter(delay == max_delay) %>% select(date, med_b,q5_b,q95_b)) %>% 
  full_join(N_b_c_df %>% filter(delay == max_delay) %>% select(date, med_b_c,q5_b_c,q95_b_c)) %>% 
  full_join(N_c_df %>% filter(delay == max_delay) %>% select(date, med_c,q5_c,q95_c)) %>% 
  full_join(N_d_df %>% filter(delay == max_delay) %>% select(date, med_d,q5_d,q95_d)) %>% 
  full_join(N_d_n_df %>% filter(delay == max_delay) %>% select(date, med_d_n,q5_d_n,q95_d_n)) %>% 
  full_join(N_f_df %>% filter(delay == max_delay) %>% select(date, med_f,q5_f,q95_f)) %>% 
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>% 
  mutate(err_a = abs(med_a-n_true_retro), 
         err_b = abs(med_b-n_true_retro), 
         err_b_c = abs(med_b-n_true_retro), 
         err_c = abs(med_c-n_true_retro),
         err_d = abs(med_d-n_true_retro),
         err_d_n = abs(med_d_n-n_true_retro),
         err_f = abs(med_f-n_true_retro),
         rel_a = med_a/n_true_retro,
         rel_b = med_b/n_true_retro,
         rel_c = med_c/n_true_retro,
         rel_d = med_d/n_true_retro,
         log_a = log_a,
         log_b = log_b,
         log_b_c = log_b_c,
         log_c = log_c,
         log_d = log_d,
         log_d_n = log_d_n,
         log_f = log_f,
         crps_a = crps_a,
         crps_b = crps_b,
         crps_b_c = crps_b_c,
         crps_c = crps_c,
         crps_d = crps_d,
         crps_d_n = crps_d_n,
         crps_f = crps_f
    )

write_csv(err_df, "../results/results_test.csv")




# Beta 0 

# beta 0 and 1 for mod b and d
files_mod_b <- list_files("beta_0", "mod_b_cp")[1:150]
# Read files
N_mod_b <- lapply(paste0("../results/beta_0/", files_mod_b), read_csv)

# Create results table


N_b_df <- c()
for(i in 1:length(N_mod_b)){
  N <-N_mod_b[[i]]
  now <- ymd(rep_dates[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_b = apply(N, 2, median),
                  q5_b = apply(N, 2, function(x) quantile(x, .025)),
                  q95_b = apply(N, 2, function(x) quantile(x, .975)),
                  type= "mod_b") %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_b_df <- bind_rows(N_b_df, post_N)
}

N_b_c_df <- c()
for(i in 1:length(N_mod_b_c)){
  N <-N_mod_b_c[[i]]
  now <- ymd(rep_dates[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_b_c = apply(N, 2, median),
                  q5_b_c = apply(N, 2, function(x) quantile(x, .025)),
                  q95_b_c = apply(N, 2, function(x) quantile(x, .975))) %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_b_c_df <- bind_rows(N_b_c_df, post_N)
}


N_c_df <- c()
for(i in 1:length(N_mod_c)){
  N <-N_mod_c[[i]]
  now <- ymd(rep_dates[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_c = apply(N, 2, median),
                  q5_c = apply(N, 2, function(x) quantile(x, .025)),
                  q95_c = apply(N, 2, function(x) quantile(x, .975)),
                  type= "mod_c") %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_c_df <- bind_rows(N_c_df, post_N)
}

N_d_df <- c()
for(i in 1:length(N_mod_d)){
  N <-N_mod_d[[i]]
  now <- ymd(rep_dates[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_d = apply(N, 2, median),
                  q5_d = apply(N, 2, function(x) quantile(x, .025)),
                  q95_d = apply(N, 2, function(x) quantile(x, .975))) %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_d_df <- bind_rows(N_d_df, post_N)
}


#N_d_n_df <- c()
#for(i in 1:length(N_mod_d_n)){
#  N <-N_mod_d_n[[i]]
#  now <- ymd(rep_dates[i])
#  start <- now - 7 * 8 + 1

#  post_N = tibble(date = seq(start, now, "1 day"),
#               med_d_n = apply(N, 2, median),
#                q5_d_n = apply(N, 2, function(x) quantile(x, .025)),
#                 q95_d_n = apply(N, 2, function(x) quantile(x, .975))) %>% 
#    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))

#  N_d_n_df <- bind_rows(N_d_n_df, post_N)
#}

N_e_df <- c()
for(i in 1:length(N_mod_e)){
  N <-N_mod_e[[i]]
  now <- ymd(rep_dates[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_e = apply(N, 2, median),
                  q5_e = apply(N, 2, function(x) quantile(x, .025)),
                  q95_e = apply(N, 2, function(x) quantile(x, .975))) %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_e_df <- bind_rows(N_e_df, post_N)
}


max_delay <- 0

beta_0_df <- N_b_df %>% filter(delay == max_delay)
write_csv(beta_0_df, "../results/results_beta_0.csv")

# Beta 0 

# beta 0 and 1 for mod b and d
files_mod_b <- list_files("beta_1", "mod_b")[1:144]
#files_mod_b <- list_files("N", "mod_d_ph")[1:n]
#files_mod_b_cases <- list_files("N", "mod_b_cases")[1:n]
#files_mod_c <- list_files("N", "mod_f")[1:n]
files_mod_d <- list_files("beta_1", "mod_d_new2")[1:144]
#files_mod_d <- list_files("N", "mod_f_ph")[1:n]
#files_mod_d_new <- list_files("N", "mod_d_new")[1:n]
#files_mod_e <- list_files("N", "mod_b_ph")[1:n]

# Read files
N_mod_b <- lapply(paste0("../results/beta_1/", files_mod_b), read_csv)
#N_mod_b_c <- lapply(paste0("../results/N/", files_mod_b_cases), read_csv)
#N_mod_c <- lapply(paste0("../results/N/", files_mod_c), read_csv)
N_mod_d <- lapply(paste0("../results/beta_1/", files_mod_d), read_csv)
#N_mod_d_n <- lapply(paste0("../results/N/", files_mod_d_new), read_csv)
#N_mod_e <- lapply(paste0("../results/N/", files_mod_e), read_csv)

# Create results table


N_b_df <- c()
for(i in 1:length(N_mod_b)){
  N <-N_mod_b[[i]]
  now <- ymd(rep_dates[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_b = apply(N, 2, median),
                  q5_b = apply(N, 2, function(x) quantile(x, .025)),
                  q95_b = apply(N, 2, function(x) quantile(x, .975)),
                  type= "mod_b") %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_b_df <- bind_rows(N_b_df, post_N)
}


N_d_df <- c()
for(i in 1:length(N_mod_d)){
  N <-N_mod_d[[i]]
  now <- ymd(rep_dates[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_d = apply(N, 2, median),
                  q5_d = apply(N, 2, function(x) quantile(x, .025)),
                  q95_d = apply(N, 2, function(x) quantile(x, .975))) %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_d_df <- bind_rows(N_d_df, post_N)
}



max_delay <- 0

beta_1_df <- N_b_df %>% filter(delay == max_delay) 
write_csv(beta_1_df, "../results/results_beta_1_mod_b.csv")


beta_1_df_d <- N_d_df %>% filter(delay == max_delay) 
write_csv(beta_1_df_d, "../results/results_beta_1_mod_d.csv")

