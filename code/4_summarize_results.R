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
  summarise(n_true_retro=n())

list_files <- function(parameter, mod){
  files <- list.files(path = paste0("./results/", parameter))
  files[str_detect(files, mod)]
}

# List files
n <- 140
files_mod_a <- list_files("N", "mod_a_ph")[1:n]
files_mod_b <- list_files("N", "mod_b_cp")[1:n]
files_mod_b_c <- list_files("N", "mod_b_cases")[1:n]
files_mod_c <- list_files("N", "mod_c_ph")[1:n]
files_mod_d <- list_files("N", "mod_d_new2")[1:n]
files_mod_d_c <- list_files("N", "mod_d_new_cases")[1:n]
files_mod_e <- list_files("N", "mod_e_ph")[1:n]

files_mod_a <- files_mod_a[21:137]
files_mod_b <- files_mod_b[21:137]
files_mod_d <- files_mod_d[21:137]

# Read files
N_mod_a <- lapply(paste0("./results/N/", files_mod_a), read_csv)
N_mod_b <- lapply(paste0("./results/N/", files_mod_b), read_csv)
N_mod_b_c <- lapply(paste0("./results/N/", files_mod_b_c), read_csv)
N_mod_c <- lapply(paste0("./results/N/", files_mod_c), read_csv)
N_mod_d <- lapply(paste0("./results/N/", files_mod_d), read_csv)
N_mod_d_c <- lapply(paste0("./results/N/", files_mod_d_c), read_csv)
N_mod_e <- lapply(paste0("./results/N/", files_mod_e), read_csv)

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


N_d_c_df <- c()
for(i in 1:length(N_mod_d_c)){
  N <-N_mod_d_c[[i]]
  now <- ymd(rep_dates[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_d_c = apply(N, 2, median),
                  q5_d_c = apply(N, 2, function(x) quantile(x, .025)),
                  q95_d_c = apply(N, 2, function(x) quantile(x, .975))) %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_d_c_df <- bind_rows(N_d_c_df, post_N)
}

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


##### Scores
log_a <- c()
crps_a <- c()
for(i in 1:(length(N_mod_a))){
  v <- N_mod_a[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_a[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_a[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

err_a_7 <- matrix(NA, length(N_mod_a), 7)
log_a_7 <- matrix(NA, length(N_mod_a), 7)
crps_a_7 <- matrix(NA, length(N_mod_a), 7)
for(i in 1:length(N_mod_a)){
  for(j in 1:7){
    v <- N_mod_a[[i]][,56+1-j] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i])-j+1) %>% select(n_true_retro) %>% unlist()
    err_a_7[i,j] <- abs(median(v)-truth)
    log_a_7[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_a_7[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

log_b <- c()
crps_b <- c()
for(i in 1:(length(N_mod_b))){
  v <- N_mod_b[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_b[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_b[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

err_b_7 <- log_b_7 <- crps_b_7 <- matrix(NA, length(N_mod_b), 7)
for(i in 1:length(N_mod_b)){
  for(j in 1:7){
    err_b_7[i,j] <- abs(median(v)-truth)
    v <- N_mod_b[[i]][,56+1-j] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i])-j+1) %>% select(n_true_retro) %>% unlist()
    log_b_7[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_b_7[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

log_b_c <- crps_b_c <- c()
for(i in 1:(length(N_mod_b_c))){
  v <- N_mod_b_c[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_b_c[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_b_c[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

err_b_c_7 <- log_b_c_7 <- crps_b_c_7 <- matrix(NA, length(N_mod_b), 7)
for(i in 1:length(N_mod_a)){
  for(j in 1:7){
    v <- N_mod_b_c[[i]][,56+1-j] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i])-j+1) %>% select(n_true_retro) %>% unlist()
    err_b_c_7[i,j] <- abs(median(v)-truth)
    log_b_c_7[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_b_c_7[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}
log_c <- crps_c <- c()
for(i in 1:length(N_mod_c)){
  v <- N_mod_c[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_c[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_c[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

err_c_7 <- log_c_7 <- crps_c_7 <- matrix(NA, length(N_mod_c), 7)
for(i in 1:length(N_mod_c)){
  for(j in 1:7){
    v <- N_mod_c[[i]][,(56+1-j)] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i])-j+1) %>% select(n_true_retro) %>% unlist()
    err_c_7[i,j] <- abs(median(v)-truth)
    log_c_7[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_c_7[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

log_d <- crps_d <- c()
for(i in 1:length(N_mod_d)){
  v <- N_mod_d[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_d[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_d[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

err_d_7 <- log_d_7 <- crps_d_7 <- matrix(NA, length(N_mod_d), 7)
for(i in 1:length(N_mod_d)){
  for(j in 1:7){
    v <- N_mod_d[[i]][,56+1-j] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i])-j+1) %>% select(n_true_retro) %>% unlist()
    err_d_7[i,j] <- abs(median(v)-truth)
    log_d_7[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_d_7[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

log_d_c <- crps_d_c <- c()
for(i in 1:length(N_mod_d_c)){
  v <- N_mod_d_c[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_d_c[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_d_c[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

err_d_c_7 <- log_d_c_7 <- crps_d_c_7 <- matrix(NA, length(N_mod_d_c), 7)
for(i in 1:length(N_mod_d_c)){
  for(j in 1:7){
    v <- N_mod_d_c[[i]][,56+1-j] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i])-j+1) %>% select(n_true_retro) %>% unlist()
    err_d_c_7[i,j] <- abs(median(v)-truth)
    log_d_c_7[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_d_c_7[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

log_e <- crps_e <- c()
for(i in 1:length(N_mod_e)){
  v <- N_mod_e[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_e[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_e[i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

err_e_7 <- log_e_7 <- crps_e_7 <- matrix(NA, length(N_mod_e), 7)
for(i in 1:length(N_mod_e)){
  for(j in 1:7){
    v <- N_mod_e[[i]][,(56+1-j)] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i])-j+1) %>% select(n_true_retro) %>% unlist()
    err_e_7[i,j] <- abs(median(v)-truth)
    log_e_7[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_e_7[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

max_delay <- 0

err_df <- N_a_df %>% filter(delay == max_delay) %>% select(date, med_a,q5_a,q95_a) %>% 
  full_join(N_b_df %>% filter(delay == max_delay) %>% select(date, med_b,q5_b,q95_b)) %>% 
  full_join(N_b_c_df %>% filter(delay == max_delay) %>% select(date, med_b_c,q5_b_c,q95_b_c)) %>% 
  full_join(N_c_df %>% filter(delay == max_delay) %>% select(date, med_c,q5_c,q95_c)) %>% 
  full_join(N_d_df %>% filter(delay == max_delay) %>% select(date, med_d,q5_d,q95_d)) %>% 
  left_join(N_d_c_df %>% filter(delay == max_delay) %>% select(date, med_d_c,q5_d_c,q95_d_c)) %>% 
  full_join(N_e_df %>% filter(delay == max_delay) %>% select(date, med_e,q5_e,q95_e)) %>% 
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>% 
  mutate(err_a = abs(med_a-n_true_retro), 
         err_a_7 = apply(err_a_7, 1, mean),
         err_b = abs(med_b-n_true_retro), 
         err_b_7 = apply(err_b_7, 1, mean),
         err_b_c = abs(med_b_c-n_true_retro), 
         err_b_c_7 = apply(err_b_c_7, 1, mean),
         err_c = abs(med_c-n_true_retro),
         err_c_7 = apply(err_c_7, 1, mean),
         err_d = abs(med_d-n_true_retro),
         err_d_7 = apply(err_d_7, 1, mean),
         err_d_c = abs(med_d_c-n_true_retro),
         err_d_c_7 = apply(err_d_c_7, 1, mean),
         err_e = abs(med_e-n_true_retro),
         err_e_7 = apply(err_e_7, 1, mean),
         rel_a = med_a/n_true_retro,
         rel_b = med_b/n_true_retro,
         rel_c = med_c/n_true_retro,
         rel_d = med_d/n_true_retro,
         log_a = log_a,
         log_a_7 = apply(log_a_7, 1, mean),
         log_b = log_b,
         log_b_7 = apply(log_b_7, 1, mean),
         log_b_c = log_b_c,
         log_b_c_7 = apply(log_b_c_7, 1, mean),
         log_c = log_c,
         log_c_7 = apply(log_c_7, 1, mean),
         log_d = log_d,
         log_d_7 = apply(log_d_7, 1, mean),
         log_d_c = log_d_c,
         log_d_c_7 = apply(log_d_c_7, 1, mean),
         log_e = log_e,
         log_e_7 = apply(log_e_7, 1, mean),
         crps_a = crps_a,
         crps_a_7 = apply(crps_a_7, 1, mean),
         crps_b = crps_b,
         crps_b_7 = apply(crps_b_7, 1, mean),
         crps_b_c = crps_b_c,
         crps_b_c_7 = apply(crps_b_c_7, 1, mean),
         crps_c = crps_c,
         crps_c_7 = apply(crps_c_7, 1, mean),
         crps_d = crps_d,
         crps_d_7 = apply(crps_d_7, 1, mean),
         crps_d_c = crps_d_c,
         crps_d_c_7 = apply(crps_d_c_7, 1, mean),
         crps_e = crps_e,
         crps_e_7 = apply(crps_e_7, 1, mean)
    )

write_csv(err_df, "./results/results_20220421.csv")

rep_dates<-rep_dates[21:137]
rep_dates <- as.Date(rep_dates)
rep_d <- seq(as.Date("2020-04-20"), as.Date("2021-05-21"), by = "days") %>% as.data.frame() 
retro_truth <- rep_d %>% left_join(retro_truth) %>% mutate(n_true_retro = replace_na(n_true_retro, 0))

# Decreasing score
log_a <- crps_a <- matrix(NA, 56, length(N_mod_a))
for(j in 1:56){
 for(i in 1:(length(N_mod_a))){
  v <- N_mod_a[[i]][,56-j+1] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]-j+1) %>% select(n_true_retro) %>% unlist()
  log_a[j,i]  <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  crps_a[j,i] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

apply(log_d_all, 2, median)
apply(crps_d_all[1:2,], 2, median)

names(rep_d) <- "date"
retro_truth <- retro_truth %>% right_join(rep_d)
retro_truth[is.na.POSIXlt(retro_truth$n_true_retro),]

err_a_all <- log_a_all<- crps_a_all <- matrix(NA, length(N_mod_a), 56)
for(i in 1:length(N_mod_d)){
  for(j in 1:56){
    v <- N_mod_a[[i]][,(56+1-j)] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i]-j+1)) %>% select(n_true_retro) %>% unlist()
    log_a_all[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_a_all[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

err_b_all <- log_d_all<- crps_d_all <- matrix(NA, length(N_mod_d), 56)
for(i in 1:length(N_mod_d)){
  for(j in 1:56){
    v <- N_mod_d[[i]][,(56+1-j)] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i]-j+1)) %>% select(n_true_retro) %>% unlist()
    log_d_all[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_d_all[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

err_d_all <- log_d_all<- crps_d_all <- matrix(NA, length(N_mod_d), 56)
for(i in 1:length(N_mod_d)){
  for(j in 1:56){
    v <- N_mod_d[[i]][,(56+1-j)] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i]-j+1)) %>% select(n_true_retro) %>% unlist()
    log_d_all[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_d_all[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

err_b_117 <- log_b_117 <- crps_b_117 <- matrix(NA, length(N_mod_b), 36)
for(i in 1:length(N_mod_a)){
  for(j in 1:36){
    v <- N_mod_b[[i]][,(56+1-j)] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i]-j+1)) %>% select(n_true_retro) %>% unlist()
    err_b_117[i,j] <- abs(median(v)-truth)
    log_b_117[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_b_117[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

err_d_117 <- log_d_117 <- crps_d_117 <- matrix(NA, length(N_mod_d), 36)
for(i in 1:length(N_mod_d)){
  for(j in 1:36){
    v <- N_mod_d[[i]][,56+1-j] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i])-j+1) %>% select(n_true_retro) %>% unlist()
    err_d_117[i,j] <- abs(median(v)-truth)
    log_d_117[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_d_117[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

err_a_117 <- log_a_117 <- crps_a_117 <- matrix(NA, length(N_mod_a), 36)
for(i in 1:length(N_mod_a)){
  for(j in 1:36){
    v <- N_mod_a[[i]][,(56+1-j)] %>% unlist()
    truth <- retro_truth %>% filter(date == as.Date(rep_dates[i]-j+1)) %>% select(n_true_retro) %>% unlist()
    err_a_117[i,j] <- abs(median(v)-truth)
    log_a_117[i,j] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
    crps_a_117[i,j] <- crps(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
  }
}

bind_cols(lapply(c(err_a_117, err_b_117, err_d_117), mean, 2))

# PI
#### PI
pi_a_95 <- c()
rep <- rep_dates[21:137]
l <- 117 # nrow(res_df)
for(i in 1:l){
  v1 <- N_mod_a[[i]][,56] %>% unlist()
  n <- retro_truth %>% filter(date == rep[i]) %>% select(n_true_retro) %>% unlist()
  pi_a_95[i] <-between(n, quantile(v1, .025), quantile(v1, .975))
}
sum(na.omit(pi_a_95))/l

pi_a_90 <- c()
for(i in 1:l){
  v1 <- N_mod_a[[i]][,56] %>% unlist()
  n <- retro_truth %>% filter(date == rep[i]) %>% select(n_true_retro) %>% unlist()
  pi_a_90[i] <-between(n, quantile(v1, .05), quantile(v1, .95))
}
sum(na.omit(pi_a_90))/l

pi_a_75 <- c()
for(i in 1:l){
  v1 <- N_mod_a[[i]][,56] %>% unlist()
  n <- retro_truth %>% filter(date == rep[i]) %>% select(n_true_retro) %>% unlist()
  pi_a_75[i] <-between(n, quantile(v1, .125), quantile(v1, .875))
}
sum(na.omit(pi_a_75))/l


pi_b_95 <- c()
for(i in 1:l){
  v1 <- N_mod_b[[i]][,56] %>% unlist()
  n <- retro_truth %>% filter(date == rep[i]) %>% select(n_true_retro) %>% unlist()
  pi_b_95[i] <-between(n, quantile(v1, .025), quantile(v1, .975))
}
sum(na.omit(pi_b_95))/l

pi_b_90 <- c()
for(i in 1:l){
  v1 <- N_mod_b[[i]][,56] %>% unlist()
  n <- retro_truth %>% filter(date == rep[i]) %>% select(n_true_retro) %>% unlist()
  pi_b_90[i] <-between(n, quantile(v1, .05), quantile(v1, .95))
}
sum(na.omit(pi_b_90))/l

pi_b_75 <- c()
for(i in 1:l){
  v1 <- N_mod_b[[i]][,56] %>% unlist()
  n <- retro_truth %>% filter(date == rep[i]) %>% select(n_true_retro) %>% unlist()
  pi_b_75[i] <-between(n, quantile(v1, .125), quantile(v1, .875))
}
sum(na.omit(pi_b_75))/l

pi_b_75 <- pi_b_90 <- pi_b_95 <- c()
for(i in 1:l){
  v1 <- N_mod_b[[i]][,56] %>% unlist()
  n <- retro_truth %>% filter(date == rep[i]) %>% select(n_true_retro) %>% unlist()
  pi_b_75[i] <-between(n, quantile(v1, .125), quantile(v1, .875))
  pi_b_90[i] <-between(n, quantile(v1, .05), quantile(v1, .95))
  pi_b_95[i] <-between(n, quantile(v1, .025), quantile(v1, .975))
}
sum(na.omit(pi_b_95))/l
sum(na.omit(pi_b_90))/l
sum(na.omit(pi_b_75))/l

pi_d_75 <- pi_d_90 <- pi_d_95 <- c()
for(i in 1:l){
  v1 <- N_mod_d[[i]][,56] %>% unlist()
  n <- retro_truth %>% filter(date == rep[i]) %>% select(n_true_retro) %>% unlist()
  pi_d_75[i] <-between(n, quantile(v1, .125), quantile(v1, .875))
  pi_d_90[i] <-between(n, quantile(v1, .05), quantile(v1, .95))
  pi_d_95[i] <-between(n, quantile(v1, .025), quantile(v1, .975))
}
sum(na.omit(pi_d_95))/l
sum(na.omit(pi_d_90))/l
sum(na.omit(pi_d_75))/l



# Beta_0 for mod l
files_mod_b <- list_files("beta_0", "mod_b")[1:150]
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
                  q95_b = apply(N, 2, function(x) quantile(x, .975))) %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_b_df <- bind_rows(N_b_df, post_N)
}

max_delay <- 0
beta_0_df <- N_b_df %>% filter(delay == max_delay)
write_csv(beta_0_df, "../results/results_beta_0.csv")


# Beta_1 for mod l and rl
files_mod_b <- list_files("beta_1", "mod_b")[1:144]
files_mod_d <- list_files("beta_1", "mod_d")[1:144]

# Read files
N_mod_b <- lapply(paste0("../results/beta_1/", files_mod_b), read_csv)
N_mod_d <- lapply(paste0("../results/beta_1/", files_mod_d), read_csv)

# Create results table
N_b_df <- c()
for(i in 1:length(N_mod_b)){
  N <-N_mod_b[[i]]
  now <- ymd(rep_dates[i])
  start <- now - 7 * 8 + 1
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_b = apply(N, 2, median),
                  q5_b = apply(N, 2, function(x) quantile(x, .025)),
                  q95_b = apply(N, 2, function(x) quantile(x, .975))) %>% 
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
    mutate(now = now, delay = 55:0)
  N_d_df <- bind_rows(N_d_df, post_N)
}

beta_1_df <- N_b_df %>% filter(delay == max_delay) 
write_csv(beta_1_df, "../results/results_beta_1_mod_b.csv")

beta_1_df_d <- N_d_df %>% filter(delay == max_delay) 
write_csv(beta_1_df_d, "../results/results_beta_1_mod_d.csv")

