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

wes_cols <- wes_palette("Darjeeling1", 5)
colors <- c("Lead signal (ICU)" = wes_cols[4], "Random walk" = wes_cols[5], "True number" = wes_cols[1])

# Import data
dat <- read_csv("../data/covid_deaths.csv")

# Restrict dataset to a specific nowcast date
now_res <- rep_dates <- dat %>%
  select(rep_date) %>%
  filter(rep_date >= "2020-10-01", rep_date <= "2021-05-31") %>%
  distinct() %>%
  t() %>%
  as.vector()


l <- length(rep_dates)

# Plot nowcast results
D_max = 35

retro_truth <- dat %>% group_by(date=death_date) %>%
  summarise(n_true_retro=n())

list_files <- function(parameter, mod){
  files <- list.files(path = paste0("../results/", parameter))
  files[str_detect(files, mod)]
}


model_spec <- c("mod_a", "mod_b", "mod_c", "mod_d")

files_mod_a <- list_files("N", "mod_a")[1:40]
files_mod_b <- list_files("N", "mod_b")[1:40]
N_mod_a <- lapply(paste0("../results/N/", files_mod_a), read_csv)
N_mod_b <- lapply(paste0("../results/N/", files_mod_b), read_csv)


# Plot over time (estimate at day of reporting)
N_a_df <- c()
for(i in 1:length(N_mod_a)){
  N <-N_mod_a[[i]]
  now <- ymd(now_res[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_a = apply(N, 2, median),
                  q10_a = apply(N, 2, function(x) quantile(x, .05)),
                  q90_a = apply(N, 2, function(x) quantile(x, .95)),
                  type= "mod_a") %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_a_df <- bind_rows(N_a_df, post_N)
}

N_b_df <- c()
for(i in 1:length(N_mod_b)){
  N <-N_mod_b[[i]]
  now <- ymd(now_res[i])
  start <- now - 7 * 8 + 1
  
  post_N = tibble(date = seq(start, now, "1 day"),
                  med_b = apply(N, 2, median),
                  q10_b = apply(N, 2, function(x) quantile(x, .05)),
                  q90_b = apply(N, 2, function(x) quantile(x, .95)),
                  type= "mod_b") %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
  N_b_df <- bind_rows(N_b_df, post_N)
}


relative_error_rep_plot <- function(max_delay = 0){
  tibble(date=seq(as.Date(now_res[1]), as.Date(now_res[length(now_res)]), by="1 day")) %>%
    mutate(wd = wday(date, label = F)) %>% 
    full_join(N_a_df %>% filter(delay == max_delay) %>% select(date, med_a,q10_a,q90_a)) %>% 
    full_join(N_b_df %>% filter(delay == max_delay) %>% select(date, med_b,q10_b,q90_b)) %>% 
    left_join(dat %>% group_by(date=death_date) %>%
                summarise(n_true_retro=n())) %>% 
    na.omit() %>% 
    mutate(rel_a = med_a/n_true_retro) %>% 
    mutate(rel_b = med_b/n_true_retro) %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = rel_a, color = "Random walk")) +
    geom_line(aes(y = rel_b, color = "Lead signal (ICU)")) +
    geom_line(aes(y = 1, color = "True number"), lty = 2) +
    ylab("Relative error") +
    xlab("Date") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    theme(legend.background = element_blank(),
          legend.position = c(0.8, 0.97),
          legend.justification = c("right", "top"),
          legend.title = element_blank())+
    scale_color_manual(values = colors)
}

relative_error_rep_plot()

mse_df()
max_delay <- 0
err_df <- N_1_df %>% filter(delay == max_delay) %>%
  full_join(N_a_df %>% filter(delay == max_delay) %>% select(date, med_a,q10_a,q90_a)) %>% 
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>% 
  na.omit() %>% 
  mutate(err_1 = abs(med-n_true_retro), err_a = abs(med_a-n_true_retro))  

err_df %>% 
  summarize(mean_1 = mean(err_1), mean_a = mean(err_a), mean_ac = mean(err_ac))
mean_err_df <- c()
error_mean <- for(i in 1:35){
  max_delay <- i
  err_df <- N_1_df %>% filter(delay == max_delay) %>%
    full_join(N_a_df %>% filter(delay == max_delay) %>% select(date, med_a,q10_a,q90_a)) %>% 
    full_join(N_ac_df %>% filter(delay == max_delay) %>% select(date, med_ac,q10_ac,q90_ac)) %>% 
    left_join(dat %>% group_by(date=death_date) %>%
                summarise(n_true_retro=n())) %>% 
    na.omit() %>% 
    mutate(err_1 = abs(med-n_true_retro), err_a = abs(med_a-n_true_retro), err_ac = abs(med_ac - n_true_retro))  %>% 
    summarize(mean_1 = mean(err_1), mean_a = mean(err_a), mean_ac = mean(err_ac))
  
  mean_err_df <- mean_err_df %>% bind_rows(err_df)
  
}

mean_err_df %>% mutate(delay = 1:35) %>% 
  select(ICU = mean_a, Cases = mean_ac, Model_1 = mean_1, delay) %>% pivot_longer(-delay) %>% 
  ggplot(aes(x = delay)) + geom_line(aes(y=value, color= name)) +
  scale_color_manual(values = colors) +
  scale_color_manual(values = c("Model_1" = wes_cols[1], "Cases" = wes_cols[2], "ICU" = wes_cols[3]))+
  ylab("RMSE") +
  xlab("Delay")+
  theme(legend.background = element_blank(),
        legend.position = c(0.8, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())+
  scale_color_manual(values = c("Model_1" = wes_cols[1], "Cases" = wes_cols[2], "ICU" = wes_cols[3]))

ggsave(paste0("../plots/mean_absolute_error.png"), last_plot(), width = 7,
       height = 4)

rmse_plot <- N_1_df %>% filter(delay == 1) %>%
  full_join(N_a_df %>% filter(delay == 1) %>% select(date, med_a,q10_a,q90_a)) %>% 
  #full_join(N_ac_df %>% filter(delay == 1) %>% select(date, med_ac,q10_ac,q90_ac)) %>% 
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>% 
  na.omit() %>% 
  mutate(err_1 = abs(med-n_true_retro), err_a = abs(med_a-n_true_retro))  %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = err_a, color = "Including ICU")) +
  #geom_line(aes(y = err_ac, color = "Cases")) +
  geom_line(aes(y = err_1, color = "Baseline"), linetype = 2) +
  ylab("RMSE") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = c(0.9, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())+
scale_color_manual(values = c("Baseline" = wes_cols[4], #"Cases" = wes_cols[2], 
                              "Including ICU" = wes_cols[5]))

ggsave(paste0("../plots/paper_rmse.png"), rmse_plot, width = 6,
       height = 4)



##### Log score
log_a <- c()
for(i in 1:length(N_mod_a)){
  v <- N_mod_a[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_a[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}

log_b <- c()
for(i in 1:length(N_mod_b)){
  v <- N_mod_b[[i]][,56] %>% unlist()
  truth <- retro_truth %>% filter(date == rep_dates[i]) %>% select(n_true_retro) %>% unlist()
  log_b[i] <- logs(y = v, family = "negative-binomial", mu = truth, size = 1) %>% mean()
}


log_plot <- bind_cols(Model_1 = log_a, ICU = log_b) %>% 
  mutate(date= rep_dates[1:length(N_mod_a)]) %>% 
  ggplot(aes(x = as.Date(date))) +
  geom_line(aes(y = ICU, color = "Lead signal (ICU)")) +
  geom_line(aes(y = Model_1, color = "Random walk"),linetype = 2) +
  ylab("Log Score") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = c(0.9, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())+
  scale_color_manual(values = c("Baseline" = wes_cols[4], 
                                "Including ICU" = wes_cols[5]))

ggsave(paste0("../plots/log_score_paper.png"), log_plot, width = 6,
       height = 4)

bind_cols(Model_1 = log_1, ICU = log_a, Cases = log_ac) %>% 
  summarize(mean_1 = mean(Model_1), mean_a = mean(ICU), mean_ac = mean(Cases) )


#### CRPS
log_1 <- c()
for(i in 1:101){
  v1 <- N_mod_1[[i]][,35] %>% unlist()
  log_1[i] <- crps(y = v1, family = "negative-binomial", mu = err_df$n_true_retro[i], size = 1) %>% mean()
}

log_a <- c()
for(i in 1:101){
  v1 <- N_mod_a[[i]][,35] %>% unlist()
  log_a[i] <- crps(y = v1, family = "negative-binomial", mu = err_df$n_true_retro[i], size = 1) %>% mean()
}

log_ac <- c()
for(i in 1:101){
  v1 <- N_mod_a_cases[[i]][,35] %>% unlist()
  log_ac[i] <- crps(y = v1, family = "negative-binomial", mu = err_df$n_true_retro[i], size = 1) %>% mean()
}

log_a2w <- c()
for(i in 1:101){
  v1 <- N_mod_a_2w[[i]][,35] %>% unlist()
  log_a2w[i] <- crps(y = v1, family = "negative-binomial", mu = err_df$n_true_retro[i], size = 1) %>% mean()
}


crps_plot <- bind_cols(Model_1 = log_1, ICU = log_a, ICU2 = log_a2w) %>% 
  mutate(date= rep_dates[1:101]) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = ICU, color = "Including ICU")) +
  #geom_line(aes(y = Cases, color = "Cases")) +
  geom_line(aes(y = Model_1, color = "Baseline"),linetype = 2) +
  ylab("CRPS") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = c(0.9, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())+
  scale_color_manual(values = c("Baseline" = wes_cols[4],# "Cases" = wes_cols[2], 
                                "Including ICU" = wes_cols[5]))

ggsave(paste0("../plots/crps_paper.png"), crps_plot, width = 6,
       height = 4)

bind_cols(Model_1 = log_1, ICU = log_a, Cases = log_ac) %>% 
  summarize(mean_1 = sum(Model_1), mean_a = sum(ICU, d=6) )/101

i = 1
v1 <- N_mod_1[[i]][,35] %>% unlist()
crps(y = v1, family = "negative-binomial", mu = err_df$n_true_retro[i], size = 1) %>% mean()
log_1[i] <- logs(y = v1, family = "negative-binomial", mu = err_df$n_true_retro[i], size = 1) %>% mean()


#### PI
rep_dates[8]
pi_a <- c()
for(i in 1:100){
  v1 <- N_mod_a[[i]][,35] %>% unlist() %>% sort()
  r <- c(v1[50], v1[1950])
  x <- err_df$n_true_retro[i]
  pi_a[i] <- check.in.range(x, r, fatal = FALSE)
}

sum(pi_a)


files_dates <- N_files_mod_a %>% str_sub(-14,-5)

pi_a <- c()
for(i in 1:101){
  v1 <- N_mod_a[[i]][2001:4000,1] %>% unlist() %>% sort()
  r <- c(v1[50], v1[1950])
  e_df <- err_df %>% filter(now == files_dates[i])
  x <- e_df$n_true_retro
  try(
  pi_a[i] <- check.in.range(x, r, fatal = FALSE))
}
sum(na.omit(pi_a))

files_dates <- N_files_mod_1 %>% str_sub(-14,-5)
pi_1 <- c()
for(i in 1:101){
  v1 <- N_mod_1[[i]][2001:4000,1] %>% unlist() %>% sort()
  r <- c(v1[50], v1[1950])
  e_df <- err_df %>% filter(now == files_dates[i])
  x <- e_df$n_true_retro
  try(
    pi_1[i] <- check.in.range(x, r, fatal = FALSE))
}
sum(na.omit(pi_1))

pi_a

err_df %>% mutate(pi_mod1 = check.in.range(n_true_retro, c(q10, q90), fatal = FALSE))
err_df %>% select(date, q10, q90, n_true_retro) %>% mutate(n_true_retro = as.double(n_true_retro)) %>% 
  mutate(pi_mod1 = isTRUE(q10 >= n_true_retro))

