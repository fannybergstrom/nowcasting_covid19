#### Plots and tables
# Load packages
library(tidyverse)
library(data.table)
library(lubridate)
library(surveillance)
library(readxl)
library(wesanderson)
library(ggpubr)

# Import COVID data and Nowcast results
dat <- read_csv("../data/covid_deaths.csv")
res_df <- read_csv("../results/results.csv")

col2 <- wes_palette("FantasticFox1", 5)

# Plot theme and color
theme_set(theme_bw())
wes_cols <- c(wes_palette("Darjeeling1", 5), wes_palette("GrandBudapest2"))
colors <-  c("RW" = wes_cols[4], "LS ICU" = wes_cols[5], "LS Cases+ICU" = wes_cols[2], "True number" = wes_cols[1])

#Figure 1, observed and unreported.

now <- "2022-02-01"
dat_mod <- dat %>% filter(rep_date <= now)
obs_plot <- dat %>% group_by(date=death_date) %>%
  summarise(n_true_retro=n()) %>%
left_join(dat_mod %>% group_by(date=death_date) %>%
  summarise(n_obs=n())) %>%

  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date > (as.Date(now)-35), date <= now) %>%
  ggplot() + 
  geom_col(aes(date, n_true_retro, fill = "Occurred but not yet reported")) +
  geom_col(aes(date, n_obs, fill = "Reported")) +
  ylab("Number Fatalities") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 week", date_labels = "%y-%m-%d")+
  theme(legend.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.21, 0.885)) +
  scale_fill_manual(values = c("Occurred but not yet reported" = "gray", "Reported"="grey30"))


obs_plot

ggsave(paste0("../plots/obs_", now, ".png"), obs_plot, height = 4, width = 6)


## Plot for a single reporting day
now <- rep_dates[10] %>% as.Date()
date <- now
#now <- ymd("2021-01-08") #ymd("2021-05-20")
start <- now- 7*8+1
model <- "mod_d"
D_max <- 35


dat %>% filter(rep_date <= now) %>% 
  group_by(date=death_date) %>% summarise(n_death=n()) %>% 
  filter(date>=ymd(start)) %>% right_join(tibble(date=seq(start, now, "1 day"))) %>%
  mutate(n_death=replace_na(n_death, 0)) %>%
  left_join(ts %>% select(date, sum_7_i_lag)) %>% ggplot() + geom_line(aes(date, n_death)) + geom_line(aes(date, sum_7_i_lag/7))

fig_a <- read_csv(paste0("../results/summary/", "mod_b_cp", "_", date, ".csv")) %>% 
  filter(str_detect(variable, "N\\[") ) %>%
  #cbind(tibble(date = seq(now-D_max +1, now, "1 day"))) %>%
  cbind(tibble(date = seq(start, now, "1 day"))) %>%
  left_join(dat %>% group_by(date = death_date) %>% summarise(truth = n())) %>%
  left_join(dat %>% filter(rep_date <= now) %>% group_by(date = death_date) %>%
              summarise(reported = n())) %>% 
  ggplot() +
  geom_col(aes(date, reported)) +
  geom_line(aes(date, median)) +
  geom_ribbon(aes(date, ymin = q5, ymax = q95), alpha = .2) +
  geom_line(aes(date, truth), col = "green", lty = 2)

fig_b <- read_csv(paste0("../results/summary/", "mod_a", "_", date, ".csv")) %>% 
  filter(str_detect(variable, "N\\[") ) %>%
  #cbind(tibble(date = seq(now-D_max +1, now, "1 day"))) %>%
  cbind(tibble(date = seq(start, now, "1 day"))) %>%
  left_join(dat %>% group_by(date = death_date) %>% summarise(truth = n())) %>%
  left_join(dat %>% filter(rep_date <= now) %>% group_by(date = death_date) %>%
              summarise(reported = n())) %>% 
  ggplot() +
  geom_col(aes(date, reported)) +
  geom_line(aes(date, median)) +
  geom_ribbon(aes(date, ymin = q5, ymax = q95), alpha = .2) +
  geom_line(aes(date, truth), col = "green", lty = 2)

ggarrange(fig_a, fig_b)

ggarrange(fig_a, fig_c)

relative_error_rep_plot <-  err_df %>% 
  na.omit() %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = rel_a, color = "Random walk")) +
  geom_line(aes(y = rel_b, color = "Lead signal (ICU)")) +
  geom_line(aes(y = rel_c, color = "Lead signal (ICU+cases) cp")) +
  # geom_line(aes(y = rel_d, color = "RW + Lead signal (ICU)")) +
  geom_line(aes(y = 1, color = "True number"), lty = 2) +
  ylab("Relative error") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  theme(legend.background = element_blank(),
        legend.position = c(0.8, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())#+
#scale_color_manual(values = colors)

ggsave(paste0("../plots/rel_plot.png"), relative_error_rep_plot, width = 6,
       height = 4)
max_delay <- 0
err_df %>% 
  summarize(mean_a = mean(err_a), mean_b = mean(err_b), mean_c = mean(err_c)#, mean_d = mean(err_d)
  )


rmse_plot <- err_df %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = err_a, color = "Random walk")) +
  geom_line(aes(y = err_b, color = "LS ICU"), linetype = 2) +
  geom_line(aes(y = err_c, color = "Lead signal ases)"), linetype = 4) +
  #   geom_line(aes(y = err_d, color = "RW + Lead signal (ICU)"), linetype = 3) +
  ylab("RMSE") +
  xlab("Date") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = c(0.9, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())#+
scale_color_manual(values = c("Random Walk" = wes_cols[4], "Lead signal (ICU)" = wes_cols[2], 
#  "RW + Lead signal (ICU)" = wes_cols[5]))
rmse_plot
ggsave(paste0("../plots/rmse.png"), rmse_plot, width = 6,
       height = 4)



log_plot <-err_df %>% filter(date != "2021-02-02",  date < "2021-06-22") %>% 
  ggplot(aes(x = as.Date(date))) +
  geom_line(aes(y = log_a, color = "RW"),linetype = 2) +
  geom_line(aes(y = log_b, color = "LS ICU"),linetype = 1) +
  geom_line(aes(y = log_c, color = "LS Cases+ICU"),linetype = 4) +
  # geom_line(aes(y = Mod_d, color = "RW + lead signal"),linetype = 3) +
  ylab("Log Score") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = c(0.9, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())+
scale_color_manual(values = c("RW" = wes_cols[4], "LS ICU" = wes_cols[5], "LS Cases+ICU" = wes_cols[6]))
log_plot
ggsave(paste0("../plots/log_score.png"), log_plot, width = 6,
       height = 4)





crps_plot <-err_df %>% filter(date != "2021-02-02", date < "2021-06-22") %>% 
  ggplot(aes(x = as.Date(date))) +
  geom_line(aes(y = crps_a, color = "RW"), linetype = 2) +
  geom_line(aes(y = crps_b, color = "LS ICU"), linetype = 1) +
  geom_line(aes(y = crps_c, color = "LS Cases+ICU"),linetype = 4) +
  #geom_line(aes(y = crps_d, color = "RW + lead signal"),linetype = 3) +
  ylab("CRPS") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = c(0.9, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())+
  scale_color_manual(values = c("RW" = wes_cols[4], "LS ICU" = wes_cols[5], "LS Cases+ICU" = wes_cols[6]))
ggsave(paste0("../plots/crps.png"), crps_plot, width = 6,
       height = 4)
crps_plot
log_score <- err_df %>%  
  summarize(mean_a = mean(log_a), mean_b = mean(log_b), mean_c = mean(log_c), #mean_d = mean(log_d)
  )

log_score

crps_score <- err_df %>%  filter(date != "2021-02-02", date < "2021-06-22") %>% 
  summarize(mean_a = mean(crps_a), mean_b = mean(crps_b), mean_c = mean(crps_c)#, mean_d = mean(crps_d)
  )

crps_score
## Res over time


rep_plot <-  err_df %>% filter(date != "2021-02-02", date < "2021-06-22") %>%  
    ggplot(aes(x = date)) +
    geom_line(aes(y = med_a, color = "RW")) +
    geom_line(aes(y = med_b, color = "LS ICU")) +
    geom_line(aes(y = n_true_retro, color = "True number"), lty = 2) +
    geom_ribbon(aes(date, ymin=q10_a, ymax=q90_a), fill=wes_cols[4], alpha=.2) +
    geom_ribbon(aes(date, ymin=q10_b, ymax=q90_b), fill=wes_cols[5], alpha=.2) +
    ylab("Number Fatalities") +
    xlab("Date") +
    scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
    theme(legend.background = element_blank(),
          legend.position = c(0.9, 0.97),
          legend.justification = c("right", "top"),
          legend.title = element_blank())+
    scale_color_manual(values = c("RW" = wes_cols[4], "LS ICU" = wes_cols[5], "True number" = wes_cols[1]))
rep_plot
ggsave(paste0("../plots/res_baseline_icu.png"), rep_plot, width = 7,
       height = 4)


rep_plot2 <-  err_df %>% filter(date != "2021-02-02", date < "2021-06-22") %>%  
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_b, color = "LS ICU")) +
  geom_line(aes(y = med_c, color = "LS Cases+ICU")) +
  geom_line(aes(y = n_true_retro, color = "True number"), lty = 2) +
  geom_ribbon(aes(date, ymin=q10_b, ymax=q90_b), fill=wes_cols[5], alpha=.2) +
  geom_ribbon(aes(date, ymin=q10_c, ymax=q90_c), fill=wes_cols[6], alpha=.2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = c(0.9, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())+
  scale_color_manual(values = c("LS ICU" = wes_cols[5], "LS Cases+ICU" = wes_cols[6], "True number" = wes_cols[1]))
rep_plot2
ggsave(paste0("../plots/res_laed_signals.png"), rep_plot2, width = 7,
       height = 4)


## Single reporting day
n <- 58
now <- ymd(rep_dates[n])
start <- ymd(now - 7*8+1)
dat_mod = dat %>%
  filter(rep_date <= now)

N_a <-N_mod_a[[n]]
N_b <-N_mod_b[[n]]
post_N_a <- tibble(date = seq(now-(55), now, "1 day"),
                   med_a=apply(N_a, 2, median), 
                   q5_a =apply(N_a, 2, function(x) quantile(x, .025)),
                   q95_a = apply(N_a, 2, function(x) quantile(x, .975)))
post_N_b <- tibble(date = seq(now-(55), now, "1 day"),
                med_b=apply(N_b, 2, median), 
                q5_b =apply(N_b, 2, function(x) quantile(x, .025)),
                q95_b = apply(N_b, 2, function(x) quantile(x, .975)))

snap_res_rw_icu <- dat_mod %>% group_by(date=death_date) %>%
  summarise(n_obs=n()) %>%
  right_join(tibble(date=seq(start, now, by="1 day"))) %>%
  left_join(post_N_a) %>%
  left_join(post_N_b) %>%
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date > (now-35)) %>%
  ggplot() + 
  geom_line(aes(date, med_a, color = "RW")) +
  geom_line(aes(date, med_b, col = "LS ICU")) +
  geom_ribbon(aes(date, ymin=q5_a, ymax=q95_a), fill=wes_cols[4], alpha=.2) +
  geom_ribbon(aes(date, ymin=q5_b, ymax=q95_b), fill=wes_cols[5], alpha=.2) +
  geom_col(aes(date, n_obs)) +
  geom_line(aes(date, n_true_retro, color="True number"), lty=2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  theme(legend.background = element_blank(),
        legend.position = c(0.4, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())+
  scale_color_manual(values = c("RW" = wes_cols[4], "LS ICU" = wes_cols[5], "True number" = wes_cols[1]))+
  scale_x_date(date_breaks = "1 week", date_labels = "%y-%m-%d")

ggsave(paste0("../plots/snap_res_rw_icu.png"), snap_res_rw_icu, width = 4,
       height = 3)


## Single reporting day
n <- 58
now <- ymd(rep_dates[n])
start <- ymd(now - 7*8+1)
dat_mod = dat %>%
  filter(rep_date <= now)

N_a <-N_mod_a[[n]]
N_b <-N_mod_b[[n]]
N_c <-N_mod_c[[n]]
post_N_a <- tibble(date = seq(now-(55), now, "1 day"),
                   med_a=apply(N_a, 2, median), 
                   q5_a =apply(N_a, 2, function(x) quantile(x, .025)),
                   q95_a = apply(N_a, 2, function(x) quantile(x, .975)))
post_N_b <- tibble(date = seq(now-(55), now, "1 day"),
                   med_b=apply(N_b, 2, median), 
                   q5_b =apply(N_b, 2, function(x) quantile(x, .025)),
                   q95_b = apply(N_b, 2, function(x) quantile(x, .975)))

post_N_c <- tibble(date = seq(now-(55), now, "1 day"),
                   med_c=apply(N_c, 2, median), 
                   q5_c =apply(N_c, 2, function(x) quantile(x, .025)),
                   q95_c = apply(N_c, 2, function(x) quantile(x, .975)))

snap_res_lead_signal <- dat_mod %>% group_by(date=death_date) %>%
  summarise(n_obs=n()) %>%
  right_join(tibble(date=seq(start, now, by="1 day"))) %>%
  left_join(post_N_b) %>%
  left_join(post_N_c) %>%
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date > (now-35)) %>%
  ggplot() + 
  geom_line(aes(date, med_b, color = "LS ICU")) +
  geom_line(aes(date, med_c, col = "LS Cases+ICU")) +
  geom_ribbon(aes(date, ymin=q5_b, ymax=q95_b), fill=wes_cols[5], alpha=.2) +
  geom_ribbon(aes(date, ymin=q5_c, ymax=q95_c), fill=wes_cols[6], alpha=.2) +
  geom_col(aes(date, n_obs)) +
  geom_line(aes(date, n_true_retro, color="True number"), lty=2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  theme(legend.background = element_blank(),
        legend.position = c(0.4, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())+
  scale_color_manual(values = c( "LS ICU" = wes_cols[5], "LS Cases+ICU" = wes_cols[6], "True number" = wes_cols[1]))+
  scale_x_date(date_breaks = "1 week", date_labels = "%y-%m-%d")

ggsave(paste0("../plots/snap_res_leadsignal.png"), snap_res_lead_signal, width = 4,
       height = 3)




