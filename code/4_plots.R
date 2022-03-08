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

# Plot theme and color scheme
theme_set(theme_bw())
wes_cols <- wes_palette("Darjeeling1", 5)
colors <- c("Lead signal (ICU)" = wes_cols[4], "Random walk" = wes_cols[5], "RW + Lead signal (ICU)" = wes_cols[2] , "True number" = wes_cols[1])


#Figure 1, observed and unreported.

now <- 
obs_plot <- dat_mod %>% group_by(date=death_date) %>%
  summarise(n_obs=n()) %>%
  right_join(tibble(date=seq(start, now, by="1 day"))) %>%
  left_join(post_N_1) %>%
  left_join(post_N_a) %>%
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date > (now-35)) %>%
  ggplot() + 
  geom_col(aes(date, n_true_retro, fill = "Occurred but not yet reported")) +
  geom_col(aes(date, n_obs, fill = "Reported")) +
  ylab("Number Fatalities") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 week", date_labels = "%y-%m-%d")+
  theme(legend.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.21, 0.8)) +
  scale_fill_manual(values = c("Occurred but not yet reported" = "gray", "Reported"="grey30"))


obs_plot

ggsave(paste0("../plots/obs_", now, ".png"), obs_plot, height = 4, width = 6)


## Plot for a single reporting day
now <- rep_dates[1] %>% as.Date()
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
  geom_line(aes(y = err_b, color = "Lead signal (ICU)"), linetype = 2) +
  geom_line(aes(y = err_c, color = "Lead signal (ICU+cases) cp"), linetype = 4) +
  #   geom_line(aes(y = err_d, color = "RW + Lead signal (ICU)"), linetype = 3) +
  ylab("RMSE") +
  xlab("Date") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = c(0.9, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())#+
#scale_color_manual(values = c("Random Walk" = wes_cols[4], "Lead signal (ICU)" = wes_cols[2], 
#  "RW + Lead signal (ICU)" = wes_cols[5]))

ggsave(paste0("../plots/rmse.png"), rmse_plot, width = 6,
       height = 4)



log_plot <-err_df %>% filter(date != "2021-02-02") %>% 
  ggplot(aes(x = as.Date(date))) +
  geom_line(aes(y = log_a, color = "RW")) +
  geom_line(aes(y = log_b, color = "Lead signal (ICU)")) +
  geom_line(aes(y = log_c, color = "Lead signal (ICU + cases)"),linetype = 2) +
  # geom_line(aes(y = Mod_d, color = "RW + lead signal"),linetype = 3) +
  ylab("Log Score") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = c(0.9, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())#+
#  scale_color_manual(values = c("Baseline" = wes_cols[4], 
#       "Including ICU" = wes_cols[5]))

ggsave(paste0("../plots/log_score.png"), log_plot, width = 6,
       height = 4)





crps_plot <-err_df %>% filter(date != "2021-02-02") %>% 
  ggplot(aes(x = as.Date(date))) +
  geom_line(aes(y = crps_a, color = "RW")) +
  geom_line(aes(y = crps_b, color = "Lead signal (ICU)")) +
  geom_line(aes(y = crps_c, color = "Lead signal (ICU + cases)"),linetype = 2) +
  #geom_line(aes(y = crps_d, color = "RW + lead signal"),linetype = 3) +
  ylab("CRPS") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = c(0.9, 0.97),
        legend.justification = c("right", "top"),
        legend.title = element_blank())#+
#  scale_color_manual(values = c("Baseline" = wes_cols[4], 
#       "Including ICU" = wes_cols[5]))
ggsave(paste0("../plots/crps.png"), crps_plot, width = 6,
       height = 4)

log_score <- err_df %>%  
  summarize(mean_a = mean(log_a), mean_b = mean(log_b), mean_c = mean(log_c), #mean_d = mean(log_d)
  )

log_score

crps_score <- err_df %>%  #filter(date < "2021-02-02") %>% 
  summarize(mean_a = mean(crps_a), mean_b = mean(crps_b), mean_c = mean(crps_c)#, mean_d = mean(crps_d)
  )