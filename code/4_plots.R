#### Nowcast visualizations
library(ggpubr)


## Plot for a single reporting day


now <- rep_dates[1] %>% as.Date()
#now <- ymd("2021-01-08") #ymd("2021-05-20")
start <- now- 7*8+1
model <- "mod_d"
D_max <- 35

# Import data
dat <- read_csv("../data/covid_deaths.csv")

dat %>% filter(rep_date <= now) %>% 
  group_by(date=death_date) %>% summarise(n_death=n()) %>% 
  filter(date>=ymd(start)) %>% right_join(tibble(date=seq(start, now, "1 day"))) %>%
  mutate(n_death=replace_na(n_death, 0)) %>%
  left_join(ts %>% select(date, sum_7_i_lag)) %>% ggplot() + geom_line(aes(date, n_death)) + geom_line(aes(date, sum_7_i_lag/7))

fig_a <- read_csv(paste0("../results/summary/", "mod_d", "_", date, ".csv")) %>% 
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

fig_b <- read_csv(paste0("../results/summary/", "mod_b", "_", date, ".csv")) %>% 
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

err_df
ggplot(aes(x = date)) +
  geom_line(aes(y = med_a, color = "Including ICU")) +
  geom_line(aes(y = med_ac, color = "Including Cases")) +
  geom_line(aes(y = n_true_retro, color = "True number"), lty = 2) +
  geom_ribbon(aes(date, ymin=q10_a, ymax=q90_a), fill=wes_cols[4], alpha=.2) +
  geom_ribbon(aes(date, ymin=q10_ac, ymax=q90_ac), fill=wes_cols[2], alpha=.2) +
  ylab("Number Fatalities") +
  xlab("Date")
