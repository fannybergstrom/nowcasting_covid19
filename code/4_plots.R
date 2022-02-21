# Nowcast visualizations



#now <- date %>% as.Date() 
now <- ymd("2021-01-08") #ymd("2021-05-20")
model <- "mod_b"
D_max <- 35

# Import data
dat <- read_csv("../data/covid_deaths.csv")

dat %>% filter(rep_date <= now) %>% 
  group_by(date=death_date) %>% summarise(n_death=n()) %>% 
  filter(date>=ymd(start)) %>% right_join(tibble(date=seq(start, now, "1 day"))) %>%
  mutate(n_death=replace_na(n_death, 0)) %>%
  left_join(ts %>% select(date, sum_7_i_lag)) %>% ggplot() + geom_line(aes(date, n_death)) + geom_line(aes(date, sum_7_i_lag/7))

read_csv(paste0("../results/summary/", model, "_", now, ".csv")) %>% 
  filter(str_detect(variable, "N") ) %>% 
  cbind(tibble(date = seq(now - D_max + 1, now, "1 day"))) %>%
  left_join(dat %>% group_by(date = death_date) %>% summarise(truth = n())) %>%
  left_join(dat %>% filter(rep_date <= now) %>% group_by(date = death_date) %>%
              summarise(reported = n())) %>%
  ggplot() +
  geom_col(aes(date, reported)) +
  geom_line(aes(date, median)) +
  geom_ribbon(aes(date, ymin = q5, ymax = q95), alpha = .2) +
  geom_line(aes(date, truth), col = "green", lty = 2)

