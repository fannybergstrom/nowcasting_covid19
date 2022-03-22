N <- read_csv("~/Documents/GitHub/nowcasting_covid19/results/N/N_mod_a_cp_2021-01-19.csv")
now <- ymd(rep_dates[66])
start <- now - 7 * 8 + 1
post_N = tibble(date = seq(start, now, "1 day"),
                  med = apply(N, 2, median),
                  q5 = apply(N, 2, function(x) quantile(x, .025)),
                  q95 = apply(N, 2, function(x) quantile(x, .975)),
                  type= "mod_f") %>% 
    mutate(now = now, delay = 55:0, weekday = wday(now, label = F))
  
post_N %>% 
  left_join(dat %>% group_by(date=death_date) %>% summarise(truth=n())) %>%
  left_join(dat %>% filter(rep_date<=now) %>% group_by(date=death_date) %>% 
              summarise(reported=n())) %>% #filter(date >"2021-01-05") %>% 
  ggplot() + geom_col(aes(date, reported)) +
  geom_line(aes(date, med)) +
  geom_ribbon(aes(date, ymin=q5, ymax=q95), alpha=.2) +
  geom_line(aes(date, truth), col="green", lty=2)

N <- read_csv("~/Documents/GitHub/nowcasting_covid19/results/N/N_mod_f_ph_2021-01-19.csv")
now <- ymd(rep_dates[66])
start <- now - 7 * 8 + 1
post_N = tibble(date = seq(start, now, "1 day"),
                med = apply(N, 2, median),
                q5 = apply(N, 2, function(x) quantile(x, .025)),
                q95 = apply(N, 2, function(x) quantile(x, .975)),
                type= "mod_f") %>% 
  mutate(now = now, delay = 55:0, weekday = wday(now, label = F))

post_N %>% 
  left_join(dat %>% group_by(date=death_date) %>% summarise(truth=n())) %>%
  left_join(dat %>% filter(rep_date<=now) %>% group_by(date=death_date) %>% 
              summarise(reported=n())) %>% #filter(date >"2021-01-15") %>% 
  ggplot() + geom_col(aes(date, reported)) +
  geom_line(aes(date, med)) +
  geom_ribbon(aes(date, ymin=q5, ymax=q95), alpha=.2) +
  geom_line(aes(date, truth), col="green", lty=2)
