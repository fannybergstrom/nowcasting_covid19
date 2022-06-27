#### Plots and tables
# Load packages
library(tidyverse)
library(data.table)
library(lubridate)
library(readxl)
library(wesanderson)
library(ggpubr)
library(ggsci)
library(extrafont)
loadfonts(device = "win")


# Import COVID data and Nowcast results
dat <- read_csv("../data/covid_deaths.csv")
res_df <- read_csv("../results/results_20220421.csv") %>% filter(date >= "2020-10-20", date <="2021-05-21")

dat %>% filter(death_date >= "2020-10-20", death_date <="2021-05-21") %>% #nrow()
  mutate(delay = rep_date -death_date) %>% filter(delay > 35) %>% nrow()

# Plot theme and color
theme_set(theme_bw())
wes_cols <- c(wes_palette("Darjeeling1", 5), wes_palette("GrandBudapest2"),wes_palette("Cavalcanti1"))


#Figure 1, observed and unreported.
now <- "2022-02-01"
start <- "2022-01-01"
dat_mod <- dat %>% filter(rep_date <= now)
obs_plot <- dat %>% group_by(date=death_date) %>%
  summarise(n_true_retro=n()) %>%
  left_join(dat_mod %>% group_by(date=death_date) %>%
              summarise(n_obs=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date >= start, date <= now) %>%
  ggplot() + 
  geom_col(aes(date, n_true_retro, fill = "Occurred but not yet reported")) +
  geom_col(aes(date, n_obs, fill = "Reported")) +
  ylab("Number Fatalities") +
  xlab("Date") +
  scale_x_date(breaks = as.Date(c("2022-01-01", "2022-01-07", "2022-01-13", "2022-01-19", 
                                  "2022-01-26", "2022-02-01")), date_labels = "%y-%m-%d", expand=c(0.02,0.02))+
  theme(legend.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 8, family="TT Arial"),
        legend.box.margin=margin(-10,-10,-10,-10)
        ) +
  scale_fill_manual(values = c("Reported"="grey30","Occurred but not yet reported" = "gray" ))

obs_plot

ggsave(paste0("../plots/obs_", now, ".png"), units="in", dpi = 300, obs_plot, height = 3.5, width = 5.2)
ggsave(paste0("../plots/fig1.png"), units="in", dpi = 300, obs_plot, height = 3.5, width = 5.2)
ggsave(paste0("../plots/fig1.tiff"), units="in", dpi = 300, obs_plot, height = 3.5, width = 5.2, compression = 'lzw')


# Fig 2
FHM_ICU <- read_excel("../data/FoHM/FHM_latest.xlsx", 
                      sheet = "Antal intensivvårdade per dag")

FHM_deaths <- read_excel("../data/FoHM/FHM_latest.xlsx", 
                         sheet = "Antal avlidna per dag", col_types = c("date", "numeric"))

FHM_cases <- read_excel("../data/FoHM/FHM_latest.xlsx", 
                        sheet = "Antal per dag region")

#remove final row
FHM_deaths <- FHM_deaths[-nrow(FHM_deaths),]

# Save dates 
now <- ymd("2021-12-31")
start <- now - 180

#start <- ymd("2020-10-15")
#now <- start+120

# Create time series data frame
ts <- FHM_deaths %>% select(date = Datum_avliden, n_deaths = Antal_avlidna)  %>%
  left_join(FHM_ICU %>% select(date = Datum_vårdstart, n_icu = Antal_intensivvårdade)) %>% 
  left_join(FHM_cases %>% select(date = Statistikdatum, n_cases = Totalt_antal_fall)) %>% 
  # left_join(SOC_hosp %>% 
  #          select(date = `datum`, n_hosp = `Inskrivna i slutenvård - antal`)) %>% 
  mutate(#n_hosp = str_replace(n_hosp, "-", "0") %>% as.numeric,
    n_deaths_lag1 = lag(n_deaths, 1),  
    sum_7_c = rollmean(n_cases, k = 21, fill = NA, align = 'center'),
    sum_7_c_lag = lag(sum_7_c, k = 21, fill = NA),
    sum_7_d = rollmean(n_deaths, k = 21, fill = NA, align = 'center'),
    sum_7_i = rollmean(n_icu, k = 21, fill = NA, align = 'center'),
    sum_7_i_lag = lag(sum_7_i, k = 7, fill = NA),
    #    sum_7_h = rollsum(as.numeric(n_hosp), k = 14, fill = NA, align = 'right'),
    #   sum_7_h_lag = lag(sum_7_h, k = 7, fill = NA),
    diff_sum7_i = as.numeric(diff(as.zoo(sum_7_i), lag= 7, na.pad=T)),
    sum_7_log_i = log(rollsum(n_icu, k = 7, fill = NA, align = 'right')),
    diff_sum7_log_i = as.numeric(diff(as.zoo(sum_7_log_i), lag= 7, na.pad=T)),
    ratio_i = log(lag(sum_7_i/sum_7_i_lag, 1)),
    ratio_c = log(lag(sum_7_c/sum_7_c_lag, 7)),
    #  ratio_h = log(lag(sum_7_h/sum_7_h_lag, 1)),
    diff_c = sum_7_c-sum_7_c_lag)

FHM_ICU %>% select(date = Datum_vårdstart, n_icu = Antal_intensivvårdade) %>% 
  filter(date >= "2020-10-20", date <= "2021-05-21") %>% summarise(n= sum(n_icu))

FHM_cases %>% select(date = Statistikdatum, n_cases = Totalt_antal_fall) %>% 
  filter(date >= "2020-10-20", date <= "2021-05-21") %>% summarise(n= sum(n_cases))

timeseries_plots <- ts %>% 
  filter(date > "2020-10-20", date <= "2021-05-21") %>% 
  mutate(sum_7_c = sum_7_c/max(sum_7_c), sum_7_i = sum_7_i/max(sum_7_i), sum_7_d = sum_7_d/max(sum_7_d)) %>% 
  pivot_longer(c(sum_7_c, sum_7_d, sum_7_i)) %>% 
  ggplot() + 
  geom_segment(aes(x=as.Date("2020-12-27"), xend=as.Date("2020-12-27"),
                   y=-Inf,yend=Inf), color = "black") +
  geom_line(aes(as.Date(date), value, color = name, linetype = name))+
  ylab("3-week average value (scaled)") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 1))+
  annotate("text", x = as.Date("2020-12-14"), y = 0.2, cex = 2.1,
           label = "Vaccination\nstart 20-12-27", color = "black") + 
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 8, family="TT Arial"),
        legend.box.margin=margin(-10,-10,-10,-10))+
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d", 
               limits = c(as.Date("2020-10-20"), as.Date("2021-05-21")), expand=c(0.02,0.02)) +
  scale_color_manual(values = c(sum_7_c = pal[4], sum_7_i = pal[1], sum_7_d = pal[9]),
                     labels=c(sum_7_c = "Reported cases", sum_7_i = "ICU admissions", sum_7_d = "Fatalities"))+
  scale_linetype_manual(values = c(sum_7_c =1, sum_7_i = 4, sum_7_d =2),
                        labels=c(sum_7_c = "Reported cases", sum_7_i = "ICU admissions", sum_7_d = "Fatalities")) 

timeseries_plots

ggsave(paste0("../plots/ts.png"), timeseries_plots, width = 7,  height = 4)
ggsave(paste0("../plots/fig2.png"), units="in", dpi = 300, timeseries_plots, height = 3.1, width = 5.2)
ggsave(paste0("../plots/fig2.tiff"), units="in", dpi = 300, timeseries_plots, height = 3.1, width = 5.2, compression = 'lzw')

# Fig 3 is a Tikz figure

# Fig 4

## Single reporting day
# Restrict dataset to a specific nowcast date
rep_dates <- list.files(path = paste0("../data/FoHM/")) %>% 
  str_extract("\\d+-\\d+-\\d+") %>%  
  as.data.frame %>% distinct() %>% 
  filter(. >= "2020-09-15") %>% 
  t() %>%
  as.vector()

n <- 60
now <- ymd(rep_dates[n])
start <- ymd(now - 7*8+1)
dat_mod = dat %>%
  filter(rep_date <= now)

N_a <- lapply(paste0("../results/N/", "N_mod_a_ph_2020-12-30.csv"), read_csv) %>% as.data.frame()
N_b <- lapply(paste0("../results/N/", "N_mod_b_cp_2020-12-30.csv"), read_csv) %>% as.data.frame()
N_d <- lapply(paste0("../results/N/", "N_mod_d_new2_2020-12-30.csv"), read_csv) %>% as.data.frame()

post_N_a <- tibble(date = seq(now-(55), now, "1 day"),
                   med_a=apply(N_a, 2, median), 
                   q5_a =apply(N_a, 2, function(x) quantile(x, .025)),
                   q95_a = apply(N_a, 2, function(x) quantile(x, .975)))
post_N_b <- tibble(date = seq(now-(55), now, "1 day"),
                   med_b=apply(N_b, 2, median), 
                   q5_b =apply(N_b, 2, function(x) quantile(x, .025)),
                   q95_b = apply(N_b, 2, function(x) quantile(x, .975)))

post_N_d <- tibble(date = seq(now-(55), now, "1 day"),
                   med_d=apply(N_d, 2, median), 
                   q5_d =apply(N_d, 2, function(x) quantile(x, .025)),
                   q95_d = apply(N_d, 2, function(x) quantile(x, .975)))

dates <- c(seq(as.IDate("2020-11-25"), as.IDate("2020-12-30"), 12), as.Date("2020-12-30"))

snap_res_a <- dat_mod %>% group_by(date=death_date) %>%
  summarise(n_obs=n()) %>%
  right_join(tibble(date=seq(start, now, by="1 day"))) %>%
  left_join(post_N_a) %>%
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date > (now-36)) %>%
  pivot_longer(c(med_a, n_true_retro)) %>% 
  ggplot() + 
  geom_line(aes(date, value, color = name, linetype = name )) +
  geom_ribbon(aes(date, ymin=q5_a, ymax=q95_a), fill=wes_cols[4], alpha=.2) +
  geom_col(aes(date, n_obs/2)) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 160))+
  scale_color_manual(values = c(med_a = wes_cols[4], n_true_retro = wes_cols[1]),
                     labels = c("R", "True number"))+
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand=c(0.02,0.02))+
  scale_linetype_manual(values = c(med_a = 1, n_true_retro = 2),
                        labels=c("R","True number")) +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 8, family="TT Arial"),
        legend.box.margin=margin(-20,-15,-15,-15),
        legend.direction="vertical",
        legend.spacing = unit(0.01, "mm"),
        plot.margin = margin(0.1,0.2,0.3,0.1, "cm"))

snap_res_a


snap_res_b <- dat_mod %>% group_by(date=death_date) %>%
  summarise(n_obs=n()) %>%
  right_join(tibble(date=seq(start, now, by="1 day"))) %>%
  left_join(post_N_b) %>%
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>% 
  filter(date > (now-36)) %>%
  pivot_longer(c(med_b, n_true_retro)) %>% 
  ggplot() + 
  geom_line(aes(date, value, col = name, linetype = name )) +
  geom_ribbon(aes(date, ymin=q5_b, ymax=q95_b), fill=wes_cols[6], alpha=.2) +
  geom_col(aes(date, n_obs/2)) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 160))+
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 8, family="TT Arial"),
        legend.box.margin=margin(-20,-15,-15,-15),
        plot.margin = margin(0.1,0.2,0.3,0.1, "cm"),
        legend.direction="vertical",
        legend.spacing = unit(0.01, "mm"))+
  scale_color_manual(values = c(med_b = wes_cols[6], n_true_retro = wes_cols[1]),
                     labels = c("L(ICU)", "True number"))+
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand=c(0.02,0.02))+
  scale_linetype_manual(values = c(med_b = 1, n_true_retro = 2),
                        labels=c("L(ICU)","True number"))

snap_res_b

snap_res_d <- dat_mod %>% group_by(date=death_date) %>%
  summarise(n_obs=n()) %>%
  right_join(tibble(date=seq(start, now, by="1 day"))) %>%
  left_join(post_N_d) %>%
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date > (now-36)) %>%
  pivot_longer(c(med_d, n_true_retro)) %>% 
  ggplot() + 
  geom_line(aes(date, value, col = name, linetype = name )) +
  geom_ribbon(aes(date, ymin=q5_d, ymax=q95_d), fill=wes_cols[5], alpha=.2) +
  geom_col(aes(date, n_obs/2)) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 160))+
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 8, family="TT Arial"),
        legend.box.margin=margin(-20,-15,-15,-15),
        plot.margin = margin(0.1,0.2,0.3,0.1, "cm"),
        legend.direction="vertical",
        legend.spacing = unit(0.01, "mm"))+
  scale_color_manual(values = c(med_d = wes_cols[5], n_true_retro = wes_cols[1]),
                     labels = c("RL(ICU)", "True number"))+
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand=c(0.02,0.02))+
  scale_linetype_manual(values = c(med_d = 1, n_true_retro = 2),
                        labels=c("RL(ICU)","True number")) 

snap_res_d

# Q-plots
p_est_a_1230 <- read_csv("../results/p/p_mod_a_ph_2020-12-30.csv") 
p_est_d_1230 <- read_csv("../results/p/p_mod_d_new2_2020-12-30.csv") 
p_est_b_1230 <- read_csv("../results/p/p_mod_b_cp_2020-12-30.csv") 
p_1230_emp <- dat %>% mutate(delay = as.numeric(rep_date-death_date)) %>% 
  filter(death_date >= "2020-11-01", death_date <= "2020-12-30") %>% 
  select(death_date, delay) %>% 
  group_by(death_date, delay) %>% 
  summarise(n = n()) %>% 
  right_join(tibble(expand.grid(death_date = seq(as.Date("2020-11-25"), as.Date("2020-12-30"), "1 day"),
                                delay = 0:500))) %>%
  mutate(n = replace_na(n, 0)) %>% 
  arrange(death_date, delay) %>% 
  mutate(frac = n/sum(n), cum_frac = cumsum(frac))

#Quantile plot A
## Quantile plot

q50_emp_30 <- p_1230_emp %>% filter(cum_frac >= 0.5) %>% 
  group_by(death_date) %>% 
  filter(delay == min(delay)) %>% 
  select(death_date, q_5_emp = delay)

q05_emp_30 <- p_1230_emp %>% filter(cum_frac >= 0.05) %>% 
  group_by(death_date) %>% 
  filter(delay == min(delay))  %>% 
  select(death_date, q_05_emp = delay)

q95_emp_30 <- p_1230_emp %>% filter(cum_frac >= 0.95) %>% 
  group_by(death_date) %>% 
  filter(delay == min(delay))  %>% 
  select(death_date, q_95_emp = delay)

p_1230_a_est = p_est_a_1230 %>% pivot_longer(starts_with("p")) %>%
  group_by(name) %>% 
  summarise(med = mean(value)) %>%
  mutate(day = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][2])),
         delay = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][3]))-1) %>%
  arrange(day, delay) %>%
  mutate(death_date=ymd("2020-12-30") - (56 - day)) %>%
  select(death_date, delay, est_p=med) %>%
  group_by(death_date) %>%
  mutate(cum_p = cumsum(est_p))

q50_est_30 <- p_1230_a_est %>% filter(cum_p >= 0.5) %>% 
  group_by(death_date) %>% 
  filter(delay == min(delay))  %>% 
  select(death_date, q_5_est = delay)

q05_est_30 <- p_1230_a_est %>% filter(cum_p >= 0.05) %>% 
  group_by(death_date) %>% 
  filter(delay == min(delay))  %>% 
  select(death_date, q_05 = delay)

q95_est_30 <- p_1230_a_est %>% filter(cum_p >= 0.95) %>% 
  group_by(death_date) %>% 
  filter(delay == min(delay))  %>% 
  select(death_date, q_95 = delay)

q_plot = q50_emp_30 %>% 
  left_join(q05_emp_30) %>% 
  left_join(q95_emp_30) %>% 
  left_join(q50_est_30) %>% 
  left_join(q05_est_30) %>% 
  left_join(q95_est_30) %>% 
  pivot_longer(c(q_5_emp:q_95)) %>% 
  mutate(name = factor(name, levels = c("q_05", "q_05_emp","q_5_est", "q_5_emp", "q_95", "q_95_emp")))

q_plot$name %>% levels

q_plot_a_30 <- q_plot %>%
  ggplot() +
  geom_line(aes(x = death_date, y = value, color = name, linetype = name))+
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand=c(0.02,0.02)) +
  ylim(0,35)+
  ylab("Delay (days)") +
  xlab("Date") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 8, family="TT Arial"),
        legend.box.margin=margin(-15,-15,-15,-15),
        legend.spacing = unit(0, "cm"),
        legend.text.align = 0,
        plot.margin = margin(0.1,0.4,0.2,0, "cm"))+
  scale_color_manual(values = c(rep( c(wes_cols[4], wes_cols[1]),3)),
                     #breaks = c("q_05", "q_05_emp", "q_5_emp", "q_5_est","q_95", "q_95_emp"),
                     labels=c(expression(paste(q[0.05]~R," ")), expression(q[0.05]~Emp), expression(paste(q[0.50]~R," ")), 
                              expression(q[0.50]~Emp), expression(paste(q[0.95]~R," ")), expression(q[0.95]~Emp)))+
  scale_linetype_manual(values = c(4, 4, 1, 1, 3, 3),
                        labels=c(expression(paste(q[0.05]~R," ")), expression(q[0.05]~Emp), expression(paste(q[0.50]~R," ")), 
                                 expression(q[0.50]~Emp), expression(paste(q[0.95]~R," ")), expression(q[0.95]~Emp)))

q_plot_a_30


ggsave(paste0("../plots/q_plot_modR.png"), q_plot_a_30, width = 3.5,
       height = 3)


# Quantil plot mod L
p_1230_b_est = p_est_b_1230 %>% pivot_longer(starts_with("p")) %>%
  group_by(name) %>% 
  summarise(med = mean(value)) %>%
  mutate(day = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][2])),
         delay = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][3]))-1) %>%
  arrange(day, delay) %>%
  mutate(death_date=ymd("2020-12-30") - (56 - day)) %>%
  select(death_date, delay, est_p=med) %>%
  group_by(death_date) %>%
  mutate(cum_p = cumsum(est_p))

q50_est_30 <- p_1230_b_est %>% filter(cum_p >= 0.5) %>% 
  group_by(death_date) %>% 
  filter(delay == min(delay))  %>% 
  select(death_date, q_5_est = delay)

q05_est_30 <- p_1230_b_est %>% filter(cum_p >= 0.05) %>% 
  group_by(death_date) %>% 
  filter(delay == min(delay))  %>% 
  select(death_date, q_05 = delay)

q95_est_30 <- p_1230_b_est %>% filter(cum_p >= 0.95) %>% 
  group_by(death_date) %>% 
  filter(delay == min(delay))  %>% 
  select(death_date, q_95 = delay)

q_plot = q50_emp_30 %>% 
  left_join(q05_emp_30) %>% 
  left_join(q95_emp_30) %>% 
  left_join(q50_est_30) %>% 
  left_join(q05_est_30) %>% 
  left_join(q95_est_30) %>% 
  pivot_longer(c(q_5_emp:q_95)) %>% 
  mutate(name = factor(name, levels = c("q_05", "q_05_emp","q_5_est", "q_5_emp", "q_95", "q_95_emp")))

q_plot$name %>% levels

q_plot_b_30 <- q_plot %>%
  ggplot() +
  geom_line(aes(x = death_date, y = value, color = name, linetype = name))+
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand=c(0.02,0.02)) +
  ylim(0,35)+
  ylab("Delay (days)") +
  xlab("Date") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 8, family="TT Arial"),
        legend.box.margin=margin(-15,-15,-15,-15),
        legend.spacing = unit(0, "cm"),
        legend.text.align = 0,
        plot.margin = margin(0.1,0.5,0.2,0, "cm"))+
  scale_color_manual(values = c(rep( c(wes_cols[6], wes_cols[1]),3)),
                     labels=c(expression(paste(q[0.05]~L(ICU))), expression(paste(q[0.05]~Emp, " ")), 
                              expression(paste(q[0.50]~L(ICU))), expression(paste(q[0.50]~Emp, " ")), 
                              expression(paste(q[0.95]~L(ICU))), expression(paste(q[0.95]~Emp, " "))),
                              #breaks = c("q_05", "q_05_emp", "q_5_emp", "q_5_est","q_95", "q_95_emp")
                     )+
  scale_linetype_manual(values = c(4, 4, 1, 1, 3, 3),
                        labels=c(expression(paste(q[0.05]~L(ICU))), expression(paste(q[0.05]~Emp, " ")), 
                                 expression(paste(q[0.50]~L(ICU))), expression(paste(q[0.50]~Emp, " ")), 
                                 expression(paste(q[0.95]~L(ICU))), expression(paste(q[0.95]~Emp, " "))))

q_plot_b_30

ggsave(paste0("../plots/q_plot_modL.png"), q_plot_b_30, width = 3.5,
       height = 3)


# Quantil plot mod RL
p_1230_d_est = p_est_d_1230 %>% pivot_longer(starts_with("p")) %>%
  group_by(name) %>% 
  summarise(med = mean(value)) %>%
  mutate(day = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][2])),
         delay = as.numeric(sapply(name, function(x) strsplit(x, "\\.")[[1]][3]))-1) %>%
  arrange(day, delay) %>%
  mutate(death_date=ymd("2020-12-30") - (56 - day)) %>%
  select(death_date, delay, est_p=med) %>%
  group_by(death_date) %>%
  mutate(cum_p = cumsum(est_p))


q50_est_30 <- p_1230_d_est %>% filter(cum_p >= 0.5) %>% 
  group_by(death_date) %>% 
  filter(delay == min(delay))  %>% 
  select(death_date, q_5_est = delay)

q05_est_30 <- p_1230_d_est %>% filter(cum_p >= 0.05) %>% 
  group_by(death_date) %>% 
  filter(delay == min(delay))  %>% 
  select(death_date, q_05 = delay)

q95_est_30 <- p_1230_d_est %>% filter(cum_p >= 0.95) %>% 
  group_by(death_date) %>% 
  filter(delay == min(delay))  %>% 
  select(death_date, q_95 = delay)

q_plot_d = q50_emp_30 %>% 
  left_join(q05_emp_30) %>% 
  left_join(q95_emp_30) %>% 
  left_join(q50_est_30) %>% 
  left_join(q05_est_30) %>% 
  left_join(q95_est_30) %>% 
  pivot_longer(c(q_5_emp:q_95)) %>% 
  mutate(name = factor(name, levels = c("q_05", "q_05_emp", "q_5_est","q_5_emp","q_95", "q_95_emp")))

q_plot$name %>% levels

q_plot_d_30 <- q_plot_d %>%
  ggplot() +
  geom_line(aes(x = death_date, y = value, color = name, linetype = name)) +
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand=c(0.02,0.02)) +
  ylim(0,35)+
  ylab("Delay (days)") +
  xlab("Date") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 8, family="TT Arial"),
        legend.box.margin=margin(-15,-15,-15,-15),
        legend.spacing = unit(0, "cm"),
        legend.text.align = 0,
        plot.margin = margin(0.1,0.7,0.2,0, "cm"))+
  scale_color_manual(values = c(rep( c(wes_cols[5], wes_cols[1]),3)),
                    # breaks = c("q_05", "q_05_emp", "q_5_emp", "q_5_est","q_95", "q_95_emp"),
                     labels=c(q_05=expression(paste(q[0.05]~RL(ICU))), q_05_emp = expression(paste(q[0.05]~Emp, " ")), 
                              q_5_est =expression(paste(q[0.50]~RL(ICU))), q_5_emp = expression(paste(q[0.50]~Emp, " ")), 
                              q_95 = expression(paste(q[0.95]~RL(ICU))), q_5_emp = expression(paste(q[0.95]~Emp, " "))))+
  scale_linetype_manual(values = c(4, 4, 1, 1, 3, 3),
                        #breaks = c("q_05", "q_05_emp", "q_5_emp", "q_5_est","q_95", "q_95_emp"),
                        labels=c(q_05=expression(paste(q[0.05]~RL(ICU))), q_05_emp = expression(paste(q[0.05]~Emp, " ")), 
                                 q_5_est =expression(paste(q[0.50]~RL(ICU))), q_5_emp = expression(paste(q[0.50]~Emp, " ")), 
                                 q_95 = expression(paste(q[0.95]~RL(ICU))), q_5_emp = expression(paste(q[0.95]~Emp, " "))))


q_plot_d_30

fig4 <- ggarrange(snap_res_a, q_plot_a_30, snap_res_b, q_plot_b_30, snap_res_d, q_plot_d_30, nrow = 3, ncol = 2)
fig4
ggsave(paste0("../plots/fig4.png"), units="in", dpi = 300, fig4, height = 6.8, width = 5.2)
ggsave(paste0("../plots/fig4.tiff"), units="in", dpi = 300, fig4, height = 6.8, width = 5.2)
ggsave(paste0("../plots/q_plot_modRL.png"), q_plot_d_30, width = 3.5,
       height = 3)


#Fig 5
rep_plot_a <- res_df %>%
  pivot_longer(c(med_a, n_true_retro)) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, color = name, linetype = name)) +
  geom_ribbon(aes(date, ymin=q5_a, ymax=q95_a), fill=wes_cols[4], alpha=.2) +
  #geom_ribbon(aes(date, ymin=q5_d, ymax=q95_d), fill=wes_cols[5], alpha=.2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d", expand=c(0.02,0.02)) +
  coord_cartesian(ylim = c(0, 250))+
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 8, family="TT Arial"),
        legend.box.margin=margin(-15,-15,-15,-15))+
  scale_color_manual(values = c( wes_cols[4], wes_cols[1]),
                     labels=c("R", "True number"))+
  scale_linetype_manual(values = c(1, 2),
                        labels=c("R", "True number"))
rep_plot_a

rep_plot_b <-  res_df %>%
  pivot_longer(c(med_b, n_true_retro)) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, color = name, linetype = name)) +
  geom_ribbon(aes(date, ymin=q5_b, ymax=q95_b), fill=wes_cols[6], alpha=.2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 250))+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%y-%m-%d",
               expand=c(0.02,0.02)) +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 8, family="TT Arial"),
        legend.box.margin=margin(-15,-15,-15,-15))+
  scale_color_manual(values = c(med_b = wes_cols[6], n_true_retro = wes_cols[1]),
                     labels = c("L(ICU)", "True number"))+
  scale_linetype_manual(values = c(med_b = 1, n_true_retro = 2),
                        labels=c("L(ICU)","True number")) 
rep_plot_b

rep_plot_d <- res_df %>%
  pivot_longer(c(med_d, n_true_retro)) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, color = name, linetype = name)) +
  geom_ribbon(aes(date, ymin=q5_d, ymax=q95_d), fill=wes_cols[5], alpha=.2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 250))+
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d", 
               limits = c(as.Date("2020-10-20"), as.Date("2021-05-21")), expand=c(0.02,0.02)) +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 8, family="TT Arial"),
        legend.box.margin=margin(-15,-15,-15,-15))+
  scale_color_manual(values = c( wes_cols[5], wes_cols[1]),
                     labels=c("RL(ICU)", "True number"))+
  scale_linetype_manual(values = c(1, 2),
                        labels=c("RL(ICU)", "True number"))
rep_plot_d

fig5 <- ggarrange(rep_plot_a, rep_plot_b, rep_plot_d, nrow = 3)
fig5
ggsave(paste0("../plots/fig5.png"), units="in", dpi = 300, fig5, height = 5.8, width = 5.2)
ggsave(paste0("../plots/fig5.tiff"), units="in", dpi = 300, fig5, height = 5.8, width = 5.2)
ggsave(paste0("../plots/eval_period.png"), q_plot_d_30, width = 3.5,
       height = 3)

# Fig 6
#Log Score 7
log_plot_7 <- res_df %>% select(date, log_a_7, log_b_7, log_d_7) %>%
  pivot_longer(starts_with("log"), names_to = "model", values_to = "log") %>% 
  #mutate(model = factor(model, levels = c("log_a", "log_d", "log_b"))) %>% 
  ggplot(aes(x = date, y = log)) +
  geom_line(aes(color = model, linetype = model)) +
  ylab("LogS") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values= c(wes_cols[4], wes_cols[5], wes_cols[1]), 
                     labels=c(log_a_7= "R", log_d_7= "RL(ICU)", log_b_7= "L(ICU)"))+
  scale_linetype_manual(values = c(1, 2, 3),
                        labels=c(log_a_7= "R", log_d_7= "RL(ICU)", log_b_7= "L(ICU)")) 

log_plot_7

