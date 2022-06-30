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
setwd("/media/fabe4028/suhome/Documents/GitHub/nowcasting_covid19/code")

# Import COVID data and Nowcast results
dat <- read_csv("../data/covid_deaths.csv")
res_df <- read_csv("../results/results_20220421.csv") %>% filter(date >= "2020-10-20", date <="2021-05-21")

dat %>% filter(death_date >= "2020-10-20", death_date <="2021-05-21") %>% #nrow()
  mutate(delay = rep_date -death_date) %>% filter(delay > 35) %>% nrow()

# Plot theme and color
theme_set(theme_bw())
wes_cols <- c(wes_palette("Darjeeling1", 5), wes_palette("GrandBudapest2"),wes_palette("Cavalcanti1"))

mypal = pal_nejm()(8)

mypal

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
        text = element_text(size = 8, family="TT Arial")) +
  scale_fill_manual(values = c("Reported"="grey30","Occurred but not yet reported" = "gray" ))

obs_plot

ggsave(paste0("../plots/obs_", now, ".png"), units="in", dpi = 300, obs_plot, height = 3.5, width = 5.2)
ggsave(paste0("../plots/fig1.png"), units="in", dpi = 300, obs_plot, height = 3.5, width = 5.2)
ggsave(paste0("../plots/fig1.tiff"), units="in", dpi = 300, obs_plot, height = 3.5, width = 5.2, compression = 'lzw')

## Error plots

#RMSE
rmse_plot <- res_df %>% select(date, err_a, err_d, err_b) %>% 
  pivot_longer(starts_with("err"), names_to = "model", values_to = "rmse") %>% 
  #mutate(model = factor(model, levels = c("err_a", "err_d", "err_b"))) %>% 
  ggplot(aes(x = date, y = rmse)) +
  geom_line(aes(color = model, linetype = model)) +
  ylab("RMSE") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values= c(wes_cols[4], wes_cols[5], wes_cols[1]), 
                     labels=c(err_a= "R", err_d= "RL(ICU)", err_b= "L(ICU)"))+
  scale_linetype_manual(values = c(1, 2, 3),
                        labels=c(err_a= "R", err_d= "RL(ICU)", err_b= "L(ICU)"))

rmse_plot

ggsave(paste0("../plots/rmse.png"), rmse_plot, width = 6,
       height = 4)

#RMSE 7days
rmse7_plot <- res_df %>% select(date, err_a_7, err_d_7, err_b_7) %>% 
  pivot_longer(starts_with("err"), names_to = "model", values_to = "rmse") %>% 
  #mutate(model = factor(model, levels = c("err_a_7", "err_d_7", "err_b_7"))) %>% 
  ggplot(aes(x = date, y = rmse)) +
  geom_line(aes(color = model, linetype = model)) +
  ylab("RMSE") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values= c(wes_cols[4], wes_cols[5], wes_cols[1]), 
                     labels=c(err_a_7= "R", err_d_7= "RL(ICU)", err_b_7= "L(ICU)"))+
  scale_linetype_manual(values = c(1, 2, 3),
                        labels=c(err_a_7= "R", err_d_7= "RL(ICU)", err_b_7= "L(ICU)")) 

rmse7_plot

ggsave(paste0("../plots/rmse7.png"), rmse7_plot, width = 6,
       height = 4)



#Log Score
log_plot <- res_df %>% select(date, log_a, log_b, log_d) %>% #select(date, contains("log"), -log_d_7) %>%  
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
                     labels=c(log_a= "R", log_d= "RL(ICU)", log_b= "L(ICU)"))+
  scale_linetype_manual(values = c(1, 2, 3),
                        labels=c(log_a= "R", log_d= "RL(ICU)", log_b= "L(ICU)")) 

log_plot

ggsave(paste0("../plots/log_score.png"), log_plot, width = 6,
       height = 4)

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

ggsave(paste0("../plots/log_score_7.png"), log_plot_7, width = 6,
       height = 4)


# CRPS
crps_plot <- res_df %>% select(date, crps_a, crps_b, crps_d) %>%
  pivot_longer(starts_with("crps"), names_to = "model", values_to = "crps") %>% 
  # mutate(model = factor(model, levels = c("crps_a", "crps_d", "crps_b"))) %>% 
  ggplot(aes(x = date, y = crps)) +
  geom_line(aes(color = model, linetype = model)) +
  ylab("CRPS") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values=c(wes_cols[4], wes_cols[5], wes_cols[1]), 
                     labels=c(crps_a= "R", crps_d= "RL(ICU)", crps_b= "L(ICU)"))+
  scale_linetype_manual(values = c(1, 2, 3),
                        labels=c(crps_a= "R", crps_d= "RL(ICU)", crps_b= "L(ICU)")) 

crps_plot

ggsave(paste0("../plots/crps.png"), crps_plot, width = 6, height = 4)


# CRPS 7
crps7_plot <- res_df %>% select(date, crps_a_7, crps_b_7, crps_d_7) %>%
  pivot_longer(starts_with("crps"), names_to = "model", values_to = "crps") %>% 
 # mutate(model = factor(model, levels = c("crps_a", "crps_d", "crps_b"))) %>% 
  ggplot(aes(x = date, y = crps)) +
  geom_line(aes(color = model, linetype = model)) +
  ylab("CRPS") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values=c(wes_cols[4], wes_cols[5], wes_cols[1]), 
                     labels=c(crps_a_7 = "R", crps_d_7= "RL(ICU)", crps_b_7= "L(ICU)"))+
  scale_linetype_manual(values = c(1, 2, 3),
                        labels=c(crps_a_7= "R", crps_d_7= "RL(ICU)", crps_b_7= "L(ICU)")) 

crps7_plot
 
ggsave(paste0("../plots/crps7.png"), crps7_plot, width = 6, height = 4)


# CRPS mod B
crps_b_plot <- res_df %>% select(date, crps_b_7, crps_b_c_7, crps_c_7) %>%
  pivot_longer(starts_with("crps"), names_to = "model", values_to = "crps") %>% 
  # mutate(model = factor(model, levels = c("crps_a", "crps_d", "crps_b"))) %>% 
  ggplot(aes(x = date, y = crps)) +
  geom_line(aes(color = model, linetype = model)) +
  ylab("CRPS") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_linetype_manual(values = c(1, 2, 3),
                        labels=c( crps_b_7= "L(ICU)", crps_b_c_7 = "L(Cases)", crps_c_7= "L(Cases, ICU)")) +
  scale_color_manual(values=c(wes_cols[4], wes_cols[5], wes_cols[1]), 
                     labels=c( crps_b_7= "L(ICU)", crps_b_c_7 = "L(Cases)", crps_c_7= "L(Cases, ICU)"))


crps_b_plot

ggsave(paste0("../plots/crps_b.png"), crps_b_plot, width = 6, height = 4)


# CRPS mod D
crps_d_plot <- res_df %>% select(date, crps_d_7, crps_d_c_7, crps_e_7) %>%
  pivot_longer(starts_with("crps"), names_to = "model", values_to = "crps") %>% 
  # mutate(model = factor(model, levels = c("crps_a", "crps_d", "crps_b"))) %>% 
  ggplot(aes(x = date, y = crps)) +
  geom_line(aes(color = model, linetype = model)) +
  ylab("CRPS") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_linetype_manual(values = c(1, 2, 3),
                        labels=c( crps_d_7= "RL(ICU)", crps_d_c_7 = "RL(Cases)", crps_e_7= "RL(Cases, ICU)")) +
  scale_color_manual(values=c(wes_cols[4], wes_cols[5], wes_cols[1]), 
                     labels=c( crps_d_7= "RL(ICU)", crps_d_c_7 = "RL(Cases)", crps_e_7= "RL(Cases, ICU)"))
 

crps_d_plot

ggsave(paste0("../plots/crps_d.png"), crps_d_plot, width = 6, height = 4)


# Summaries
rmse_score <- res_df %>%
  summarize(mean_a = mean(err_a), 
            mean_a_7 = mean(err_a_7),
            mean_b = mean(err_b), 
            mean_b_7 = mean(err_b_7),
            mean_b_c = mean(err_b_c), 
            mean_b_c_7 = mean(err_b_c_7), 
            mean_c = mean(err_c), 
            mean_c_7 = mean(err_c_7), 
            mean_d = mean(err_d), 
            mean_d_7 = mean(err_d_7), 
            mean_d_c = mean(err_d_c), 
            mean_d_c_7 = mean(err_d_c_7), 
            mean_e = mean(err_e),
            mean_e_7 = mean(err_e_7), 
  )

rmse_score

log_score <- res_df %>%
  summarize(mean_a = mean(log_a), 
            mean_a_7 = mean(log_a_7),
            mean_b = mean(log_b), 
            mean_b_7 = mean(log_b_7),
            mean_b_c = mean(log_b_c), 
            mean_b_c_7 = mean(log_b_c_7), 
            mean_c = mean(log_c), 
            mean_c_7 = mean(log_c_7), 
            mean_d = mean(log_d), 
            mean_d_7 = mean(log_d_7), 
            mean_d_c = mean(log_d_c), 
            mean_d_c_7 = mean(log_d_c_7), 
            mean_e = mean(log_e),
            mean_e_7 = mean(log_e_7), 
  )


log_score



crps_score <- res_df %>% filter(date > "2020-11-01") %>% 
  summarize(mean_a = mean(crps_a), 
            mean_a_7 = mean(crps_a_7),
            mean_b = mean(crps_b), 
            mean_b_7 = mean(crps_b_7),
            mean_b_c = mean(crps_b_c), 
            mean_b_c_7 = mean(crps_b_c_7), 
            mean_c = mean(crps_c), 
            mean_c_7 = mean(crps_c_7), 
            mean_d = mean(crps_d), 
            mean_d_7 = mean(crps_d_7), 
            mean_d_c = mean(crps_d_c), 
            mean_d_c_7 = mean(crps_d_c_7), 
            mean_e = mean(crps_e),
            mean_e_7 = mean(crps_e_7), 
  )


crps_score


## Results over time

rep_plot_a <- res_df %>%
  pivot_longer(c(med_a, n_true_retro)) %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = value, color = name, linetype = name)) +
    geom_ribbon(aes(date, ymin=q5_a, ymax=q95_a), fill=wes_cols[4], alpha=.2) +
    #geom_ribbon(aes(date, ymin=q5_d, ymax=q95_d), fill=wes_cols[5], alpha=.2) +
    ylab("Number Fatalities") +
    xlab("Date") +
    scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
    coord_cartesian(ylim = c(0, 250))+
    theme(legend.background = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank())+
  scale_color_manual(values = c( wes_cols[4], wes_cols[1]),
                     labels=c("R", "True number"))+
  scale_linetype_manual(values = c(1, 2),
                        labels=c("R", "True number"))
rep_plot_a

ggsave(paste0("../plots/res_moda.png"), rep_plot_a, width = 5,
       height = 3)

rep_plot_d <- res_df %>%
  pivot_longer(c(med_d, n_true_retro)) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, color = name, linetype = name)) +
  geom_ribbon(aes(date, ymin=q5_d, ymax=q95_d), fill=wes_cols[5], alpha=.2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 250))+
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d", 
               limits = c(as.Date("2020-10-20"), as.Date("2021-05-21"))) +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c( wes_cols[5], wes_cols[1]),
                     labels=c("RL(ICU)", "True number"))+
  scale_linetype_manual(values = c(1, 2),
                        labels=c("RL(ICU)", "True number"))
rep_plot_d

ggsave(paste0("../plots/res_modd.png"), rep_plot_d, width = 5,
       height = 3)

rep_plot3 <- res_df %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_d_c, color = "RL(Cases)")) +
  geom_line(aes(y = n_true_retro, color = "True number"), lty = 2) +
  geom_ribbon(aes(date, ymin=q5_d_c, ymax=q95_d_c), fill=wes_cols[8], alpha=.2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 250))+
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("RL(Cases)" = wes_cols[8], "True number" = wes_cols[1]))

rep_plot3

ggsave(paste0("../plots/res_modd_c.png"), rep_plot3, width = 5,
       height = 3)


rep_plot4 <-  res_df %>%
  pivot_longer(c(med_b, n_true_retro)) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, color = name, linetype = name)) +
  geom_ribbon(aes(date, ymin=q5_b, ymax=q95_b), fill=wes_cols[6], alpha=.2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 250))+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
    scale_color_manual(values = c(med_b = wes_cols[6], n_true_retro = wes_cols[1]),
                       labels = c("L(ICU)", "True number"))+
    scale_linetype_manual(values = c(med_b = 1, n_true_retro = 2),
                          labels=c("L(ICU)","True number")) 
rep_plot4

ggsave(paste0("../plots/res_modb.png"), rep_plot4, width = 5,
       height = 3)

rep_plot5 <-  res_df %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_b_c, color = "L(Cases)")) +
  geom_line(aes(y = n_true_retro, color = "True number"), lty = 2) +
  geom_ribbon(aes(date, ymin=q5_b_c, ymax=q95_b_c), fill=wes_cols[10], alpha=.2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 350))+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("L(Cases)" = wes_cols[10], "True number" = wes_cols[1]))
rep_plot5
ggsave(paste0("../plots/res_modb_c.png"), rep_plot5, width = 5,
       height = 3)

rep_plot_c <-  res_df %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_c, color = "L(Cases, ICU)")) +
  geom_line(aes(y = n_true_retro, color = "True number"), lty = 2) +
  geom_ribbon(aes(date, ymin=q5_c, ymax=q95_c), fill=wes_cols[11], alpha=.2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 350))+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("L(Cases, ICU)" = wes_cols[11], "True number" = wes_cols[1]))
rep_plot_c
ggsave(paste0("../plots/res_mod_c.png"), rep_plot_c, width = 5,
       height = 3)

rep_plot6 <- res_df %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_e, color = "RL(ICU, Cases)")) +
  geom_line(aes(y = n_true_retro, color = "True number"), lty = 2) +
  geom_ribbon(aes(date, ymin=q5_d_c, ymax=q95_d_c), fill=wes_cols[9], alpha=.2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 250))+
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("RL(ICU, Cases)" = wes_cols[9], "True number" = wes_cols[1]))

rep_plot6

ggsave(paste0("../plots/res_mod_e.png"), rep_plot6, width = 5,
       height = 3)

## Single reporting day
# Restrict dataset to a specific nowcast date
rep_dates <- list.files(path = paste0("../data/FoHM/")) %>% 
  str_extract("\\d+-\\d+-\\d+") %>%  
  as.data.frame %>% distinct() %>% 
  #dat %>%
  #select(rep_date) %>%
  filter(. >= "2020-09-15") %>%  #, rep_date <= "2021-05-31")  %>%
  #distinct() %>% 
  t() %>%
  as.vector()

n <- 60
now <- ymd(rep_dates[n])
start <- ymd(now - 7*8+1)
dat_mod = dat %>%
  filter(rep_date <= now)

m <-40
N_a <-N_mod_a[[m]]
N_b <-N_mod_b[[m]]
N_d <-N_mod_d[[m]]
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

snap_res_1 <- dat_mod %>% group_by(date=death_date) %>%
  summarise(n_obs=n()) %>%
  right_join(tibble(date=seq(start, now, by="1 day"))) %>%
  left_join(post_N_a) %>%
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date > (now-36)) %>%
  pivot_longer(c(med_a, n_true_retro)) %>% 
  ggplot() + 
  geom_line(aes(date, value, col = name, linetype = name )) +
  geom_ribbon(aes(date, ymin=q5_a, ymax=q95_a), fill=wes_cols[4], alpha=.2) +
  geom_col(aes(date, n_obs/2)) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 160))+
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 8, family="TT Arial"))+
  scale_color_manual(values = c(med_a = wes_cols[4], n_true_retro = wes_cols[1]),
                     labels = c("R", "True number"))+
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand=c(0.02,0.02))+
  scale_linetype_manual(values = c(med_a = 1, n_true_retro = 2),
                        labels=c("R","True number")) 
  
snap_res_1

ggsave(paste0("../plots/snap_res1.png"), snap_res_1, width = 3.5,
       height = 3)
ggsave(paste0("../plots/fig4.png"), units="in", dpi = 300, snap_res_1, height = 3.5, width = 5.2)
ggsave(paste0("../plots/fig4.tiff"), units="in", dpi = 300, snap_res_1, height = 3.5, width = 5.2, compression = 'lzw')


snap_res_2 <- dat_mod %>% group_by(date=death_date) %>%
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
        text = element_text(size = 8, family="TT Arial"))+
  scale_color_manual(values = c(med_b = wes_cols[6], n_true_retro = wes_cols[1]),
                     labels = c("L(ICU)", "True number"))+
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand=c(0.02,0.02))+
  scale_linetype_manual(values = c(med_b = 1, n_true_retro = 2),
                        labels=c("L(ICU)","True number")) 

snap_res_2

ggsave(paste0("../plots/snap_res2.png"), snap_res_2, width = 3.5,
       height = 3)



snap_res_3 <- dat_mod %>% group_by(date=death_date) %>%
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
  coord_cartesian(ylim = c(0, 165))+
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 8, family="TT Arial"))+
  scale_color_manual(values = c(med_d = wes_cols[5], n_true_retro = wes_cols[1]),
                     labels = c("RL(ICU)", "True number"))+
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand=c(0.02,0.02))+
  scale_linetype_manual(values = c(med_d = 1, n_true_retro = 2),
                        labels=c("RL(ICU)","True number")) 

snap_res_3

ggsave(paste0("../plots/snap_res3.png"), snap_res_3, width = 3.5,
       height = 3)


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





# Beta 0
res_beta_0 <- read_csv("../results/results_beta_0.csv") %>% filter(date >= "2020-10-15", date <="2021-05-14") 
beta_0_plot <- res_beta_0 %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_b, color = "L(ICU)")) +
  geom_ribbon(aes(date, ymin=q5_b, ymax=q95_b), fill=wes_cols[7], alpha=.2) +
  ylab(expression(beta[0])) +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("L(ICU)" = wes_cols[7]))
beta_0_plot

ggsave(paste0("../plots/beta_0_mod_b.png"), beta_0_plot, width = 6,
       height = 4)

# Beta 1
res_beta_1 <- read_csv("../results/results_beta_1_mod_b.csv") %>% filter(date >= "2020-10-15", date <="2021-05-14")
beta_1_plot <- res_beta_1 %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_b, color = "L(ICU)")) +
  geom_ribbon(aes(date, ymin=q5_b, ymax=q95_b), fill=wes_cols[8], alpha=.2) +
  ylab(expression(beta[1])) +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("L(ICU)" = wes_cols[8]))
beta_1_plot

ggsave(paste0("../plots/beta_1_mod_b.png"), beta_1_plot, width = 6,
       height = 4)


res_beta_1_d <- read_csv("../results/results_beta_1_mod_d.csv") %>% filter(date >= "2020-10-15", date <="2021-05-15")
beta_1_plot_d <- res_beta_1_d %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_d, color = "L(ICU)")) +
  geom_ribbon(aes(date, ymin=q5_d, ymax=q95_d), fill=wes_cols[9], alpha=.2) +
  ylab(expression(beta[1])) +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("RL(ICU)" = wes_cols[9]))
beta_1_plot_d

ggsave(paste0("../plots/beta_1_mod_d.png"), beta_1_plot_d, width = 6,
       height = 4)


# Split table 
# Summaries
rmse_score1 <- res_df %>% filter(date < "2021-02-02") %>% 
  summarize(mean_a = mean(err_a), mean_b = mean(err_b), 
            #mean_c = mean(err_c), 
            mean_d = mean(err_d), 
            # mean_d_n = mean(err_d_n), 
            #mean_f = mean(err_f)
  )

rmse_score1

rmse_score2 <- res_df %>% filter(date >= "2021-02-02") %>% 
   summarize(mean_a = mean(err_a), mean_b = mean(err_b), 
            #mean_c = mean(err_c), 
            mean_d = mean(err_d), 
            # mean_d_n = mean(err_d_n), 
            #mean_f = mean(err_f)
  )

rmse_score2

log_score_1 <- res_df %>% filter(date < "2021-02-02") %>% 
  summarize(mean_a = mean(log_a), 
            mean_b = mean(log_b), 
            #mean_c = mean(log_c), 
            mean_d = mean(log_d),  
            #mean_d_n = mean(log_d_n), 
            #mean_f = mean(log_f)
  )
log_score_1
log_score_2 <- res_df %>% filter(date >= "2021-02-02") %>% 
  summarize(mean_a = mean(log_a), 
            mean_b = mean(log_b), 
            #mean_c = mean(log_c), 
            mean_d = mean(log_d),  
            #mean_d_n = mean(log_d_n), 
            #mean_f = mean(log_f)
  )

log_score_2

crps_score1 <- res_df %>% filter(date < "2021-02-02") %>% 
  summarize(mean_a = mean(crps_a), mean_b = mean(crps_b), 
            #mean_c = mean(crps_c), 
            mean_d = mean(crps_d), 
            #mean_e = mean(crps_d_n), 
  )

crps_score1

crps_score2 <- res_df %>% filter(date >= "2021-02-02") %>% 
  summarize(mean_a = mean(crps_a), mean_b = mean(crps_b), 
            #mean_c = mean(crps_c), 
            mean_d = mean(crps_d), 
            #mean_e = mean(crps_d_n), 
  )

crps_score2



# Spikes
n <- 53
now <- ymd(rep_dates[n])
start <- ymd(now - 7*8+1)
dat_mod = dat %>%
  filter(rep_date <= now)

N_a <-N_mod_a[[n]]
N_b <-N_mod_b[[n]]
N_d <-N_mod_d[[n]]
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


snap_res_1 <- dat_mod %>% group_by(date=death_date) %>%
  summarise(n_obs=n()) %>%
  right_join(tibble(date=seq(start, now, by="1 day"))) %>%
  left_join(post_N_a) %>%
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date > (now-36)) %>%
  ggplot() + 
  geom_line(aes(date, med_a, color = "R")) +
  geom_ribbon(aes(date, ymin=q5_a, ymax=q95_a), fill=wes_cols[4], alpha=.2) +
  geom_col(aes(date, n_obs)) +
  geom_line(aes(date, n_true_retro, color="True number"), lty=2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  #ylim(0, 175) +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("R" = wes_cols[4], "True number" = wes_cols[1]))+
  scale_x_date(date_breaks = "5 days", date_labels = "%y-%m-%d")

snap_res_1

ggsave(paste0("../plots/spike_snap_res1.png"), snap_res_1, width = 6,
       height = 4)

reported_plot <- dat %>% group_by(date=death_date) %>%
  summarise(n_true_retro=n()) %>%
  left_join(dat %>% group_by(date=rep_date) %>%
              summarise(n_rep=n())) %>% 
  filter(date >= "2020-10-15", date <= "2021-05-15") %>%
  ggplot(aes(x = date, y = n_rep)) +geom_col()+
  geom_line(aes(y = n_true_retro, col = "Actual deaths"))+
  geom_vline(xintercept=c( as.Date("2020-12-01"), as.Date("2020-12-15"), 
                     as.Date("2020-11-17")), #as.Date("2021-02-02")) , 
             linetype="dashed", color = "blue")+
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d")+
  ylab("Number Reported Fatalities") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
        xlab("Date")
reported_plot

ggsave(paste0("../plots/reported_actual.png"), reported_plot, width = 6,
       height = 4)

dat %>% 
  filter(rep_date > "2020-11-01", rep_date < "2020-12-01") %>%
  mutate(delay = as.numeric(rep_date-death_date)) %>%
  group_by(delay) %>% 
  summarize(n = n()) %>% 
   ggplot() + geom_col(aes(x = delay, y = n))  
# rep day snap
#dates <- c( as.Date("2020-12-01"), as.Date("2020-12-15"), 
  # as.Date("2021-01-12"), as.Date("2020-11-17"), as.Date("2021-02-02"))
n <- 37 #45 53 64 76
now <- ymd(rep_dates[n])
start <- ymd(now - 7*8+1)
dat_mod = dat %>%
  filter(rep_date <= now)

N_a <-N_mod_a[[n]]
N_b <-N_mod_b[[n]]
N_d <-N_mod_d[[n]]
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


spike1 <- dat_mod %>% group_by(date=death_date) %>%
  summarise(n_obs=n()) %>%
  right_join(tibble(date=seq(start, now, by="1 day"))) %>%
  left_join(post_N_a) %>%
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date > (now-36)) %>%
  ggplot() + 
  geom_line(aes(date, med_a, col = "R")) +
  geom_ribbon(aes(date, ymin=q5_a, ymax=q95_a), fill=wes_cols[6], alpha=.2) +
  geom_col(aes(date, n_obs)) +
  geom_line(aes(date, n_true_retro, color="True number"), lty=2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("R" = wes_cols[6], "True number" = wes_cols[1]))+
  scale_x_date(date_breaks = "5 days", date_labels = "%y-%m-%d")

spike1


spike2 <- dat_mod %>% group_by(date=death_date) %>%
  summarise(n_obs=n()) %>%
  right_join(tibble(date=seq(start, now, by="1 day"))) %>%
  left_join(post_N_d) %>%
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date > (now-36)) %>%
  ggplot() + 
  geom_line(aes(date, med_d, col = "RL(ICU)")) +
  geom_ribbon(aes(date, ymin=q5_d, ymax=q95_d), fill=wes_cols[6], alpha=.2) +
  geom_col(aes(date, n_obs)) +
  geom_line(aes(date, n_true_retro, color="True number"), lty=2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("RL(ICU)" = wes_cols[6], "True number" = wes_cols[1]))+
  scale_x_date(date_breaks = "5 days", date_labels = "%y-%m-%d")

spike2





ggsave(paste0("../plots/snap_res2.png"), snap_res_2, width = 6,
       height = 4)



snap_res_3 <- dat_mod %>% group_by(date=death_date) %>%
  summarise(n_obs=n()) %>%
  right_join(tibble(date=seq(start, now, by="1 day"))) %>%
  left_join(post_N_d) %>%
  left_join(dat %>% group_by(date=death_date) %>%
              summarise(n_true_retro=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date > (now-36)) %>%
  ggplot() + 
  geom_line(aes(date, med_d, col = "RL(ICU)")) +
  geom_ribbon(aes(date, ymin=q5_d, ymax=q95_d), fill=wes_cols[5], alpha=.2) +
  geom_col(aes(date, n_obs)) +
  geom_line(aes(date, n_true_retro, color="True number"), lty=2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  #ylim(0,175) +
  theme(legend.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("RL(ICU)" = wes_cols[5], "True number" = wes_cols[1]))+
  scale_x_date(date_breaks = "5 days", date_labels = "%y-%m-%d")

snap_res_3

ggsave(paste0("../plots/snap_res3.png"), snap_res_3, width = 6,
       height = 4)

# Time series plot
