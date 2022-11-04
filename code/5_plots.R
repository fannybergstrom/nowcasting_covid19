#### Plots and tables ####

# Load packages and functions
source("./code/2_functions.r")

loadfonts(device = "win")

# Import COVID data and Nowcast results
dat <- read_csv("./data/covid_deaths.csv")
res_df <- read_csv("./results/summarized_results_and_tables/results.csv") %>% 
  filter(date >= "2020-10-20", date <= "2021-05-21")

# Plot theme and color
theme_set(theme_bw())
cols <- pal_nejm("default", alpha = 1)(8)
wes_cols <- c(wes_palette("Darjeeling1", 5),
              wes_palette("Zissou1", 8, "continuous"),
              wes_palette("GrandBudapest2"))
vir_cols <- viridis_pal()(12)

# Figure 1, observed and unreported.
now <- "2022-02-01"
start <- "2022-01-01"
obs_plot <- dat %>%
  group_by(date = death_date) %>%
  summarise(n_true_retro = sum(n)) %>%
  left_join(dat %>% filter(rep_date <= now) %>% 
              group_by(date = death_date) %>%
              summarise(n_obs = sum(n))) %>%
  mutate_if(is.integer, ~ replace(., is.na(.), 0)) %>%
  filter(date >= start, date <= now) %>%
  ggplot() +
  geom_col(aes(date, n_true_retro, fill = "Occurred but not yet reported")) +
  geom_col(aes(date, n_obs, fill = "Reported")) +
  ylab("Number Fatalities") +
  xlab("Date") +
  scale_y_continuous(breaks = 0:5*10, expand = c(0.02, 0.02)) +
  scale_x_date(breaks = as.Date(c(
    "2022-01-01", "2022-01-07", "2022-01-13", "2022-01-19",
    "2022-01-26", "2022-02-01"
  )), date_labels = "%y-%m-%d", expand = c(0.02, 0.02)) +
  theme(
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-10, -10, -10, -10)
  ) +
  scale_fill_manual(values = c("Reported" = "grey30", "Occurred but not yet reported" = "gray"))

obs_plot

ggsave(paste0("./plots/fig1.png"), units = "in", dpi = 300, obs_plot, height = 3.5, width = 5.2)
#ggsave(paste0("./plots/fig1.tiff"), units = "in", dpi = 300, obs_plot, height = 3.5, width = 5.2, compression = "lzw")


# Fig 2
FHM_ICU <- read_excel("./data/fohm/fohm_covid19_2022-05-12.xlsx",
  sheet = "Antal intensivvårdade per dag"
)

FHM_deaths <- read_excel("./data/fohm/fohm_covid19_2022-05-12.xlsx",
  sheet = "Antal avlidna per dag", col_types = c("date", "numeric")
)

FHM_cases <- read_excel("./data/FoHM/fohm_covid19_2022-05-12.xlsx",
  sheet = "Antal per dag region"
)

# Remove final row
FHM_deaths <- FHM_deaths[-nrow(FHM_deaths), ]

# Save dates
now <- ymd("2021-12-31")
start <- now - 180

# Create time series data frame
ts <- FHM_deaths %>%
  select(date = Datum_avliden, n_deaths = Antal_avlidna) %>%
  left_join(FHM_ICU %>% select(date = Datum_vårdstart, n_icu = Antal_intensivvårdade)) %>%
  left_join(FHM_cases %>% select(date = Statistikdatum, n_cases = Totalt_antal_fall)) %>%
  mutate(
    n_deaths_lag1 = lag(n_deaths, 1),
    mean_c = rollmean(n_cases, k = 21, fill = NA, align = "center"),
    mean_d = rollmean(n_deaths, k = 21, fill = NA, align = "center"),
    mean_i = rollmean(n_icu, k = 21, fill = NA, align = "center")
  )

max_death <- ts %>% filter(date < "2021-02-21") %>% 
  filter(mean_d == max(mean_d, na.rm = T))
max_icu <- ts %>% filter(date < "2021-02-21", date > "2020-11-01") %>% 
  filter(mean_i == max(mean_i, na.rm = T))
max_cases <- ts %>% filter( date < "2021-02-21") %>% 
  filter(mean_c == max(mean_c, na.rm = T))


timeseries_plots <- ts %>%
  filter(date > "2020-10-20", date <= "2021-05-21") %>%
  mutate(mean_c = mean_c / max_cases$mean_c, mean_i = mean_i / max_icu$mean_i, mean_d = mean_d / max_death$mean_d) %>%
  pivot_longer(c(mean_c, mean_d, mean_i)) %>%
  ggplot() +
  geom_segment(aes(
    x = as.Date("2020-12-27"), xend = as.Date("2020-12-27"),
    y = -Inf, yend = Inf
  ), color = "black") +
  geom_line(aes(as.Date(date), value, color = name, linetype = name)) +
  ylab("3-week average value (scaled)") +
  xlab("Date") +
  coord_cartesian(ylim = c(0.05, 1.15)) +
  annotate("text",
    x = as.Date("2020-12-14"), y = 0.2, cex = 2.1,
    label = "Vaccination\nstart 20-12-27", color = "black"
  ) +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-10, -10, -10, -10)
  ) +
  scale_x_date(
    date_breaks = "1 month", date_labels = "%y-%m-%d",
    limits = c(as.Date("2020-10-20"), as.Date("2021-05-21")), expand = c(0.02, 0.02)
  ) +
  scale_y_continuous(breaks=seq(0, 1, by = 0.25), c(0.02, 0.02))+
  scale_color_manual(
    values = c(mean_c = cols[6], mean_i = cols[3], mean_d = cols[1]),
    labels = c(mean_c = "Reported cases", mean_i = "ICU admissions", mean_d = "Fatalities")
  ) +
  scale_linetype_manual(
    values = c(mean_c = 1, mean_i = 4, mean_d = 2),
    labels = c(mean_c = "Reported cases", mean_i = "ICU admissions", mean_d = "Fatalities")
  )

timeseries_plots

ggsave(paste0("./plots/fig2.png"), units = "in", dpi = 300, timeseries_plots, height = 3.1, width = 5.2)
#ggsave(paste0("./plots/fig2.tiff"), units = "in", dpi = 300, timeseries_plots, height = 3.1, width = 5.2, compression = "lzw")

# Fig 3 is a Tikz figure

# Fig 4
now
## Single reporting day
fig_1day <- function(N_mod, now = as.Date("2020-12-30"), fig_col = "blue", fig_label = "Model type"){
  post_N <- tibble(
    date = seq(now - (55), now, "1 day"),
    med = apply(N_mod, 2, median),
    q5 = apply(N_mod, 2, function(x) quantile(x, .025)),
    q95 = apply(N_mod, 2, function(x) quantile(x, .975))
  )
  
  start <- ymd(now - 7 * 8 + 1)

  dat %>%
    filter(rep_date <= now) %>%
    group_by(date = death_date) %>%
    summarise(n_obs = sum(n)) %>%
    right_join(tibble(date = seq(start, now, by = "1 day"))) %>%
    left_join(post_N) %>%
    left_join(dat %>% group_by(date = death_date) %>%
                summarise(n_true_retro = sum(n))) %>%
    mutate_if(is.integer, ~ replace(., is.na(.), 0)) %>%
    filter(date > (now - 36)) %>%
    pivot_longer(c(med, n_true_retro)) %>%
    ggplot() +
    geom_line(aes(date, value, color = name, linetype = name)) +
    geom_col(aes(date, n_obs / 2)) +
    ylab("Number Fatalities") +
    xlab("Date") +
    geom_ribbon(aes(date, ymin = q5, ymax = q95), fill = fig_col, alpha = .2) +
    scale_color_manual(
      values = c(med = fig_col, n_true_retro = cols[1]),
      labels = c(fig_label, "True number")
    ) + 
    scale_linetype_manual(
      values = c(med = 1, n_true_retro = 2),
      labels = c(fig_label, "True number")
    ) +
    theme(
      legend.background = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      text = element_text(size = 8, family = "sans"),
      legend.box.margin = margin(-20, -15, -15, -15),
      legend.direction = "vertical",
      legend.spacing = unit(0.01, "mm"),
      plot.margin = margin(0.1, 0.4, 0.2, 0.1, "cm")
    )
}


rep_dates <- list.files(path = paste0("./data/fohm/")) %>%
  str_extract("\\d+-\\d+-\\d+") %>%
  as.data.frame() %>%
  distinct() %>%
  filter(. >= "2020-09-15") %>%
  t() %>%
  as.vector()

# Set dates
n <- 60
now <- ymd(rep_dates[n])
start <- ymd(now - 7 * 8 + 1)
path <- "./results/N/N_mod_"

# Import data
N_a <- lapply(str_c(path,"r_", now, ".csv"), read_csv) %>% as.data.frame()
N_b <- lapply(str_c(path,"l_", now, ".csv"), read_csv) %>% as.data.frame()
N_d <- lapply(str_c(path,"rl_", now, ".csv"), read_csv) %>% as.data.frame()

dates <- c(seq(as.IDate("2020-11-25"), as.IDate("2020-12-30"), 12), as.Date("2020-12-30"))

snap_res_a <- fig_1day(N_a, fig_col = cols[4], fig_label = "R") +
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand = c(0.02, 0.02))+
  coord_cartesian(ylim = c(0, 160))

snap_res_b <- fig_1day(N_b, fig_col = cols[2], fig_label = "L(ICU)") +
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand = c(0.02, 0.02))+
  coord_cartesian(ylim = c(0, 160))

snap_res_d <- fig_1day(N_d, fig_col = wes_cols[3], fig_label = "RL(ICU)") +
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand = c(0.02, 0.02))+
  coord_cartesian(ylim = c(0, 160))


# Q-plots
q_plot <- read_csv("./results/summarized_results_and_tables/q_plot.csv")
q_plot_a_30 <- q_plot %>% 
  filter(model == "r") %>% 
  ggplot() +
  geom_line(aes(x = as.Date(death_date), y = value, color = name, linetype = name)) +
  ylim(0, 35) +
  ylab("Delay (days)") +
  xlab("Date") +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15),
    legend.spacing = unit(0, "cm"),
    legend.text.align = 0,
    legend.key.width= unit(0.4, 'cm'),
    plot.margin = margin(0.1, 0.7, 0.2, 0.1, "cm")
  ) +
  scale_color_manual(
    values = c(rep(c(cols[4], cols[1]), 3)),
    labels = c(
      expression(paste(q[0.05] ~ R, " ")), expression(q[0.05] ~ Emp), expression(paste(q[0.50] ~ R, " ")),
      expression(q[0.50] ~ Emp), expression(paste(q[0.95] ~ R, " ")), expression(q[0.95] ~ Emp)
    )
  ) +
  scale_linetype_manual(
    values = c(4, 4, 1, 1, 3, 3),
    labels = c(
      expression(paste(q[0.05] ~ R, " ")), expression(q[0.05] ~ Emp), expression(paste(q[0.50] ~ R, " ")),
      expression(q[0.50] ~ Emp), expression(paste(q[0.95] ~ R, " ")), expression(q[0.95] ~ Emp)
    )
  )+
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand = c(0.02, 0.02))

q_plot_a_30

# Quantile plot mod L

q_plot_b_30 <-  q_plot %>% 
  filter(model == "l") %>%
  ggplot() +
  geom_line(aes(x = death_date, y = value, color = name, linetype = name)) +
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand = c(0.02, 0.02)) +
  ylim(0, 35) +
  ylab("Delay (days)") +
  xlab("Date") +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15),
    legend.spacing = unit(0, "cm"),
    legend.text.align = 0,
    legend.key.width= unit(0.4, 'cm'),
    plot.margin = margin(0.1, 0.7, 0.2, 0.1, "cm")
  ) +
  scale_color_manual(
    values = c(rep(c(cols[2], cols[1]), 3)),
    labels = c(
      expression(paste(q[0.05] ~ L(ICU))), expression(paste(q[0.05] ~ Emp, " ")),
      expression(paste(q[0.50] ~ L(ICU))), expression(paste(q[0.50] ~ Emp, " ")),
      expression(paste(q[0.95] ~ L(ICU))), expression(paste(q[0.95] ~ Emp, " "))
    ),
    # breaks = c("q_05", "q_05_emp", "q_5_emp", "q_5_est","q_95", "q_95_emp")
  ) +
  scale_linetype_manual(
    values = c(4, 4, 1, 1, 3, 3),
    labels = c(
      expression(paste(q[0.05] ~ L(ICU))), expression(paste(q[0.05] ~ Emp, " ")),
      expression(paste(q[0.50] ~ L(ICU))), expression(paste(q[0.50] ~ Emp, " ")),
      expression(paste(q[0.95] ~ L(ICU))), expression(paste(q[0.95] ~ Emp, " "))
    )
  )

q_plot_b_30

# Quantile plot mod RL
q_plot_d_30 <-  q_plot %>% 
  filter(model == "rl") %>% 
  ggplot() +
  geom_line(aes(x = death_date, y = value, color = name, linetype = name)) +
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand = c(0.02, 0.02)) +
  ylim(0, 35) +
  ylab("Delay (days)") +
  xlab("Date") +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15),
    legend.spacing = unit(0, "cm"),
    legend.text.align = 0,
    legend.key.width= unit(0.4, 'cm'),
    plot.margin = margin(0.1, 0.7, 0.2, 0.1, "cm")
  ) +
  scale_color_manual(
    values = c(rep(c(wes_cols[3], cols[1]), 3)),
    labels = c(
      q_05 = expression(paste(q[0.05] ~ RL(ICU))), q_05_emp = expression(paste(q[0.05] ~ Emp, " ")),
      q_5_est = expression(paste(q[0.50] ~Emp)), q_5_emp = expression(paste(q[0.50] ~ RL(ICU), " ")),
      q_95 = expression(paste(q[0.95] ~ RL(ICU))), q_95_emp = expression(paste(q[0.95] ~ Emp, " "))
    )
  ) +
  scale_linetype_manual(
    values = c(4, 4, 1, 1, 3, 3),
    labels = c(
      q_05 = expression(paste(q[0.05] ~ RL(ICU))), q_05_emp = expression(paste(q[0.05] ~ Emp, " ")),
      q_5_est = expression(paste(q[0.50] ~ Emp)), q_5_emp = expression(paste(q[0.50] ~ RL(ICU), " ")),
      q_95 = expression(paste(q[0.95] ~ RL(ICU))), q_95_emp = expression(paste(q[0.95] ~ Emp, " "))
    )
  )


q_plot_d_30


fig4 <- {snap_res_a + q_plot_a_30 + plot_layout(tag_level = 'new')} / 
        {snap_res_b + q_plot_b_30 + plot_layout(tag_level = 'new')} /
        {snap_res_d + q_plot_d_30 + plot_layout(tag_level = 'new')} +
    plot_annotation(tag_levels = c('A', '1')) +
    plot_layout( ncol = 1)

fig4
ggsave(paste0("./plots/fig4.png"), units = "in", dpi = 300, fig4, height = 6.8, width = 5.2)
#ggsave(paste0("./plots/fig4.tiff"), units = "in", dpi = 300, fig4, height = 6.8, width = 5.2)

# Fig 5
err_delay <- read_csv("./results/summarized_results_and_tables/error_by_delay.csv") %>% as.data.frame()

error_plot <- function(table, plot_error = "rmse", plot_label = "RMSE"){
  err_delay %>% 
    pivot_longer(starts_with(plot_error), names_to = "model", values_to = "error") %>%
    ggplot(aes(x = delay, y = error)) +
    geom_line(aes(color = model, linetype = model)) +
    ylab(plot_label) +
    xlab(expression(paste("Days since day ", italic("T")))) +
    scale_x_continuous(breaks = 0:7 * 5, expand = c(0.02, 0.02)) +
    theme(
      legend.background = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      text = element_text(size = 8, family = "sans"),
      legend.box.margin = margin(-15, -15, -15, -15),
      plot.margin = margin(0.1, 0.1, 0.5, 0, "cm")
    ) 
}
rmse_plot <-  error_plot(err_delay, plot_error = "rmse", plot_label = "RMSE") +
  scale_color_manual(
  values = c(cols[4], cols[2], wes_cols[3]),
  labels = c(rmse_a = "R", rmse_d = "RL(ICU)", rmse_b = "L(ICU)")
) +
  scale_linetype_manual(
    values = c(1, 3, 2),
    labels = c(rmse_a = "R", rmse_d = "RL(ICU)", rmse_b = "L(ICU)")
  )

rmse_plot

log_plot <- error_plot(err_delay, plot_error = "logs", plot_label = "logS")+
  scale_color_manual(
    values = c(cols[4], cols[2], wes_cols[3]),
    labels = c(logs_a = "R", logs_d = "RL(ICU)", logs_b = "L(ICU)")
  ) +
  scale_linetype_manual(
    values = c(1, 3, 2),
    labels = c(logs_a = "R", logs_d = "RL(ICU)", logs_b = "L(ICU)")
  )

log_plot


crps_plot <- error_plot(err_delay, "crps", "CRPS") +
  scale_color_manual(
    values = c(cols[4], cols[2], wes_cols[3]),
    labels = c(crps_a = "R", crps_d = "RL(ICU)", crps_b = "L(ICU)")
  ) +
  scale_linetype_manual(
    values = c(1, 3, 2),
    labels = c(crps_a = "R", crps_d = "RL(ICU)", crps_b = "L(ICU)")
  )

crps_plot

fig5 <- crps_plot + log_plot +
  rmse_plot +
  plot_annotation(tag_levels = c("A", "B")) +
  plot_layout(guides = "collect", ncol = 3) & theme(
    legend.position = "bottom",
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15)
  )

fig5
ggsave(paste0("./plots/fig5.png"), units = "in", dpi = 300, fig5, height = 2.5, width = 5.2)
#ggsave(paste0("./plots/fig5.tiff"), units = "in", dpi = 300, figS2, height = 2.5, width = 5.2)


# Fig 6
rep_plot_a <- res_df %>%
  pivot_longer(c(med_a, n_true_retro)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, color = name, linetype = name)) +
  geom_ribbon(aes(date, ymin = q5_a, ymax = q95_a), fill = cols[4], alpha = .2) +
  scale_color_manual(
    values = c(cols[4], cols[1]),
    labels = c("R", "True number")
  ) +
  scale_linetype_manual(
    values = c(1, 2),
    labels = c("R", "True number")
  )
rep_plot_a

rep_plot_b <- res_df %>%
  pivot_longer(c(med_b, n_true_retro)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, color = name, linetype = name)) +
  geom_ribbon(aes(date, ymin = q5_b, ymax = q95_b), fill = cols[2], alpha = .2) +
  scale_color_manual(
    values = c(med_b = cols[2], n_true_retro = cols[1]),
    labels = c("L(ICU)", "True number")
  ) +
  scale_linetype_manual(
    values = c(med_b = 1, n_true_retro = 2),
    labels = c("L(ICU)", "True number")
  )
rep_plot_b

rep_plot_d <- res_df %>%
  pivot_longer(c(med_d, n_true_retro)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = value, color = name, linetype = name)) +
  geom_ribbon(aes(date, ymin = q5_d, ymax = q95_d), fill = wes_cols[3], alpha = .2) +
  scale_color_manual(
    values = c(wes_cols[3], cols[1]),
    labels = c("RL(ICU)", "True number")
  ) +
  scale_linetype_manual(
    values = c(1, 2),
    labels = c("RL(ICU)", "True number")
  )

rep_plot_d

fig6 <- rep_plot_a + rep_plot_b + rep_plot_d +
  plot_annotation(tag_levels = c("A", "B")) +
  plot_layout(ncol = 1) & 
  ylab("Number Fatalities") &
  xlab("Date") &
  coord_cartesian(ylim = c(0, 250)) &
  scale_x_date(
    date_breaks = "1 month", date_labels = "%y-%m-%d",
    limits = c(as.Date("2020-10-20"), as.Date("2021-05-21")), expand = c(0.02, 0.02)
  ) &
  theme(
    legend.position = "bottom",
    plot.margin = margin(0.1, 0.1, 0.2, 0.1, "cm"),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15),
    legend.background = element_blank(),
    legend.title = element_blank()
  ) 


fig6
ggsave(paste0("./plots/fig6.png"), units = "in", dpi = 300, fig6, height = 5.8, width = 5.2)
#ggsave(paste0("./plots/fig6.tiff"), units = "in", dpi = 300, fig6, height = 5.8, width = 5.2)

# Fig 7
dates <- c(as.Date("2020-11-01"), as.Date("2021-01-01"), as.Date("2021-03-01"), as.Date("2021-05-01"))
# Log Score 7
log_plot_7 <- res_df %>%
  select(date, log_a_7, log_b_7, log_d_7) %>%
  pivot_longer(starts_with("log"), names_to = "model", values_to = "log") %>%
  ggplot(aes(x = date, y = log)) +
  geom_line(aes(color = model, linetype = model)) +
  ylab("LogS") +
  xlab("Date") +
  scale_x_date(
    date_breaks = "1 month", date_labels = "%y-%m-%d",
    limits = c(as.Date("2020-10-20"), as.Date("2021-05-21")), expand = c(0.02, 0.02)
  ) +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15),
    plot.margin = margin(0.1, 0.1, 0, 0, "cm")
  ) +
  scale_color_manual(
    values = c(cols[4], cols[2], wes_cols[3]),
    labels = c(log_a_7 = "R", log_d_7 = "RL(ICU)", log_b_7 = "L(ICU)")
  ) +
  scale_linetype_manual(
    values = c(1, 3, 2),
    labels = c(log_a_7 = "R", log_d_7 = "RL(ICU)", log_b_7 = "L(ICU)")
  )

log_plot_7


crps_plot_7 <- res_df %>%
  select(date, crps_a_7, crps_b_7, crps_d_7) %>% 
  pivot_longer(starts_with("crps"), names_to = "model", values_to = "crps") %>%
  ggplot(aes(x = date, y = crps)) +
  geom_line(aes(color = model, linetype = model)) +
  ylab("CRPS") +
  xlab("Date") +
  scale_x_date(
    date_breaks = "1 month", date_labels = "%y-%m-%d",
    limits = c(as.Date("2020-10-20"), as.Date("2021-05-21")), expand = c(0.02, 0.02)
  ) +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15),
    plot.margin = margin(0.1, 0.1, 0, 0, "cm")
  ) +
  scale_color_manual(
    values = c(cols[4], cols[2], wes_cols[3]),
    labels = c(crps_a_7 = "R", crps_d_7 = "RL(ICU)", crps_b_7 = "L(ICU)")
  ) +
  scale_linetype_manual(
    values = c(1, 3, 2),
    labels = c(crps_a_7 = "R", crps_d_7 = "RL(ICU)", crps_b_7 = "L(ICU)")
  )

crps_plot_7

fig7 <- log_plot_7 + crps_plot_7 + plot_annotation(tag_levels = c("A", "B")) +
  plot_layout(guides = "collect", ncol = 1) & theme(
  legend.position = "bottom",
  plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
  text = element_text(size = 8, family = "sans"),
  legend.box.margin = margin(-15, -15, -15, -15)
)

fig7
ggsave(paste0("./plots/fig7.png"), units = "in", dpi = 300, fig7, height = 3.5, width = 5.2)
#ggsave(paste0("./plots/fig7.tiff"), units = "in", dpi = 300, fig7, height = 3.5, width = 5.2)




# SI

# Fig S1
# Transforming (reversing) dates
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)
  name <- paste(a$name, b$name, sep = "-")
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
}

D <- 35
start <- now - D

plot_dat <- dat %>%
  group_by(death_date, rep_date) %>%
  summarise(n_true_retro = as.factor(n())) %>%
  mutate(
    delay = rep_date - death_date,
    Reported = case_when(
      death_date <= now - delay ~ "n_obs",
      death_date > now - delay ~ "unrep"
    ),
    Reported = factor(Reported, levels= c("n_obs", "unrep"), ordered = T),
    death_date = as.POSIXct(death_date)
  ) %>% 
  filter(death_date >= start, death_date <= now +1, delay <= D)
rev_date <- c_trans("reverse", "time")

p1 <- plot_dat %>% ggplot(aes(x = as.numeric(delay), y = death_date)) +
  theme_bw() +
  xlab("Days delay") +
  ylab("Date") +
  geom_tile(aes(fill = Reported)) +
  theme(panel.grid.major = element_line(color = "#eeeeee")) +
  geom_text(aes(label = n_true_retro),size = 1.5) +
  coord_cartesian(xlim = c(1, 34)) +
  scale_y_continuous(trans = rev_date, 
                     breaks = as.POSIXct(c("2020-11-25", "2020-12-02", "2020-12-09",
                                           "2020-12-16","2020-12-23","2020-12-30")),
                     labels = c("20-11-25", "20-12-02", "20-12-09",
                                "20-12-16","20-12-23","20-12-30"),
                     expand = c(0.02, 0.02))+
  scale_fill_manual(values = c(n_obs = "#51C56AFF", unrep = "#FDE725FF"),
                    labels = c("Reported", "Occurred but not yet reported")) +
  theme(
    legend.position = "none",
    text = element_text(size = 8, family = "sans")
  ) 

p2 <- dat %>% group_by(date=death_date) %>%
  summarise(n_true_retro=n()) %>%
  left_join(dat %>% filter(rep_date <= now) %>% 
              group_by(date=death_date) %>%
              summarise(n_obs=n())) %>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
  filter(date >= start, date <= now) %>%
  mutate(unrep = n_true_retro-n_obs) %>% 
  gather(Reported, n, c(n_obs,unrep)) %>% 
  mutate(Reported = factor(Reported, levels= c( "unrep", "n_obs"), ordered = T)) %>% 
  ggplot()+
  geom_col(aes(date, n, fill = Reported)) +
  xlab("Date") +
  ylab("Deaths") +
  scale_x_date(breaks = as.Date(c("2020-11-25", "2020-12-02", "2020-12-09",
                                  "2020-12-16","2020-12-23","2020-12-30")), 
               date_labels = "%y-%m-%d",
               expand = c(0.02, 0.02))+
  scale_fill_manual(values = c(n_obs="#51C56AFF",unrep = "#FDE725FF" ),
                    labels = c("Reported", "Occurred but not yet reported"))+
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.title = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-10, -10, -10, -10)
  ) 

swe_rep <- p1 + p2 + plot_annotation(tag_levels = c("A", "B")) + plot_layout( ncol =1) 

swe_rep

ggsave(paste0("./plots/figS1.png"), units = "in", dpi = 300, swe_rep, height = 5, width = 5.2)
#ggsave(paste0("./plots/figS1.tiff"), units = "in", dpi = 300, swe_rep, height = 4, width = 5.2, compression = "lzw")


# Fig S2
p_plot_df <- read_csv("./results/summarized_results_and_tables/p_plot.csv")

plot_est_a_1230 <- p_plot_df %>% 
  filter(mod == "r",
         delay %in% c(1, 3, 7, 10, 14, 21, 35)) %>%
  mutate(Delay = factor(delay,
    levels = rev(c(1, 3, 7, 10, 14, 21, 35)),
    labels = rev(c(as.character(c(1, 3, 7, 10, 14, 21)), ">21"))
  )) %>%
  filter(death_date >= "2020-11-25") %>%
  ggplot() +
  geom_ribbon(aes(death_date, ymax = cum_frac, ymin = 0, fill = Delay)) +
  geom_line(aes(death_date, y = cum_frac, group = Delay)) +
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d") +
  ylab("Estimated probability R") +
  xlab("Date") +
  scale_fill_manual(values = wes_cols[6:13]) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(),
        text = element_text(size = 8, family = "sans")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

plot_est_a_1230

plot_est_b_1230 <- p_plot_df %>% 
  filter(mod == "l",
         delay %in% c(1, 3, 7, 10, 14, 21, 35)) %>%
  mutate(Delay = factor(delay,
    levels = rev(c(1, 3, 7, 10, 14, 21, 35)),
    labels = rev(c(as.character(c(1, 3, 7, 10, 14, 21)), ">21"))
  )) %>%
  filter(death_date >= "2020-11-25") %>%
  ggplot() +
  geom_ribbon(aes(death_date, ymax = cum_frac, ymin = 0, fill = Delay)) +
  geom_line(aes(death_date, y = cum_frac, group = Delay)) +
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d") +
  ylab("Estimated probability L(ICU)") +
  xlab("Date") +
  scale_fill_manual(values = wes_cols[6:13]) +
  theme(legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(),
    text = element_text(size = 8, family = "sans")
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

plot_est_b_1230

plot_est_d_1230 <- p_plot_df %>% 
  filter(mod == "rl",
         delay %in% c(1, 3, 7, 10, 14, 21, 35)) %>%
  mutate(Delay = factor(delay,
    levels = rev(c(1, 3, 7, 10, 14, 21, 35)),
    labels = rev(c(as.character(c(1, 3, 7, 10, 14, 21)), ">21"))
  )) %>%
  filter(death_date >= "2020-11-25") %>%
  ggplot() +
  geom_ribbon(aes(death_date, ymax = cum_frac, ymin = 0, fill = Delay)) +
  geom_line(aes(death_date, y = cum_frac, group = Delay)) +
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d") +
  ylab("Estimated probability RL(ICU)") +
  xlab("Date") +
  scale_fill_manual(values = wes_cols[6:13]) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(),
    text = element_text(size = 8, family = "sans")
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

plot_emp_1230 <- p_plot_df %>% 
  filter(mod == "emp",
         delay %in% c(1, 3, 7, 10, 14, 21, 500)) %>%
  mutate(Delay = factor(delay,
    levels = rev(c(1, 3, 7, 10, 14, 21, 500)),
    labels = rev(c(as.character(c(1, 3, 7, 10, 14, 21)), ">21"))
  )) %>%
  ggplot() +
  geom_ribbon(aes(death_date, ymax = cum_frac, ymin = 0, fill = Delay)) +
  geom_line(aes(death_date, y = cum_frac, group = Delay)) +
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d") +
  ylab("Empirical probability") +
  xlab("Date") +
  scale_fill_manual(values = wes_cols[6:13]) +
  theme( # legend.background = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(),
    text = element_text(size = 8, family = "sans")
    #  legend.title = "element_blank()"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


figS2 <- plot_emp_1230 + plot_est_a_1230 +
  plot_est_b_1230 + plot_est_d_1230 +
  plot_annotation(tag_levels = c("A", "B")) +
  plot_layout(guides = "collect", ncol = 2) & 
  theme(
  legend.position = "bottom",
  plot.margin = margin(0.1, 0.2, 0.3, 0.1, "cm"),
  text = element_text(size = 8, family = "sans"),
  legend.box.margin = margin(-5, -10, -10, -10)
) 
figS2

ggsave(paste0("./plots/figS2.png"), units = "in", dpi = 300, figS2, height = 4, width = 5.2)
#ggsave(paste0("./plots/figS2.tiff"), units = "in", dpi = 300, figS1, height = 4, width = 5.2)


# Fig S3 (beta)

# Beta 0
res_beta_0 <- read_csv("./results/summarized_results_and_tables/results_beta_0.csv") %>% filter(date >= "2020-10-15", date <= "2021-05-14")
beta_0_plot <- res_beta_0 %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_b, color = "L(ICU)")) +
  geom_ribbon(aes(date, ymin = q5_b, ymax = q95_b), fill = wes_cols[12], alpha = .2) +
  ylab(expression(beta[0])) +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d", expand = c(0.02, 0.02)) +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15)
  ) +
  scale_color_manual(values = c("L(ICU)" = wes_cols[12]))
beta_0_plot

# Beta 1 mod b
res_beta_1 <- read_csv("./results/summarized_results_and_tables/results_beta_1_mod_b.csv") %>% filter(date >= "2020-10-15", date <= "2021-05-14")
beta_1_b_plot <- res_beta_1 %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_b, color = "L(ICU)")) +
  geom_ribbon(aes(date, ymin = q5_b, ymax = q95_b), fill = wes_cols[13], alpha = .2) +
  ylab(expression(beta[1])) +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d", expand = c(0.02, 0.02)) +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15)
  ) +
  scale_color_manual(values = c("L(ICU)" = wes_cols[13]))
beta_1_b_plot


# Beta 1 mod b
res_beta_1_d <- read_csv("./results/summarized_results_and_tables/results_beta_1_mod_d.csv") %>% filter(date >= "2020-10-15", date <= "2021-05-14")
beta_1_d_plot <- res_beta_1_d %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_d, color = "RL(ICU)")) +
  geom_ribbon(aes(date, ymin = q5_d, ymax = q95_d), fill = wes_cols[13], alpha = .2) +
  ylab(expression(beta[1])) +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels = "%y-%m-%d", expand = c(0.02, 0.02)) +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15)
  ) +
  scale_color_manual(values = c("L(ICU)" = wes_cols[13]))
beta_1_d_plot


figS3 <- beta_0_plot + beta_1_b_plot + beta_1_d_plot +
  plot_annotation( "A", "B", "C") +
  plot_layout( ncol = 1)
figS3
ggsave(paste0("./plots/figS3.png"), units = "in", dpi = 300, figS3, height = 3.9, width = 5.2)
#ggsave(paste0("./plots/figS3.tiff"), units = "in", dpi = 300, figS3, height = 3.9, width = 5.2)



res_beta_1_d <- read_csv("./results/summarized_results_and_tables/results_beta_1_mod_d.csv") %>% filter(date >= "2020-10-15", date <= "2021-05-15")
figS4 <- res_beta_1_d %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_d, color = "RL(ICU)")) +
  geom_ribbon(aes(date, ymin = q5_d, ymax = q95_d), fill = wes_cols[3], alpha = .2) +
  ylab(expression(beta[1])) +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", expand = c(0.02, 0.02)) +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15)
  ) +
  scale_color_manual(values = c("RL(ICU)" = wes_cols[3]))
figS4

ggsave("./plots/figS4.png", units = "in", dpi = 300, figS4, height = 2, width = 5.2)
#ggsave("./plots/figS4.tiff", units = "in", dpi = 300, figS4, height = 2, width = 5.2)


# S4 Plots of alternative leading indcators
rep_plot_dc <- res_df %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_d_c, color = "RL(Cases)")) +
  geom_line(aes(y = n_true_retro, color = "True number"), lty = 2) +
  geom_ribbon(aes(date, ymin = q5_d_c, ymax = q95_d_c), fill = wes_cols[16], alpha = .2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 250)) +
  scale_x_date(
    date_breaks = "1 month", date_labels = "%y-%m-%d",
    limits = c(as.Date("2020-10-20"), as.Date("2021-05-21")), expand = c(0.02, 0.02)
  ) +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15)
  ) +
  scale_color_manual(
    values = c(wes_cols[16], cols[1]),
    labels = c("RL(Cases)", "True number")
  ) +
  scale_linetype_manual(
    values = c(1, 2),
    labels = c("RL(Cases)", "True number")
  )
rep_plot_dc

rep_plot_dic <- res_df %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = med_e, color = "RL(ICU, Cases)")) +
  geom_line(aes(y = n_true_retro, color = "True number"), lty = 2) +
  geom_ribbon(aes(date, ymin = q5_e, ymax = q95_e), fill = wes_cols[17], alpha = .2) +
  ylab("Number Fatalities") +
  xlab("Date") +
  coord_cartesian(ylim = c(0, 250)) +
  scale_x_date(
    date_breaks = "1 month", date_labels = "%y-%m-%d",
    limits = c(as.Date("2020-10-20"), as.Date("2021-05-21")), expand = c(0.02, 0.02)
  ) +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15)
  ) +
  scale_color_manual(
    values = c(wes_cols[17], cols[1]),
    labels = c("RL(ICU, Cases)", "True number")
  ) +
  scale_linetype_manual(
    values = c(1, 2),
    labels = c("RL(ICU, Cases)", "True number")
  )
rep_plot_dic

figS4 <- rep_plot_dc + rep_plot_dic +
  plot_annotation(tag_levels = "A") +
  plot_layout( ncol = 1)
figS4
ggsave(paste0("./plots/figS4.png"), units = "in", dpi = 300, figS2, height = 3.9, width = 5.2)
#ggsave(paste0("./plots/figS4.tiff"), units = "in", dpi = 300, figS2, height = 3.9, width = 5.2)


# FigS1 RMSE

# RMSE 7days

rmse7_plot <- res_df %>%
  select(date, err_a_7, err_d_7, err_b_7) %>%
  pivot_longer(starts_with("err"), names_to = "model", values_to = "rmse") %>%
  ggplot(aes(x = date, y = rmse)) +
  geom_line(aes(color = model, linetype = model)) +
  ylab("RMSE") +
  xlab("Date") +
  scale_x_date(breaks = dates, date_labels = "%y-%m-%d", expand = c(0.02, 0.02)) +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 8, family = "sans"),
    legend.box.margin = margin(-15, -15, -15, -15),
    plot.margin = margin(0.1, 0.1, 0.5, 0, "cm")
  ) +
  scale_color_manual(
    values = c(cols[4], cols[2], wes_cols[3]),
    labels = c(err_a_7 = "R", err_d_7 = "RL(ICU)", err_b_7 = "L(ICU)")
  ) +
  scale_linetype_manual(
    values = c(1, 3, 2),
    labels = c(err_a_7 = "R", err_d_7 = "RL(ICU)", err_b_7 = "L(ICU)")
  )

rmse7_plot

ggsave(paste0("./plots/S1_fig.png"), units = "in", dpi = 300, rmse7_plot, height = 1.75, width = 5.2)
#ggsave(paste0("./plots/S1_fig.tiff"), units = "in", dpi = 300, rmse7_plot, height = 1.75, width = 5.2)






