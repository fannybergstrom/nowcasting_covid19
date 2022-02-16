# Load packages
library(tidyverse)
library(data.table)
library(cmdstanr)
library(posterior)
library(readxl)
library(zoo)
library(splitstackshape)

# Import data, rename and save into line list format
read_csv("../data/covid_deaths_latest.csv") %>% 
  filter(date > as_date("2020-04-01"), n_diff > 0) %>%
  mutate(rep_date = publication_date, death_date = date) %>%
  select(death_date, n_diff, rep_date) %>%
  expandRows(., "n_diff") %>%
  mutate(rep_date_wd = wday(rep_date, label = F)) %>%
  filter(rep_date>=ymd("2020-05-01")) %>%  
  write_csv("../data/covid_deaths.csv")


# Additional data source


# Create time series data frame
ts <- FHM_ICU %>%
  select(date = Datum_vårdstart, n_icu = Antal_intensivvårdade) %>%
  left_join(FHM_cases %>% select(date = Statistikdatum, n_cases = Totalt_antal_fall)) %>%
  mutate(
    sum_7_c = rollsum(n_cases, k = 7, fill = NA, align = "right"),
    sum_7_c_lag = lag(sum_7_c, k = 7, fill = NA),
    sum_7_i = rollsum(n_icu, k = 7, fill = NA, align = "right"),
    sum_7_i_lag = lag(sum_7_i, k = 7, fill = NA),
    diff_sum7_i = as.numeric(diff(as.zoo(sum_7_i), lag = 7, na.pad = T)),
    sum_7_log_i = log(rollsum(n_icu, k = 7, fill = NA, align = "right")),
    diff_sum7_log_i = as.numeric(diff(as.zoo(sum_7_log_i), lag = 7, na.pad = T)),
    ratio_i = log(lag(sum_7_i / sum_7_i_lag, 1)),
    ratio_c = log(lag(sum_7_c / sum_7_c_lag, 7))
  ) %>%
  filter(
    date <= now,
    date >= start
  )