# Load packages
library(tidyverse)
library(data.table)
library(cmdstanr)
library(posterior)
library(lubridate)
library(readxl)
library(zoo)
library(splitstackshape)

# Import data, rename and save into line list format
read_csv("./data/covid_deaths_latest.csv") %>%
  filter(date > as_date("2020-04-01"), n_diff > 0) %>%
  mutate(rep_date = publication_date, death_date = date) %>%
  select(death_date, n_diff, rep_date) %>%
  expandRows(., "n_diff") %>%
  mutate(rep_date_wd = wday(rep_date, label = F)) %>%
  filter(rep_date>=ymd("2020-05-01")) %>%  
  write_csv("./data/covid_deaths.csv")


# ICU
df_icu <- read_csv("./data/covid_icu_latest.csv") %>%
filter(date >= as_date("2020-10-20"), 
       date <= as_date("2021-05-21"),
       n_diff > 0) %>%
  expandRows(., "n_diff") 

df_icu$days_since_publication %>% median()
