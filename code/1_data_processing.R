#### This document is for computing the elements of the reporting triangle of COVID-19 deaths
#### reported by the Swedish Public Health Agency Folkhälsomyndigheten (fohm)

# Libraries
# if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, data.table, lubridate, readxl, readr, zoo, splitstackshape, stringr
)

# File path
path <- "./data/fohm/"

# Get file names from fohm
fohm_files <- str_c(path, list.files(path))

# Get rep dates
rep_dates <- fohm_files %>%
  str_extract("\\d+-\\d+-\\d+") %>%
  as.data.frame() %>%
  distinct() %>%
  # filter(. >= "2020-10-20", . <= "2021-05-21") %>%
  t() %>%
  as.vector()

names(fohm_files) <- rep_dates

# Import death data
covid_death_df <- lapply(fohm_files, read_excel,
  sheet = 2, col_types = c("text", "numeric")
) %>%
  bind_rows(.id = "rep_date") %>%
  select(death_date = Datum_avliden, N = Antal_avlidna, rep_date) %>%
  filter(death_date != "Uppgift saknas") %>%
  mutate(
    death_date = as.Date(as.numeric(death_date), origin = "1899-12-30"),
    rep_date = as.Date(rep_date)
  )

# Compute deaths n_t,d's
covid_death_df %>%
  group_by(death_date) %>%
  mutate(n = c(first(N), diff(N))) %>%
  filter( # date > as_date("2020-05-01"),
    n > 0
  ) %>%
  select(death_date, n, rep_date) %>%
  write_csv("./data/covid_deaths.csv")

# Import ICU data
covid_icu_df <- lapply(fohm_files, read_excel,
  sheet = 3, col_types = c("text", "numeric")
) %>%
  bind_rows(.id = "rep_date") %>%
  select(icu_date = Datum_vårdstart, N = Antal_intensivvårdade, rep_date) %>%
  mutate(
    icu_date = as.Date(as.numeric(icu_date), origin = "1899-12-30"),
    rep_date = as.Date(rep_date)
  )

# Compute ICU n_{t,d}'s
covid_icu_df %>%
  group_by(icu_date) %>%
  mutate(n = c(first(N), diff(N))) %>%
  filter( # date > as_date("2020-05-01"),
    n > 0
  ) %>%
  select(icu_date, n, rep_date) %>%
  write_csv("./data/covid_icu.csv")
