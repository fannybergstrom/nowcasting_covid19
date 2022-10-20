# This document is for computing the elements of the reporting triangle of COVID-19 deaths
# reported by the Swedish Public Health Agency Folkhälsomyndigheten (fohm)

# Libraries
# if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, data.table, lubridate, readxl, readr, zoo, splitstackshape, stringr)

# Get file names from fohm
fohm_files <- list.files("./data/fohm/")


DT <- data.table(
  read_excel(path = 
              str_c("./data/fohm/", fohm_files)[100], 
                          sheet = 2, col_types = c("text", "numeric")) %>% 
    select(date = Datum_avliden, n = Antal_avlidna) %>%
    filter(date != "Uppgift saknas")%>% 
    mutate(date = as.dar)
) 


death_dt <- data.table(death_dts)
setkey(death_dt, publication_date, date)

first_pub_date <- death_dt[, min(publication_date, na.rm = TRUE)]
death_dt[!is.na(date) & publication_date > first_pub_date, days_since_publication := publication_date - date]
death_dt[date == first_pub_date & publication_date == first_pub_date, days_since_publication := 0]

death_dt[!is.na(date), paste0("n_m", 1) := shift(N, n = 1, type = "lag", fill = 0L), by = date]
death_dt[!is.na(date), n_diff := N - n_m1]
death_dt[!is.na(date) & n_m1 > 0 & !is.na(n_m1), n_diff_pct := N/n_m1 - 1]
death_dt[!is.na(date) & n_m1 == 0 & N == 0, n_diff_pct := 0]
death_dt[, n_m1 := NULL]

# Categorize by grouped days since publication.
# 1, 2, ... > 1 week, 2 weeks
death_dt[, delay := as.numeric(days_since_publication)]
death_dt[delay >= 14, delay := 14]
death_dt[delay >= 7 & delay < 14, delay := 7]
death_dt[is.na(delay), delay := -1]
death_dt[, delay := factor(delay,
                           levels = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 14),
                           labels = c("No Data", "Same day", "1 Day", "2 Days",
                                      "3-4 Days", "3-4 Days", "5-6 Days",
                                      "5-6 Days", "7-13 Days", "14 Days +"))]



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
df <- read_csv("./data/covid_icu_latest.csv") %>%
filter(date >= as_date("2020-10-20"), 
       date <= as_date("2021-05-21"),
       n_diff > 0) %>%
  expandRows(., "n_diff") 

df$days_since_publication %>% median()

df %>% filter(days_since_publication > 14) %>% summarise(n()) /df %>% summarise(n())


rename.files("./data/fohm/, pattern, replacement)

