##### Timeseries models in R
library(fpp3)
library(tidyverse)
library(zoo)
library(readxl)
library(wesanderson)
select <- dplyr::select
setwd("/media/fabe4028/suhome/Documents/Forskning/Nowcasting/nowcasting_covid19/code")
# Data from nowcasting
#nc_res <- read_csv("../data/nc_res.csv")  
# FHM

FHM_ICU <- read_excel("../data/FHM/FHM_latest.xlsx", 
                      sheet = "Antal intensivvårdade per dag")

FHM_deaths <- read_excel("../data/FHM/FHM_latest.xlsx", 
                      sheet = "Antal avlidna per dag", col_types = c("date", "numeric"))

FHM_cases <- read_excel("../data/FHM/FHM_latest.xlsx", 
                        sheet = "Antal per dag region")


#remove final row
FHM_deaths <- FHM_deaths[-nrow(FHM_deaths),]

# Save dates 
now <- ymd("2020-12-31")
start <- now - 180

#start <- ymd("2020-10-15")
#now <- start+120

# Create time series data frame
ts <- FHM_ICU %>%
  select(date = Datum_vårdstart, n_icu = Antal_intensivvårdade) %>%
  left_join(FHM_cases %>% select(date = Statistikdatum, n_cases = Totalt_antal_fall)) %>%
  left_join(FHM_deaths %>% select(date = Datum_avliden, n_deaths = Antal_avlidna)) %>% 
  mutate(
    n_deaths_lag = lag(n_deaths, 1, fill = NA),
    mean_7_c = rollmean(n_cases, k = 7, fill = NA, align = "center"),
    mean_7_c_lag = lag(mean_7_c, 7, fill = NA),
    lead_ind_cases = log(lag(mean_7_c, 19, fill = NA)),
    mean_7_i = rollmean(n_icu, 7, fill = NA, align = "center"),
    mean_7_i_lag = lag(mean_7_i, 7, fill = NA),
    lead_ind_icu = log(lag(mean_7_i, 14, fill = NA)),
    lead_ind_icu_f = lag(mean_7_i-mean_7_i_lag, 1, fill = NA),
    lead_ind_icu_d = lag(log(mean_7_i)-log(mean_7_i_lag), 3, fill = NA),
    ratio_i = lag(mean_7_i / mean_7_i_lag, 7),
    ratio_c = log(lag(mean_7_c / mean_7_c_lag, 7)))


res3 <- lm(log(n_deaths+1) ~ -1 + log(n_deaths_lag+1), 
           data = ts %>% filter(date > "2020-09-01", date < "2020-12-15")) %>% summary()
res3$adj.r.squared


res3 <- lm(log(n_deaths+1) ~ -1 + log(n_deaths_lag+1) + lead_ind_icu_f, 
           data = ts %>% filter(date > "2020-09-01", date < "2020-12-15")) %>% summary()
res3$adj.r.squared

res3 <- lm(log(n_deaths+1) ~ -1 + log(n_deaths_lag+1) + lead_ind_icu_d, 
           data = ts %>% filter(date > "2020-09-01", date < "2020-12-15")) %>% summary()
res3$adj.r.squared

res3 <- lm(log(n_deaths) ~ -1 + log(n_deaths_lag), 
           data = ts %>% filter(date > "2020-04-01", date < "2021-03-01")) %>% summary()
res3$adj.r.squared


res4 <- lm(log(n_deaths+1) ~ -1 + log(n_deaths_lag1+1) + ratio_c +ratio_i, 
           data = ts %>% filter(date > "2020-04-01", date < "2021-03-01")) %>% summary()
res4$adj.r.squared



lm(n_deaths ~ -1 + ratio_c, data = ts, offset = n_deaths_lag1) %>% summary()

res <- c()
for(i in 1:40){
  ts_i <- ts %>% select(date, n_deaths, n_deaths_lag , lead_ind_icu_d) %>% 
  mutate(ratio_c = lag(lead_ind_icu_d, i)) %>%
    filter(date > "2020-09-01", date < "2020-12-15")
 res[i]<- summary(lm(log(n_deaths+1) ~ -1 + log(n_deaths_lag+1) + ratio_c, data = ts_i))$adj.r.squared
} 
which.max(res)
resf <- res
res <- c()
for(i in 1:30){
  ts_i <- ts %>% select(date, n_deaths, n_deaths_lag1 , ratio_i) %>% 
    mutate(ratio_i = lag(ratio_i, i)) %>%
    filter(date > "2020-04-01", date < "2021-03-01")
  res[i]<- summary(lm(log(n_deaths+1) ~ -1 + log(n_deaths_lag1+1) + ratio_i, data = ts_i))$adj.r.squared
} 
which.max(res)

res <- c()
for(i in 1:30){
  ts_i <- ts %>% select(date, n_deaths, n_deaths_lag1 , avg_7_i_lag) %>% 
    mutate(avg_7_i = lag(avg_7_i_lag, i)) %>%
    filter(date > "2020-04-01", date < "2021-03-01")
  res[i]<- summary(lm(log(n_deaths+1) ~ avg_7_i, data = ts_i))$r.squared
} 
which.max(res)

for(i in 1:30){
  ts_i <- ts %>% select(date, n_deaths, n_deaths_lag1 , avg_7_i) %>% 
    mutate(avg_7_i = lag(avg_7_i, i)) %>%
    filter(date > "2020-04-01", date < "2021-03-01")
  res[i]<- summary(lm(log(n_deaths+1) ~ log(avg_7_i), data = ts_i))$r.squared
} 
which.max(res)

for(i in 1:30){
  ts_i <- ts %>% select(date, n_deaths, n_deaths_lag1 , avg_7_i) %>% 
    mutate(avg_7_i = lag(avg_7_i, i)) %>%
    filter(date > "2020-04-01", date < "2021-03-01")
  res[i]<- summary(lm(log(n_deaths+1) ~ log(avg_7_i), data = ts_i))$r.squared
} 
which.max(res)

for(i in 1:30){
  ts_i <- ts %>% select(date, n_deaths , avg_7_c) %>% 
    mutate(avg_7_c = lag(avg_7_c, i)) %>%
    filter(date > "2020-04-01", date < "2021-03-01")
  res[i]<- summary(lm(log(n_deaths+1) ~ log(avg_7_c), data = ts_i))$r.squared
} 
which.max(res)


#Ratio
ts  %>% 
  filter(date > "2020-04-01", date < "2021-12-15") %>% 
  ggplot(aes(x=date)) +
  geom_line(aes(y=sum_7_d/max(sum_7_d),col = "Deaths"))+
  geom_line(aes(y=ratio_c, col = "Cases"))+
  geom_line(aes(y=ratio_i, col = "ICU"))
  #geom_line(aes(y=diff_c/(max(diff_c)), col = "e"))


ts %>% filter(date < "2022-01-01")  %>% pivot_longer(n_deaths:n_cases) %>% ggplot(aes(x=date, y= scale(value), col = name)) + geom_line()

ts  %>% 
  filter(date > "2020-08-01", date < "2021-12-15") %>% 
  ggplot(aes(x=as.Date(date))) +
  geom_rect(aes(xmin=as.Date('2020-09-01'),
                xmax = as.Date('2020-09-15'),
                ymin = -Inf,
                ymax = Inf), fill = "grey", alpha = 0.05)+
  geom_rect(aes(xmin=as.Date('2020-11-01'),
                xmax = as.Date('2020-11-15'),
                ymin = -Inf,
                ymax = Inf), fill = "grey", alpha = 0.05)+
  geom_rect(aes(xmin=as.Date('2021-03-01'),
                xmax = as.Date('2021-03-15'),
                ymin = -Inf,
                ymax = Inf), fill = "grey", alpha = 0.05)+
  geom_rect(aes(xmin=as.Date('2021-05-01'),
                xmax = as.Date('2021-05-15'),
                ymin = -Inf,
                ymax = Inf), fill = "grey", alpha = 0.05)+
  geom_rect(aes(xmin=as.Date('2021-09-01'),
                xmax = as.Date('2021-09-15'),
                ymin = -Inf,
                ymax = Inf), fill = "grey", alpha = 0.05)+
  geom_line(aes(y=sum_7_c/max(sum_7_c), col = "Cases"))+
  geom_line(aes(y=sum_7_h/max(sum_7_h), col = "Hospitalisations"))+
  geom_line(aes(y=sum_7_i/max(sum_7_i), col = "ICU"))+
  geom_line(aes(y=sum_7_d/max(sum_7_d),col = "Deaths")) +
  scale_x_date(date_breaks = "2 month", date_labels = "%y-%m-%d") +
  ylab("Number (scaled) 3 w rolling avg") +
  xlab("Date")

ggsave(paste0("../evaluation/plots/ts_scaled.png"), last_plot(), width = 9,
       height = 4)


ts  %>% 
  filter(date > "2020-08-01", date < "2021-12-15") %>% 
  ggplot(aes(x=date)) +
  #geom_line(aes(y=sum_7_c, col = "Cases"))+
  geom_line(aes(y=sum_7_h, col = "Hospitalisations"))+
  geom_line(aes(y=sum_7_i, col = "ICU"))+
  geom_line(aes(y=sum_7_d,col = "Deaths"))+
  ylab("Number 3 w rolling avg")

ggsave(paste0("../evaluation/plots/ts.png"), last_plot(), width = 9,
       height = 4)
ts  %>% 
  filter(date > "2020-08-01", date < "2021-12-15") %>% 
  ggplot(aes(x=date)) +
  geom_line(aes(y=sum_7_c, col = "Cases"))+
  geom_line(aes(y=sum_7_h, col = "Hospitalisations"))+
  geom_line(aes(y=sum_7_i, col = "ICU"))+
  geom_line(aes(y=sum_7_d,col = "Deaths"))

ts  %>% 
  filter(date > "2020-08-01", date < "2021-12-15") %>% 
  ggplot(aes(x=date)) +
  #geom_line(aes(y=sum_7_c, col = "Cases"))+
  #geom_line(aes(y=sum_7_h, col = "Hospitalisations"))+
  geom_line(aes(y=sum_7_i, col = "ICU"))+
  geom_line(aes(y=sum_7_d,col = "Deaths"))

icu_ts <- FHM_ICU %>% mutate(sum_7 = rollsum(Antal_intensivvårdade, k = 7, fill = NA, align = 'right'),
                            diff_sum7 = as.numeric(diff(as.zoo(sum_7), lag= 7, na.pad=T)),
                            sum_7_log = log(rollsum(Antal_intensivvårdade, k = 7, fill = NA, align = 'right')),
                            diff_sum7_log = as.numeric(diff(as.zoo(sum_7_log), lag= 7, na.pad=T)),
                            sum_14 = rollsum(Antal_intensivvårdade, k = 14, fill = NA, align = 'right'),
                            diff_sum14 = as.numeric(diff(as.zoo(sum_14), lag= 14, na.pad=T)),
                            sum_14_log = log(rollsum(Antal_intensivvårdade, k = 14, fill = NA, align = 'right')),
                            diff_sum14_log = as.numeric(diff(as.zoo(sum_14_log), lag= 7, na.pad=T)),
                            sum_21 = rollsum(Antal_intensivvårdade, k = 21, fill = NA, align = 'right'),
                            diff_sum21 = as.numeric(diff(as.zoo(sum_21), lag= 21, na.pad=T)),
                            sum_21_log = log(rollsum(Antal_intensivvårdade, k = 21, fill = NA, align = 'right')),
                            diff_sum21_log = as.numeric(diff(as.zoo(sum_21_log), lag= 21, na.pad=T)))

icu <-icu_ts %>% filter(Datum_vårdstart <= now-5,
                  Datum_vårdstart >= start-5) %>% 
                select(-Datum_vårdstart)

df_ts <- cbind(ts, icu) 

# Plot time series 
df_ts %>% 
  dplyr::select(date, log_lambda_t, Antal_intensivvårdade) %>% 
  mutate(Antal_intensivvårdade = log(Antal_intensivvårdade)) %>% 
  pivot_longer(-date) %>% 
  ggplot() + 
  geom_line(aes(date, value, color = name)) +
  theme_bw() +
  theme_bw() + ylab("Fatalities") +
  theme(legend.position = "none") 


# Model
m1 <- lm(log_lambda_t ~ -1 + diff_sum7, data = df_ts, offset = log_lambda_tm1) %>% summary()
m1

# Looping over different lags
n_lags <- 25
pvals7 <- c()
pvals7log <- c()
pvals14 <- c()
pvals14log <- c()
pvals21 <- c()

pvals_7 <- c()
pvals_14 <- c()

for(i in 1:n_lags){
  icu <- icu_ts %>% 
            filter(Datum_vårdstart <= now-i,
                   Datum_vårdstart >= start-i) %>% 
            select(-Datum_vårdstart)  %>% 
    scale()
  
  df <- cbind(ts, icu) 
  
  pvals7[i] <- summary(lm(log_lambda_t ~ -1 + diff_sum7, data = df, offset = log_lambda_tm1))$coefficients[4]
  pvals7log[i] <- summary(lm(log_lambda_t ~ -1 + diff_sum7_log, data = df, offset = log_lambda_tm1))$coefficients[4]
  pvals14[i] <- summary(lm(log_lambda_t ~ -1 + diff_sum14, data = df, offset = log_lambda_tm1))$coefficients[4]
  pvals21[i] <- summary(lm(log_lambda_t ~ -1 + diff_sum21, data = df, offset = log_lambda_tm1))$coefficients[4]
  #without rw
  pvals_7[i] <- summary(lm(log_lambda_t ~ sum_7_log, data = df))$coefficients[2,4]
  pvals_14[i] <- summary(lm(log_lambda_t ~ sum_14_log, data = df))$coefficients[2,4]
}

# Plot results
bind_cols(pvals07 = pvals7, pvals14 = pvals14, pvals21 = pvals21, lag =1:n_lags)  %>% pivot_longer(-lag) %>% 
  ggplot() + 
  geom_point(aes(lag, value, color = name)) +
  facet_wrap(vars(name), nrow = 3) +
  theme_bw() 

plot(log(pvals_7))
plot(log(pvals_14))

#Pick the smallest p-value
min_lag <- which.min(pvals_14)
min_lag <-7
icu <- icu_ts %>% filter(Datum_vårdstart <= now-min_lag,
                         Datum_vårdstart >= start-min_lag) %>% 
  select(-Datum_vårdstart)

df_ts <- cbind(ts, icu) 

res_minpval <- summary(lm(log_lambda_t ~ -1 + diff_sum7, data = df_ts, offset = log_lambda_tm1))
res_minpval

# Plot of differences
df_ts %>%
  ggplot() + 
  geom_line(aes(date, scale(log_lambda_t-log_lambda_tm1), color = "Lambda_t")) +
  geom_line(aes(date, scale(diff_sum14), color = "ICU diff"))+
  theme_bw() 

# Plot lambda vs new ICU intakes
df_ts %>%
  ggplot() + 
  geom_line(aes(date, scale(log_lambda_t), color = "Lambda_t")) +
  geom_line(aes(date, scale(log(Antal_intensivvårdade)), color = "ICU "))+
  theme_bw() 

# 
df_ts %>%
  ggplot() + 
  geom_line(aes(date, scale(log_lambda_t), color = "Lambda_t")) +
  geom_line(aes(date, scale(log(Antal_intensivvårdade)), color = "ICU "))+
  theme_bw() 


