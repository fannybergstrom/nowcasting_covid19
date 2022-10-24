# Evaluation of the nowcasting models

# Import functions
source("./code/2_functions.r")
# Import data
dat <- read_csv("./data/covid_deaths.csv")

# Restrict dataset to a specific nowcast dates
rep_dates <- list.files(path = paste0("./data/fohm/")) %>% 
  str_extract("\\d+-\\d+-\\d+") %>%  
  as.data.frame %>% 
  distinct() %>% 
  filter(. >= "2020-10-20", . <= "2021-05-21") %>%  
  t() %>%
  as.vector()

# Choose one of following models
models <- c("mod_r", "mod_r_cases", "mod_l", "mod_l_cases", 
            "mod_l_2", "mod_rl", "mod_rl_cases", "mod_rl_2")

for(i in c(100)){
  now <- rep_dates[i]
  model_spec <- "mod_r"
  lapply(model_spec , evaluate_nowcast, dat = dat, now = now)
}
