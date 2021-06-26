# make data
require(tidyverse)

url <-
  "https://raw.githubusercontent.com/opencovid-mr/covid-19_sorveglianza_integrata_italia/main/data/"
fname <- "sesso_eta.csv"

gotosaturday <- function(date){
  offset = 1 - lubridate::wday(date, week_start = 7)
  return(date + offset)
}

ddata <- tibble()
for (i in format(seq(as.Date("2021-01-01"),
                     Sys.Date(),
                     by = 1), "%Y-%m-%d")) {
  t <- tryCatch(
    readr::read_csv(paste0(url, i, "/", fname)),
    error = function(e) {
      tibble()
    }
  )
  ddata <- union_all(ddata, t)
}


data1 <- ddata %>% 
  select(date = iss_date, age_group = AGE_GROUP, deaths = DECEDUTI) %>%
  filter(age_group != "Non noto") %>% 
  filter(deaths != "<5") %>% 
  group_by(date, age_group) %>% 
  mutate(deaths = as.integer(deaths)) %>% 
  summarise(tdeaths = sum(deaths)) %>%
  ungroup() %>% 
  mutate(date = as.Date(date, "%d/%m/%Y")) %>% 
  arrange(age_group, date) %>% 
  mutate(daily_deaths = as.double(tdeaths - lag(tdeaths))) %>% 
  mutate(week = gotosaturday(date)) %>% 
  mutate(daily_deaths = if_else(daily_deaths < 0, 0, daily_deaths)) %>% 
  arrange(week, age_group) %>% 
  group_by(week, age_group) %>% 
  summarise(total_deaths = sum(daily_deaths)) %>% 
  select(week = week, 
         age_range = age_group,
         total_deaths = total_deaths)

write_csv(data1, "./data/data1.csv")
