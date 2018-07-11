library(tidyverse)
library(googlesheets)
library(lubridate)
library(zoo)
library(TTR)

#shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
#saveRDS(shiny_token, "shiny_app_token.rds")

googlesheets::gs_auth(token = "shiny_app_token.rds")
sheet_key <- "1lVvc2_SD_JdwNLcrNgTfDg0n7w-sc9nSBhE_fDn5_IM"
ss <- gs_key(sheet_key)

sheet_data <- gs_read(ss) %>%
  filter(!is.na(last_30days)) %>%
  mutate(date = dmy(date),
         exersice = 1)

year_dates <- tibble(
  date = today() - days(1:365)
)

combined <- year_dates %>%
  merge(sheet_data, by = "date", all.x = T) %>%
  mutate(
    exersice = if_else(is.na(exersice), 0, exersice),
    last_30_calc = c(rep(0, 29), rollmean(exersice, 30)),
    last_7_calc = c(rep(0, 6), rollmean(exersice, 7)),
    last_365_calc = mean(exersice)
  )

ggplot(combined) +
  geom_line(aes(date, last_7_calc * 30), color = "red") +
  geom_line(aes(date, last_30_calc * 30), color = "green") +
  geom_line(aes(date, last_365_calc * 30), color = "blue")
  
