library(tidyverse)
library(googlesheets)
library(lubridate)
library(zoo)
library(plotly)

#shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
#saveRDS(shiny_token, "shiny_app_token.rds")

googlesheets::gs_auth(token = "shiny_app_token.rds")
sheet_key <- "1lVvc2_SD_JdwNLcrNgTfDg0n7w-sc9nSBhE_fDn5_IM"
ss <- gs_key(sheet_key)

sheet_data <- gs_read(ss) %>%
  filter(!is.na(last_30days)) %>%
  mutate(date = dmy(date),
         exersice = 1)

start_date = min(sheet_data$date)
n_days <- abs(today() %--% (start_date - years(1)) / days(1))

plot <- tibble(
  date = today() - days(1:n_days)
) %>%
  merge(sheet_data, by = "date", all.x = T) %>%
  mutate(
    exersice = if_else(is.na(exersice), 0, exersice),
    last_month_mean = rollmean(exersice, 30, fill = NA, align = "right") * 30,
    last_year_mean = rollmean(exersice, 365, fill = NA, align = "right") * 30
  ) %>%
  filter(date >= start_date) %>%
  ggplot() +
  geom_line(aes(date, last_month_mean), color = "green") +
  geom_line(aes(date, last_year_mean), color = "blue") +
  labs(title = "Monthly average per year and month",
       y = "Average")
  
ggplotly(plot)
