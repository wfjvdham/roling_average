library(shiny)
library(tidyverse)
library(googlesheets)
library(lubridate)
library(zoo)
library(plotly)

googlesheets::gs_auth(token = "shiny_app_token.rds")
sheet_key <- "1lVvc2_SD_JdwNLcrNgTfDg0n7w-sc9nSBhE_fDn5_IM"
ss <- gs_key(sheet_key)

sheet_data <- gs_read(ss) %>%
  filter(!is.na(last_30days)) %>%
  mutate(date = dmy(date))

ui <- fluidPage(
  titlePanel("Fitness Progress"),
  plotOutput("plot")
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    tibble(
      date = seq(min(sheet_data$date) - years(1), today(), by = "1 day")
    ) %>%
      merge(sheet_data, by = "date", all.x = T) %>%
      mutate(
        exersice = as.numeric(!is.na(last_30days)),
        last_month_mean = rollmean(exersice, 30, fill = NA, align = "right") * 30,
        last_year_mean = rollmean(exersice, 365, fill = NA, align = "right") * 30
      ) %>%
      filter(date >= start_date) %>%
      ggplot() +
      geom_step(aes(date, last_month_mean), color = "green") +
      geom_step(aes(date, last_year_mean), color = "blue") +
      scale_x_date(date_breaks = "10 days", date_labels =  "%d %b", 
                   limits = c(today() - months(4), NA)) +
      scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = seq(0, 20, 1),
                         position = "right") + 
      theme_bw() +
      labs(title = "Monthly average per year and month",
           y = "Average")
  })
}

shinyApp(ui, server)
