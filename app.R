library(shiny)
library(dplyr)
library(ggplot2)
library(googlesheets)
library(lubridate)
library(plotly)
library(tsibble)

#shiny_token <- gs_auth() 
#saveRDS(shiny_token, "shiny_app_token.rds")
gs_auth(token = "shiny_app_token.rds")
sheet_key <- "1lVvc2_SD_JdwNLcrNgTfDg0n7w-sc9nSBhE_fDn5_IM"
ss <- gs_key(sheet_key)

sheet_data <- gs_read(ss) %>%
  filter(!is.na(last_30days)) %>%
  mutate(
    date = dmy(date),
    id = 1,
    exersice = as.numeric(!is.na(last_30days))
  )

ui <- fluidPage(
  titlePanel("Fitness Progress"),
  plotOutput("plot")
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    sheet_data %>%
      as_tsibble(
        index = date,
        key = id(id)    
      ) %>%
      fill_gaps(.full = TRUE) %>%
      mutate(
        last_month_mean = slide_dbl(
          exersice, sum, .size = 31, na.rm = TRUE),
        last_year_mean = slide_dbl(
          exersice, sum, .size = 365, na.rm = TRUE) / 12,
      ) %>%
      ggplot() +
      geom_step(aes(date, last_month_mean), color = "green") +
      geom_step(aes(date, last_year_mean), color = "blue") +
      scale_x_date(date_breaks = "10 days", date_labels =  "%d %b", 
                   limits = c(today() - months(4), NA)) +
      scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = seq(0, 20, 1),
                         position = "right") + 
      theme_bw() +
      labs(title = "Last month actual and yearly average",
           y = "Times per Month")
  })
}

shinyApp(ui, server)
