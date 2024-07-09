library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

# Your plot function
oplot <- function(df, t1 = "1993-04-01", t2 = "2024-07-02") {
  a <- df |> 
    filter(date >= t1 & date <= t2) |>
    mutate(billions = debt / 1000000000)
  
  ggplot(a, aes(x = date, y = billions)) +
    geom_area(fill = "gray", alpha = 0.3) +
    geom_line(col = "red", size = 1) +
    labs(x = "Date", y = "Debt Load (Billions USD)", title = "U.S. Federal Debt") +
    ylim(0, max(a$billions)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
}

ui <- fluidPage(
  titlePanel("U.S. Federal Debt Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("end_year",
                  "Select End Year:",
                  min = year(min(o$date)),
                  max = year(max(o$date)),
                  value = year(min(o$date)),
                  step = 1,
                  animate = animationOptions(interval = 600, loop = TRUE)),
      width = 3
    ),
    
    mainPanel(
      plotOutput("debt_plot")
    )
  )
)

server <- function(input, output) {
  output$debt_plot <- renderPlot({
    start_date <- min(o$date)
    end_date <- as.Date(paste0(input$end_year, "-12-31"))
    
    oplot(o, t1 = start_date, t2 = end_date)
  })
}

shinyApp(ui = ui, server = server)