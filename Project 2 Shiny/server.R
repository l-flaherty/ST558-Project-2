library(shiny)
library(tidyverse)

function(input, output) {
  filtered_data <- reactive({
    user_date <- as.Date(input$date_selector)

    df <- read.csv("gold.csv")
    closest_date <- df |>
      filter(!is.na(date)) |>
      slice(which.min(abs(date - user_date))) |>
      pull(date)
    
    df_filtered <- df |>
      filter(date == closest_date)
    
    return(df_filtered)
  })
  
  output$plot1 <- renderPlot({
    troy <- filtered_data()
    ggplot(troy, aes(x = location, y = qty)) +
      geom_bar(stat = "identity", fill = "darkgoldenrod1") +
      labs(x = "Location", y = "Quantity", title = "Gold Holdings by Location")
  })
  
  output$plot2 <- renderPlot({
    troy <- filtered_data()
    ggplot(troy, aes(x = location, y = qty)) +
      geom_point(size = 3, color = "blue") +
      labs(x = "Location", y = "Quantity", title = "Scatter Plot of Gold Holdings")
  })
}