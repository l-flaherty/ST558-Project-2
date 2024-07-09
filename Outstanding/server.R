#####1. Load Required Packages#####
library("lubridate")
library("gifski")
library("datasauRus") 
library("tidyverse")
library("httr")
library("jsonlite")
library("shiny")
library("shinycssloaders")
library("gganimate")
load("outstanding.R") #So don't have to call API#





#####3. UDF: Graphing Functions#####
###3b. Outstanding###
outstanding_plot <- function(df, t1="1993-04-01", t2="2024-07-02") {
  a <- df |>
    filter(date >= as.Date(t1) & date <= as.Date(t2)) |>
    mutate(billions = debt / 1000000000)
  
  if (nrow(a) > 1) {  # Check if there's more than one row after filtering
    ggplot(a, aes(x = date, y = billions)) +
      geom_area(fill = 555, alpha = 0.3) +
      geom_line(col = "red", size = 1) +
      labs(x = "Date", y = "Debt Load (Billions USD)", title = "U.S. Federal Debt") +
      ylim(0, max(a$billions)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  } else if (nrow(a) == 1) {  # Handle case with only one row
    ggplot(a, aes(x = date, y = billions)) +
      geom_point() +  # Use geom_point instead of geom_line for a single point
      labs(x = "Date", y = "Debt Load (Billions USD)", title = "U.S. Federal Debt") +
      ylim(0, max(a$billions)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  } else {  # Handle case where no rows match the filter
    ggplot() +
      labs(x = "Date", y = "Debt Load (Billions USD)", title = "U.S. Federal Debt") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 10)
  }
}





#####4. Shiny Server Logic#####
shinyServer(function(input, output, session) {
  
  # Reactive values to store current state
  rv <- reactiveValues(
    start_date = as.Date("1993-04-01"),
    end_date = as.Date("2024-07-02"),
    skip_years = 5,
    current_index = 1  # Initialize index for date sequence
  )
  
  # Generate date sequence based on skip_years
  date_sequence <- reactive({
    seq(rv$start_date, rv$end_date, by = paste0(rv$skip_years, " years"))
  })
  
  # Update plot based on current_index
  observe({
    # Check if current_index is within bounds of date_sequence
    if (rv$current_index <= length(date_sequence())) {
      Sys.sleep(1)  # Pause for 1 second
      rv$current_index <- rv$current_index + 1  # Increment current_index
      
      # Render plot based on current_index
      output$debt_plot <- renderPlot({
        outstanding_plot(outstanding, rv$start_date, date_sequence()[rv$current_index])
      })
    } else {
      # Reset current_index to start over if end of sequence reached
      rv$current_index <- 1
    }
  })
})