# server.R

library(shiny)
library(dplyr)
library(ggplot2)

df <- read.csv("gold.csv", stringsAsFactors = FALSE)

gold_location=function(df, user_date="2024-05-31") {
  
  user_date=as.Date(user_date)
  
  closest_date <- df |>
    filter(!is.na(date)) |>
    slice(which.min(abs(date - user_date))) |>
    pull(date)
  
  troy=df |>
    filter(date==closest_date) |>
    group_by(location) |>
    summarize(troy_ounces=sum(qty))
  
  mytitle=paste0("U.S. Gold Holdings", " As Of ", user_date, " (Updated Monthly)")
  
  ggplot(troy, aes(x=location, y=troy_ounces)) +
    geom_bar(stat="identity", fill="darkgoldenrod1") +
    labs(x ="Location", y="Troy Ounces", title=mytitle) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(labels = scales::comma) 
}



function(input, output, session) {
  output$gold_plot <- renderPlot({
    gold_location(df, input$default_date)
  })
}