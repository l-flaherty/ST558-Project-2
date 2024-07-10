#####1. Load Required Packages#####
library(lubridate)
library(gifski)
library(datasauRus) 
library(tidyverse)
library(httr)
library(jsonlite)
library(shiny)
library(shinycssloaders)
library(gganimate)
load("gold.R") #So don't have to call API#





#####3. UDF: Graphing Functions#####
###3c. Gold###
gold_location=function(df, default_date="2024-05-31") {
  user_date=as.Date(default_date)
  
  closest_date=df |>
    filter(!is.na(date)) |>
    filter(date <= user_date) |>
    slice(which.max(date-user_date)) |>
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







#####4. Shiny Server Logic#####
shinyServer(function(input, output, session) {

  output$gold_plot=renderPlot({
    dateval=as.Date(input$input_date)
    gold_location(gold,dateval)
  })
})