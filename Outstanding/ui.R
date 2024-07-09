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



#####2. Create UI#####
ui <- fluidPage(
  titlePanel("U.S. Federal Debt Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("start_date", "Start Date:", value = as.Date("1993-04-01")),
      dateInput("end_date", "End Date:", value = as.Date("2024-07-02")),
      numericInput("skip_years", "Skip Years:", value = 5, min = 1, max = 20),
      actionButton("play_button", "See Growth"),
      width = 3
    ),
    
    mainPanel(
      plotOutput("debt_plot")
    )
  )
)



