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
ui=fluidPage(
  titlePanel("U.S. Federal Debt Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("start_date", label="Start Year:", value=1993, min=1790),
      numericInput("end_date", label="End Year:", value=2024, max=2024),
      numericInput("skip_value", label="Years To Skip", value=2, min=1, max=10),
      actionButton("play", label="Play Animation"),
      width=3
    ),
    
    mainPanel(
      plotOutput("debt_plot")
    )
  )
)