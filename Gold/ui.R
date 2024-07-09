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



#####2. Create UI#####
fluidPage(
  
  ###Title###
  titlePanel("U.S. Gold Holdings Analysis"),
  
  ####Sidebar layout with input###
  sidebarLayout(
      sidebarPanel(
      
      #Date input#
      dateInput("input_date", 
                label="Select a date:", 
                value=as.Date("2024-05-31"))),
    
    ###Show Results Of Graph###
    mainPanel(
      plotOutput("gold_plot")
    )
  )
)