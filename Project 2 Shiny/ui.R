library(shiny)
library(shinycssloaders)
library(tidyverse)
library(httr)
library(jsonlite)
library(gganimate)


fluidPage(
  titlePanel("Data Exploration"),
  
  tabsetPanel(
    tabPanel("Data Exploration",
             sidebarLayout(
               sidebarPanel(
                 selectInput("data_type", "Select Data Type:",
                             choices = c("Rates of Exchange"="fx", 
                                         "Interest Expense"="interest", 
                                         "Debt to Penny"="debt",
                                         "Gold Reserve"="gold", 
                                         "Average Interest Rates"="rates",
                                         "Debt Outstanding"="outstanding",
                                         "Receipts by Department"="spending")),
                 
                 conditionalPanel(
                   condition = "input.data_type == 'fx'",  # Check if data type is 'fx'
                   
                   selectInput("country", "Select Country:",
                               choices = NULL),  # Initialize with NULL choices
                   
                   selectInput("currency", "Select Currency:",
                               choices = NULL),  # Initialize with NULL choices
                   
                   sliderInput("year_slider", "Select Year:",
                               min = 1990, max = 2024, value = c(2001, 2024),
                               step = 1, animate = TRUE)
                 )
               ),
               
               mainPanel(
                 plotOutput("graphic_plot")
               )
             )
    )
  )
)