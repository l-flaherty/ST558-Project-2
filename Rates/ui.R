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
load("rates.R") #So don't have to call API#



#####2. Create UI#####
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
                   condition = "input.data_type == 'rates'", 
                   
                   sliderInput("user_year", label="Select Year:", min=2001, max=2024, value=2020),  
                   sliderInput("user_month", label="Select Month:", min=1, max=12, value=1),  
                 )
               ),
      
               mainPanel(
                 tableOutput("myTable"),
                 textOutput("myText")
               )
             )
    )
  )
)