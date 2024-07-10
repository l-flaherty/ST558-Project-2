#####1. Load Required Packages#####
library(lubridate)
library(gifski)
library(datasauRus) 
library(tidyverse)
library(httr)
library(jsonlite)
library(shiny)
library(shinycssloaders)
library("gganimate")
load("debt.R") #So don't have to call API#



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
                                         "Debt Breakdown"="debt",
                                         "Gold Reserve"="gold", 
                                         "Average Interest Rates"="rates",
                                         "Debt Outstanding"="outstanding",
                                         "Receipts by Department"="spending")),
                 
                 conditionalPanel(
                   condition = "input.data_type == 'debt'", 
                   
                   numericInput("date1", label="Start Date", value=2000),  
                   numericInput("date2", label="Start Date", value=2020)
                 )
               ),
               
               mainPanel(
                 plotOutput("debt_image")
               )
             )
    )
  )
)