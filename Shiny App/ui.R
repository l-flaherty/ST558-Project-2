#####1. Load Required Packages#####
library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(gifski)
library(datasauRus) 
library(shinycssloaders)
library(gganimate)
load("gold.R")
load("debt.R")
load("outstanding.R")
load("fx.R")
load("rates.R")





#####2. Create UI For App#####
fluidPage(
  titlePanel("U.S. Treasury Data Visualizations"),
  tabsetPanel(
    
    
    
    ###2a. The First Tab, About###
    tabPanel("About", fluidPage(
      tags$div(
        h1("Information About This Shiny App"),
        p("This app is for users who want to learn more about the evolution
              of U.S. economic and fiscal conditions. The app pulls data from
              the U.S. Treasury's API (full details are available at ", 
          tags$a(href = "https://fiscaldata.treasury.gov/api-documentation/", "www.treasury.gov/api-documentation"), ")."),
        p("The first tab of the App, 'Data Download', allows users to easily pull data from specific API endpoints. 
              There is data available on exchange rates, interest rates, gold holdings, federal debt,
              federal spending, budgets, and more."),
        p("The second tab of the App, `Data Exploration`, allows users to visualize the data returned from the API."),
        tags$hr(),
        img(src ="treasury.jpg"),
        img(src="building.jpg", style="width: 30%; height: auto;")
      )
    )),
    
    
    
    ###2b. The Second Tab, Data Download###
    tabPanel("Data Download", fluidPage(
      h2("Data Download"),
      selectInput("data_type", "Select Data Type:",
                  choices = c("Rates of Exchange"="fx", 
                              "Debt Outstanding"="outstanding",
                              "Debt to Penny"="debt",
                              "Average Interest Rates"="rates",
                              "Gold Reserve"="gold")),
      downloadButton("download_data", "Download Data"),
      verbatimTextOutput("table_head")
    )),
    
    
    
    ###2c. The Third Tab, Data Exploration###
    tabsetPanel(
      tabPanel("Data Exploration",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("data_type", "Select Data Type:",
                               choices = c("Rates of Exchange"="fx", 
                                           "Debt Outstanding"="outstanding",
                                           "Debt to Penny"="debt",
                                           "Average Interest Rates"="rates",
                                           "Gold Reserve"="gold")),
                   
                   #Conditional Widgets#
                   conditionalPanel(
                     condition="input.data_type == 'rates'", 
                     sliderInput("user_year", label="Select Year:", min=2001, max=2024, value=2020),  
                     sliderInput("user_month", label="Select Month:", min=1, max=12, value=1),  
                   ),
                   
                   conditionalPanel(
                     condition="input.data_type == 'fx'", 
                     selectInput("country", "Select Country:", choices=NULL),  
                     selectInput("currency", "Select Currency:", choices=NULL),  
                     uiOutput("year_slider_ui")
                   ),
                   
                   conditionalPanel(
                     condition="input.data_type == 'gold", 
                     dateInput("input_date", 
                               label="Select a date:", 
                               value=as.Date("2024-05-31"))
                   ),
                   
                   conditionalPanel(
                     condition = "input.data_type == 'debt'", 
                     numericInput("date1", label="Start Date", value=2000),  
                     numericInput("date2", label="Start Date", value=2020)
                   ),
                   
                   conditionalPanel(
                     condition = "input.data_type == 'outstanding'", 
                     numericInput("start_date", label="Start Year:", value=1993, min=1790),
                     numericInput("end_date", label="End Year:", value=2024, max=2024)
                   )
                 ),
                 
                 #Conditional Display#
                 mainPanel(
                   conditionalPanel(
                     condition="input.data_type == 'rates'",
                     tableOutput("myTable"),
                     textOutput("myText")
                   ),
                   
                   conditionalPanel(
                     condition="input.data_type == 'fx'",
                     plotOutput("fx_plot")
                   ),
                   
                   conditionalPanel(
                     condition="input.data_type == 'gold'",
                     plotOutput("gold_plot")
                   ),
                   
                   conditionalPanel(
                     condition="input.data_type == 'debt'",
                     plotOutput("debt_image")
                   ),
                   
                   conditionalPanel(
                     condition="input.data_type == 'outstanding'",
                     plotOutput("debt_plot")
                   )
                 )
               )
      )
    )
  )
)