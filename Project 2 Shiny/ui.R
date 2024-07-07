library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)


fluidPage(
  titlePanel("U.S. Treasury Data Visualizations"),
      tabsetPanel(
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
        
        tabPanel("Data Download", fluidPage(
          h2("Data Download"),
          selectInput("data_type", "Select Data Type:",
                      choices = c("Rates of Exchange"="fx", 
                                  "Interest Expense"="interest", 
                                  "Auctions Query"="auction",
                                  "Debt to Penny"="debt",
                                  "Gold Reserve"="gold", 
                                  "Average Interest Rates"="rates",
                                  "Debt Outstanding"="outstanding",
                                  "Receipts by Department"="spending")),
          downloadButton("download_data", "Download Data"),
          verbatimTextOutput("table_head")
        )),
        
        tabPanel("Data Exploration", plotOutput("plot2"))
      )
    )