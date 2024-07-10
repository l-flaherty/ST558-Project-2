# ST558-Project-2
Data Science For Statisticians Project #2

This project is an R Shiny App centered around macroeconomic and fiscal data provided by the U.S. Treasury Department. The app allows users to easily query the U.S. Treasury's API (documentation at https://fiscaldata.treasury.gov/api-documentation/ as of 2024-07-01) to get data on gold holdings, interest rates, exchange rates, inflation, federal debt, federal spending, and treasury auctions. 

The app requires a few libaries including the tidyverse, httr, and jsonlite. Users can install all required packages prior to running the app with the following: install.packages(c("tidyverse", "httr", "jsonlite", "shiny", "lubridate")), and then load them in with library(tidyverse), library(httr), etc. 

One can run shiny::runGitHub("ST558-Project-2", "l-flaherty", ref="main", subdir="Shiny App") directly from their console to interact with the application. 
