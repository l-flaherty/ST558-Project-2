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





#####3. UDF: Graphing Functions#####
###3a. Rates###
rates_table=function(df, user_year=2020, user_month=1) {
  a=df|>
    mutate(year=year(date), month=month(date)) |>
    filter(year==user_year, month==user_month) |>
    mutate(avg=mean(rate)) |>
    mutate(over=ifelse(rate>avg, "over average", "under average"))
  
  contin=table(a$type, a$over)
  debt_type=attributes(contin)$dimnames[[1]]
  m=as.data.frame.matrix(contin)
  mylist=list(cbind(debt_type,m), as.numeric(round(a[1,"avg"],2)))
  mylist
}




#####4. Shiny Server Logic#####
shinyServer(function(input, output, session) {
  
  output$myTable=renderTable({
    dateyear=input$user_year
    datemonth=input$user_month
    rates_table(rates, dateyear,datemonth)[[1]]
  })
  
  output$myText=renderText({
    dateyear=input$user_year
    datemonth=input$user_month
    avg=rates_table(rates, dateyear,datemonth)[[2]]
    mydate=paste0(dateyear, "-", datemonth)
    paste0("In ", mydate, " the average interest rate across all security types was ", avg, "%.")
  })
  
})
  
