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
load("debt.R") #So don't have to call API#





#####3. UDF: Graphing Functions#####
###3d. Debt###
debt_plot=function(df, t1=1993, t2=2024, type="default") {
  if(t1>t2) {
    a=t2; t2=t1; t1=a
  }
  
  a=df |> 
    filter(!is.na(debt_held_public)) |>
    select(date, "Debt Held By Public"="debt_held_public",
           "Intragovernment Holdings"="intragov_hold") |>
    mutate(year=year(date)) |>
    distinct(year, .keep_all=TRUE) |>
    filter(year>=t1 & year<=t2) |>
    pivot_longer(cols=c("Debt Held By Public", "Intragovernment Holdings"), 
                 names_to="type", 
                 values_to="value") |>
    mutate(value=value/1000000000000)
  
  ggplot(a, aes(x=date, y=value, fill=type)) +
    geom_bar(position="stack", stat="identity") +
    labs(x="Date", y="Debt Load (Trillions USD)", 
         title= paste0("National Debt Breakdown (", 
                       as.numeric(t1), "-", as.numeric(t2), ")")) +
    theme_bw()+
    theme(plot.title=element_text(hjust=0.5))
}




#####4. Shiny Server Logic#####
shinyServer(function(input, output, session) {
  
  output$debt_image=renderPlot({
    date1=as.Date(input$date1)
    date2=as.Date(input$date2)
    debt_plot(debt, date1, date2)
  })
})