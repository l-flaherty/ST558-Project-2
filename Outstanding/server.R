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
load("outstanding.R")     #So don't have to call API#





#####3. UDF: Graphing Functions#####
###3b. Outstanding###
outstanding_plot=function(df, t1="1993", t2="2024") {
  
  #Make inputs dates and in the correct order#
  t1=as.Date(paste0(t1,"-01-01"))
  t2=as.Date(paste0(t2,"-01-01"))
  
  if(t1>t2) {
    a=t2; t2=t1; t1=a
    rm(a)
  }
  
  #Filter tibble to desired range#
  a=df |> 
    filter(date>=as.Date(t1), date<=as.Date(t2)) |>
    mutate(billions=debt/1000000000)
  
  #Plot a line chart if possible, otherwise a bar#
  if(nrow(a)>1) {
    ggplot(a, aes(x=date, y=billions)) +
      geom_area(fill=555, alpha=0.3) +
      geom_line(col="red", linewidth=1) +
      labs(x="Date", y="Debt Load (Billions USD)", title="U.S. Federal Debt") +
      ylim(0, max(a$billions)) +
      theme_bw()+
      theme(plot.title=element_text(hjust=0.5))
  } else {
    a$date = factor(a$date)
    
    ggplot(a, aes(x=date, y=billions)) +
      geom_bar(stat="identity", width=0.15, col="red", fill=555, alpha=0.3) +
      labs(x="Date", y="Debt Load (Billions USD)", title="U.S. Federal Debt") +
      ylim(0, max(a$billions)) +
      theme_bw()+
      theme(plot.title=element_text(hjust=0.5))
  }
}





#####4. Shiny Server Logic#####
shinyServer(function(input, output, session) {
  #Reactive values to store current state#
  myvals = reactiveValues(
    start=NULL,
    end=NULL,
    skip=NULL,
    current_end=NULL,
    play_status=FALSE
  )
  
  # Observe changes in input values and update reactive values#
  observe({
    myvals$start=input$start_date
    myvals$end=input$end_date
    myvals$skip=input$skip_value
  })
  
  output$debt_plot=renderPlot({
    outstanding_plot(outstanding, myvals$start, myvals$end)
  })
})