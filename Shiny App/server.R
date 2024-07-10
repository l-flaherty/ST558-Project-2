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

###########
#2. UDF: Data Download=21-
#3. UDF: Functions=27-162
#4. Server Logic:
###########

#####2. UDF: Download Data#####




#####3. UDF: Graphing Functions#####
###3a. Rates Function###
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



###3b. Debt Function###
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



###3c. Gold Function###
gold_location=function(df, default_date="2024-05-31") {
  user_date=as.Date(default_date)
  
  closest_date=df |>
    filter(!is.na(date)) |>
    filter(date <= user_date) |>
    slice(which.max(date-user_date)) |>
    pull(date)
  
  troy=df |>
    filter(date==closest_date) |>
    group_by(location) |>
    summarize(troy_ounces=sum(qty))
  
  mytitle=paste0("U.S. Gold Holdings", " As Of ", user_date, " (Updated Monthly)")
  
  ggplot(troy, aes(x=location, y=troy_ounces)) +
    geom_bar(stat="identity", fill="darkgoldenrod1") +
    labs(x ="Location", y="Troy Ounces", title=mytitle) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(labels = scales::comma) 
}



###3d. FX Function###
fx_rate=function(user_country, user_currency="default", user_date1="2001-03-31", user_date2="2024-06-14") {
  a=fx|>
    filter(country==user_country, 
           (date>=user_date1 & date<=user_date2))
  
  if(user_currency=="default") {
    a=a
  } else {
    a=filter(a, currency==user_currency)
  }
  
  ggplot(data=a, aes(x=date, y=rate_per_usd)) +
    geom_line(color="Red") +
    labs(x="Date", 
         y="Rate Per USD", 
         title=paste(a$country, a$currency, "Per USD")) +
    scale_y_continuous(labels=function(x) format(x, scientific=FALSE)) +
    theme_bw() +
    theme(plot.title=element_text(hjust=0.5))
}



###3e. Outstanding Function###
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





######4. Server Logic######
shinyServer(function(input, output) {
  
  #Store API data based on user input#
  api_data <- reactive({
    req(input$data_type)  
    treasury(input$data_type)
  })
  
  #Show some data on tab#
  output$table_head <- renderPrint({
    api_data()
  })
  
  #Download data as CSV#
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_download.csv")
    },
    content = function(file) {
      write.csv(as.data.frame(api_data()), file, row.names = FALSE)
    }
  )
  
  
  
  #Placeholder Plot for Data Exploration tab#
  output$plot2 <- renderPlot({
    plot(1:10, main = "Placeholder Plot")
  })
})