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
load("fx.R") #So don't have to call API#





#####3. UDF: Graphing Functions#####
###3a. FX###
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





#####4. Shiny Server Logic#####
shinyServer(function(input, output, session) {
  
  #Store available countries and currencies#
  data_choices=reactiveValues(countries=character(0),
                                 currencies=character(0))
  
  #If right data type selection, update country choices#
  observe({
    req(input$data_type)
    
    if (input$data_type == "fx") {  
      countries <- unique(fx$country)
      data_choices$countries <- countries
    } else {
      data_choices$countries <- character(0)
    }
  })
  
  
  # Update choices for country and currency inputs
  observe({
    
    updateSelectInput(session, "country", choices = unique(fx$country))
    updateSelectInput(session, "currency", choices = unique(fx$currency))
  })
  
  output$year_slider_ui <- renderUI({
    req(input$country, input$currency)  
    
    
    filtered_fx <- fx |>
      filter(country == input$country, currency == input$currency)
    
    
    min_year <- year(min(filtered_fx$date))
    max_year <- year(max(filtered_fx$date))
    
    sliderInput("year_slider", "Select Year:",
                min = min_year, max = max_year,
                value = c(min_year, max_year),
                step = 1, animate = TRUE)
  })
  
  
  
  
  
  
  #Create line plot#
  output$graphic_plot <- renderPlot({
    req(input$data_type)
    
    if (input$data_type == "fx" &&
        !is.null(input$country) &&
        !is.null(input$currency)) {
      fx_rate(input$country, input$currency, as.Date(paste0(input$year_slider[1], "-01-01")), as.Date(paste0(input$year_slider[2], "-12-31")))
    } else {
      ggplot() +
        geom_point() +
        labs(title = "Select a Data Type and Inputs to Begin", x = "", y = "")
    }
  })
  
  # Update country choices based on selected data type
  observe({
    updateSelectInput(session, "country", choices=data_choices$countries)
  })
  
  # Update currency choices based on selected country
  observe({
    updateSelectInput(session, "currency", choices=data_choices$currencies)
  })
  
})