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
fx_rate=function(user_country, user_currency="default") {
  a=fx|>
    filter(country==user_country)
  
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
  
  # Update country choices
  observe({
    updateSelectInput(session, "country", choices = unique(fx$country))
  })
  
  # Update currency choices based on selected country
  observe({
    req(input$country)
    currencies <- unique(fx$currency[fx$country == input$country])
    updateSelectInput(session, "currency", 
                      choices = c("All Currencies" = "default", currencies),
                      selected = "default")
  })
  
  # Create the plot
  output$fx_plot <- renderPlot({
    req(input$country)
    fx_rate(input$country, input$currency)
  })
  
})