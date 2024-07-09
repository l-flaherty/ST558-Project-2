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
load("outstanding.R") #So don't have to call API#





#####3. UDF: Graphing Functions#####
###3b. Outstanding###
outstanding_plot=function(df, t1="1993-04-01", t2="2024-07-02") {
  a=df |> 
    filter(date >= t1 & date <=t2) |>
    mutate(billions=debt/1000000000)
  
  ggplot(a, aes(x=date, y=billions)) +
    geom_area(fill=555, alpha=0.3) +
    geom_line(col="red", size=1) +
    labs(x="Date", y="Debt Load (Billions USD)", title="U.S. Federal Debt") +
    ylim(0, max(a$billions)) +
    theme_bw() +
    theme(plot.title=element_text(hjust=0.5))
}




#####4. Shiny Server Logic#####
shinyServer(function(input, output, session) {
  
  observeEvent(input$play_button, {
    isolate({
      start_date=input$start_date
      end_date=input$end_date
      skip_years=input$skip_years
      
      date_sequence=seq(start_date, end_date, by=paste0(skip_years, " years"))
      
      i=1
      
      autoInvalidate=reactiveTimer(1000)
      
      observe({
        autoInvalidate()
        if (i <= length(date_sequence)) {
          output$debt_plot <- renderPlot({
            outstanding_plot(outstanding, t1=start_date, t2=date_sequence[i])
          })
          i=i+1
        } else {
          stopObserving()
        }
      })
    })
  })
  
  stopObserving <- function() {
    stopObserver <- reactiveVal(FALSE)
    observe({
      if (isolate(stopObserver())) {
        return()
      }
      stopObserver(TRUE)
      stop()
    })
  }
  
  output$debt_plot <- renderPlot({
    outstanding_plot(outstanding, t1=input$start_date, t2=input$end_date)
  })
  
})