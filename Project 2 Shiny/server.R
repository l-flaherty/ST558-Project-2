library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)

#UDF#
treasury=function(data_type) {
  endpoint_lookup=data.frame(
    input=c("fx", "interest", "auction", "debt", "gold", "rates", "outstanding", "spending"),
    url=c("v1/accounting/od/rates_of_exchange",
          "v2/accounting/od/interest_expense",
          "v1/accounting/od/auctions_query",
          "v2/accounting/od/debt_to_penny",
          "v2/accounting/od/gold_reserve",
          "v2/accounting/od/avg_interest_rates",
          "v2/accounting/od/debt_outstanding",
          "v1/accounting/od/receipts_by_department")
  )
  
  base_url="https://api.fiscaldata.treasury.gov/services/api/fiscal_service/"
  endpoint=endpoint_lookup[,2][match(data_type, endpoint_lookup[,1])]
  
  api_return=GET(paste0(base_url, endpoint))
  plaintext=fromJSON(rawToChar(api_return$content))
  
  page_size=1000
  page_number=round(plaintext$meta[["total-count"]]/page_size)+1
  
  df=data.frame()
  
  for (i in 1:page_number) {
    url=paste0(
      base_url,
      endpoint,
      "?page[number]=", i,
      "&page[size]=", 1000)
    
    api=GET(url)
    plain=fromJSON(rawToChar(api$content))
    a=plain$data
    
    df=rbind(df, a)
  }
  return(as_tibble(df))
}



# Server Logic
shinyServer(function(input, output) {
  
  # Reactive expression to store API data based on user input
  api_data <- reactive({
    req(input$data_type)  # Require the user to select a data type
    treasury(input$data_type)
  })
  
  # Render the table output based on user selection
  output$table_head <- renderPrint({
    head(api_data())
  })
  
  # Placeholder reactive plot for Data Exploration tab
  output$plot2 <- renderPlot({
    # Your plot logic here
    plot(1:10, main = "Placeholder Plot")
  })
})
