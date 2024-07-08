library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)

#UDF#
treasury=function(data_type) {
  
  #Create URL for API Query#
  endpoint_lookup=data.frame(
    input=c("fx", "interest", "debt", "gold", "rates", "outstanding", "spending"),
    url=c("v1/accounting/od/rates_of_exchange",
          "v2/accounting/od/interest_expense",
          "v2/accounting/od/debt_to_penny",
          "v2/accounting/od/gold_reserve",
          "v2/accounting/od/avg_interest_rates",
          "v2/accounting/od/debt_outstanding",
          "v1/accounting/od/receipts_by_department")
  )
  base_url="https://api.fiscaldata.treasury.gov/services/api/fiscal_service/"
  endpoint=endpoint_lookup[,2][match(data_type, endpoint_lookup[,1])]
  
  #Query API#
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
  
  #Format resulting data#
  if(data_type=="fx") {
    
    df=df |>
      select(effective_date, country, currency, exchange_rate) |>
      rename(date=effective_date, rate_per_usd=exchange_rate) |>
      mutate(date=as.Date(date), rate_per_usd=as.numeric(rate_per_usd))
    
  } else if (data_type=="gold") {
    
    goldcol=c("record_date", "facility_desc", "form_desc", "location_desc", "fine_troy_ounce_qty", "book_value_amt")
    
    df= df |>
      select(all_of(goldcol)) 
    names(df)=c("date", "facility", "form", "location", "qty", "book_value")
    
    df=df |>
      mutate(facility=str_replace(facility, " Held Gold", "")) |>
      mutate(location=str_replace(location, "All Locations- Coins, blanks, m", "M")) |>
      mutate(location=str_replace(location, "Federal Reserve Banks", "FRB")) |>
      mutate(date=as.Date(date), qty=round(as.numeric(qty),2), book_value=as.numeric(book_value))
    
  } else if (data_type=="interest") {
    
    df=df |> 
      rename(date=record_date,
             category=expense_catg_desc,
             group=expense_group_desc,
             type=expense_type_desc,
             mtd_expense=month_expense_amt,
             ytd_expense=fytd_expense_amt) |>
      select(date, category, group, type, mtd_expense, ytd_expense) |>
      mutate(category=str_replace(category, "INTEREST EXPENSE ON ", ""))
    
  } else if (data_type=="outstanding") {
    
    df=df |>
      rename(date=record_date, debt=debt_outstanding_amt) |>
      select(date, debt) |>
      mutate(date = as.Date(date)) |>
      mutate(debt = round(as.numeric(debt),2))
    
  } else if (data_type=="debt") {
    
    df=df|>
      rename(date=record_date,
             debt_held_public=debt_held_public_amt,
             intragov_hold=intragov_hold_amt,
             debt_outstanding=tot_pub_debt_out_amt) |>
      select(date, debt_held_public, intragov_hold, debt_outstanding) |>
      mutate(date=as.Date(date), 
             debt_held_public=suppressWarnings(as.numeric(debt_held_public)),
             intragov_hold=suppressWarnings(as.numeric(intragov_hold)),
             debt_outstanding=suppressWarnings(as.numeric(debt_outstanding))) 
    
  } else if (data_type=="rates") {
    
    df=df|>
      rename(date=record_date, type=security_type_desc, security=security_desc, rate=avg_interest_rate_amt) |>
      select(date, type, security, rate) |>
      suppressWarnings(mutate(date=as.Date(date), as.numeric(rate)))
    
  } else if (data_type=="spending") {
    
    df=df |>
      rename(date=record_date, 
             line_item=receipt_line_item_nm, 
             agency_id=aid_cd, 
             amount=receipt_amt) |>
      select(date, line_item, agency_id, amount) |>
      mutate(date=as.Date(date), 
             agency_id=as.numeric(agency_id),
             amount=as.numeric(amount))
  }
  return(as_tibble(df))
}



######2. Server Logic######
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
  
  
  
  #Placeholder reactive plot for Data Exploration tab#
  output$plot2 <- renderPlot({
    plot(1:10, main = "Placeholder Plot")
  })
})
