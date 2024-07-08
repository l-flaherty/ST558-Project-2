library(shiny)
library(shinycssloaders)
library(tidyverse)
library(httr)
library(jsonlite)

######1. UDF To Download Data From API######
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





#####2. Returning Data From API#####
#fx=treasury("fx")
#interest=treasury("interest")
#outstanding=treasury("outstanding")
#spending=treasury("spending")
#rates=treasury("rates")
#gold=treasury("gold")
#debt=treasury("debt")






#####3. Graphing Functions#####
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
    geom_line() +
    labs(x="Date", 
         y="Rate Per USD", 
         color="Red", 
         title=paste(a$country, a$currency, "Per USD")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    theme_bw() +
    theme(plot.title=element_text(hjust=0.5))
}







#####4. Shiny Server Logic#####
shinyServer(function(input, output, session) {
  
  # ReactiveValues to store available countries and currencies
  data_choices <- reactiveValues(countries = character(0),
                                 currencies = character(0))
  
  # Observe data type selection and update country choices
  observe({
    req(input$data_type)
    
    if (input$data_type == "fx") {  # Only update for 'fx' data type
      countries <- unique(fx$country)
      data_choices$countries <- countries
    } else {
      data_choices$countries <- character(0)
    }
  })
  
  # Observe country selection and update currency choices
  observe({
    req(input$country)
    
    if (!is.null(input$country)) {
      currencies <- fx |>
        filter(country == input$country) |>
        pull(currency) |>
        unique()
      
      data_choices$currencies <- currencies
    } else {
      data_choices$currencies <- character(0)
    }
  })
  
  # Render the plot based on user selection
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
    updateSelectInput(session, "country", choices = data_choices$countries)
  })
  
  # Update currency choices based on selected country
  observe({
    updateSelectInput(session, "currency", choices = data_choices$currencies)
  })
  
})