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

###########
#2. UDF: Data Download=18-138
#3. UDF: Functions=144-278
#4. Server Logic: 284-376
###########

#####2. UDF: Download Data#####
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
      mutate(date=as.Date(date), rate=as.numeric(rate)) |>
      suppressWarnings()
    
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

###2a. Returning Data From API###
fx=treasury("fx")
outstanding=treasury("outstanding")
rates=treasury("rates")
gold=treasury("gold")
debt=treasury("debt")





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
shinyServer(function(input, output, session) {
  ###4a. Data Download Tab###
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
  
  
  
  ###4b. Data Exploration tab###
  #Outstanding Section#
  myvals = reactiveValues(
    start=NULL,
    end=NULL,
    skip=NULL,
    current_end=NULL,
    play_status=FALSE
  )
  
  observe({
    myvals$start=input$start_date
    myvals$end=input$end_date
    myvals$skip=input$skip_value
  })
  
  output$debt_plot=renderPlot({
    outstanding_plot(outstanding, myvals$start, myvals$end)
  })
  
  #Debt Section#
  output$debt_image=renderPlot({
    date1=as.Date(input$date1)
    date2=as.Date(input$date2)
    debt_plot(debt, date1, date2)
  })
  
  #Gold Section#
  output$gold_plot=renderPlot({
    dateval=as.Date(input$input_date)
    gold_location(gold,dateval)
  })
  
  
  #Rates Section#
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
  
  #FX Section#
  observe({
    updateSelectInput(session, "country", choices=unique(fx$country))
  })
  
  observe({
    req(input$country)
    currencies=unique(fx$currency[fx$country==input$country])
    updateSelectInput(session, "currency", 
                      choices=c("All Currencies"="default", currencies),
                      selected="default")
  })
  
  output$fx_plot=renderPlot({
    req(input$country)
    fx_rate(input$country, input$currency)
  })
})