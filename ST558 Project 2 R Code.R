########## Written By Liam Flaherty For ST558 Project 2##########
#####1. Load Data And Required Packages#####
install.packages("tidyverse")
install.packages("httr")
install.packages("jsonlite")
library("tidyverse")
library("httr")
library("jsonlite")





#####2. Query APIs#####
###2a. Set up base urls, endpoints, and keys###
treasury_base_url="https://api.fiscaldata.treasury.gov/services/api/fiscal_service"

treasury_endpoint="/v1/accounting/od/tips_cpi_data_summary"
treasury_endpoint_FX="/v1/accounting/od/rates_of_exchange"



###2b. Set up url###
treasury_url=paste0(treasury_base_url, treasury_endpoint)
treasury_url1=paste0(treasury_base_url, treasury_endpoint_FX)



###2c. Query API###
api_return=GET(treasury_url)          
str(api_return, max.level=1)    #glimpse of what the API returns#


plaintext=fromJSON(rawToChar(api_return$content))
str(plaintext, max.level=1)    #plaintext$data seems to be format of most of them#

plaintext$data                 #record date not recent#



###2d. Getting correct number of pages###
#note that by default, API call only returns 100 observations on 1 page#
#https://fiscaldata.treasury.gov/datasets/treasury-reporting-rates-exchange/treasury-reporting-rates-of-exchange...#
#...gives information about pagination and other parts of the call#
#format added on to endpoint is e.g. "?page[number]=1&page[size]=3"#
#Can do plaintext$meta and see `total-count` and `total-pages` to expedite call#
#e.g. plaintext$meta[["total-pages"]] or plaintext$meta[["total-count"]]#

plaintext$meta[["total-count"]]     #17368#
plaintext$meta[["total-pages"]]     #174#

treasury_url=paste0(
  treasury_base_url,
  treasury_endpoint_FX,
  "?page[number]=", 1,
  "&page[size]=", 17368
)

api_return1=GET(treasury_url) 
plaintext=fromJSON(rawToChar(api_return1$content))
str(plaintext, max.level=1)        #error message: max per page is 10000#



###2e. Loop through pages###
page_size=1000
page_number=round(plaintext$meta[["total-count"]]/page_size)+1

treasury_df=data.frame()
for (i in 1:page_number) {
  
  treasury_url=paste0(
    treasury_base_url,
    treasury_endpoint_FX,
    "?page[number]=", i,
    "&page[size]=", 1000)
  
  api=GET(treasury_url)
  plain=fromJSON(rawToChar(api$content))
  a=plain$data
  
  treasury_df=rbind(treasury_df, a)
}

unique(treasury_df$record_date)            #looks good#



###2c. Create UDF To Make Easier###

treasury=function(data_type) {
  
  base_url="https://api.fiscaldata.treasury.gov/services/api/fiscal_service"
  
  endpoint_lookup=data.frame(
    data=c("TIPS", "FX"),
    url=c("/v1/accounting/od/tips_cpi_data_summary",
          "/v1/accounting/od/rates_of_exchange")
  )
  
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

             





#########scrap#################

treasury_TIPS=plaintext$data

api_return=GET(treasury_url1) 






FRED_base_url="https://api.stlouisfed.org/fred/category"
FRED_key="3b731d3fa63435daf96131814e4b9079"
categoryid=1234

FRED_url=paste0(FRED_base_url,
                "?category_id=", categoryid,
                "?api_key=", FRED_key)
