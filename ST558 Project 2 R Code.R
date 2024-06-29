########## Written By Liam Flaherty For ST558 Project 2##########
#####1. Load Data And Required Packages#####
install.packages("tidyverse")
install.packages("httr")
install.packages("jsonlite")
library("tidyverse")
library("httr")
library("jsonlite")

#####2. Query APIs#####
###2a. Set up base urls, enpoints, and keys###
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

#note that by default, API call only returns 100 observations on 1 page#
#https://fiscaldata.treasury.gov/datasets/treasury-reporting-rates-exchange/treasury-reporting-rates-of-exchange...#
#...gives information about pagination and other parts of the call#
#Can do plaintext$meta and see `total-count` and `total-pages` to expedite call#
#e.g. plaintext$meta[["total-pages"]] or plaintext$meta[["total-count"]]#


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
  return(plaintext$data)
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
