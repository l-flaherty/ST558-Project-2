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
treasury_base_url="https://api.fiscaldata.treasury.gov/services/api/fiscal_service/"

treasury_endpoint="v1/accounting/od/tips_cpi_data_summary"
treasury_endpoint_FX="v1/accounting/od/rates_of_exchange"



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



###2f. Add additional endpoints and extract details###
#See that treasury("TIPS") returns way less than expected#
#https://fiscaldata.treasury.gov/datasets/tips-cpi-data/reference-cpi-numbers-and-daily-index-ratios-summary-table...#
#says two possible data tables can be returned, summary (53 rows) and details (134 rows)#
#guess that details changes endpoint from "/v1/accounting/od/tips_cpi_data_summary" to ...#
#"/v1/accounting/od/tips_cpi_data_detail". Checking seems correct#

"v1/accounting/od/tips_cpi_data_detail"                 #Information on CPI and TIPS#
"v1/accounting/od/rates_of_exchange"                    #Quarterly info on exhange rates#
interest="v2/accounting/od/interest_expense"            #Interest Payments on debt#
auction="v1/accounting/od/auctions_query"               #treasury auction data#
debt="v2/accounting/od/debt_to_penny"                   #debt to the penny#
gold="v2/accounting/od/gold_reserve"                    #gold reserves#
rates="v2/accounting/od/avg_interest_rates"             #average interest rate across maturities#
outstanding="v2/accounting/od/debt_outstanding"         #total debt outstanding#
spending="v1/accounting/od/receipts_by_department"      #spending by dept#



###2f. Create UDF to make easier###

treasury=function(data_type) {
  endpoint_lookup=data.frame(
    input=c("tips", "fx", "interest", "auction", "debt", "gold", "rates", "outstanding", "spending"),
    url=c("v1/accounting/od/tips_cpi_data_detail",
          "v1/accounting/od/rates_of_exchange",
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

fx=treasury("fx")
tips=treasury("tips")
gold=treasury("gold")
debt=treasury("debt")
auction=treasury("auction")
rates=treasury("rates")
outstanding=treasury("outstanding")
spending=treasury("spending")
interest=treasury("interest")








#########scrap#################

treasury_TIPS=plaintext$data

api_return=GET(treasury_url1) 



treasury_base_url="https://api.fiscaldata.treasury.gov/services/api/fiscal_service"
treasury_endpoint="/v1/accounting/od/tips_cpi_data_detail"

api_return=GET(paste0(treasury_base_url, treasury_endpoint)) 
str(api_return, max.level=1)

plain=fromJSON(rawToChar(api_return$content))
str(plain, max.level=1)


FRED_base_url="https://api.stlouisfed.org/fred/category"
FRED_key="3b731d3fa63435daf96131814e4b9079"
categoryid=1234

FRED_url=paste0(FRED_base_url,
                "?category_id=", categoryid,
                "?api_key=", FRED_key)
