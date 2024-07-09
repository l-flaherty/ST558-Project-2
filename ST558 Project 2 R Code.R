########## Written By Liam Flaherty For ST558 Project 2##########
#####1. Load Data And Required Packages#####
install.packages("tidyverse")
install.packages("httr")
install.packages("jsonlite")
install.packages("shiny")
install.packages("shinycssloaders")
install.packages("gganimate")
install.packages("datasauRus")
install.packages("gifski")
install.packages("lubridate")
library("lubridate")
library("gifski")
library("datasauRus") 
library("tidyverse")
library("httr")
library("jsonlite")
library("shiny")
library("shinycssloaders")
library("gganimate")
options(scipen=999)                  #to prevent scientific  notation with large numbers#
#options(scipen = 0, digits = 7)     to reset#



#####2. Query APIs#####
###2a. Set up base urls, endpoints, and keys###
treasury_base_url="https://api.fiscaldata.treasury.gov/services/api/fiscal_service/"
treasury_endpoint="v1/accounting/od/rates_of_exchange"



###2b. Set up url###
treasury_url=paste0(treasury_base_url, treasury_endpoint)



###2c. Query API###
api_return=GET(treasury_url)          
str(api_return, max.level=1)                        #glimpse of what the API returns#


plaintext=fromJSON(rawToChar(api_return$content))
str(plaintext, max.level=1)                         #plaintext$data seems to be format of most of them#

plaintext$data                 
unique(plaintext$data$record_date)                  #record date not recent#



###2d. Getting correct number of pages###
#note that by default, API call only returns 100 observations on 1 page#
#https://fiscaldata.treasury.gov/datasets/treasury-reporting-rates-exchange/treasury-reporting-rates-of-exchange...#
#...gives information about pagination and other parts of the call#
#format added on to endpoint is e.g. "?page[number]=1&page[size]=3"#
#Can do plaintext$meta and see `total-count` and `total-pages` to expedite call#
#e.g. plaintext$meta[["total-pages"]] or plaintext$meta[["total-count"]]#

plaintext$meta[["total-count"]]     #17368#
plaintext$meta[["total-pages"]]     #174#

test_url=paste0(
  treasury_base_url,
  treasury_endpoint,
  "?page[number]=", 1,
  "&page[size]=", 17368
)

test_return=GET(test_url) 
test_plain=fromJSON(rawToChar(test_return$content))
str(test_plain, max.level=1)                           #error message: see that max per page is 10000#
rm(test_url, test_return, test_plain)                   #keep environment clean#



###2e. Loop through pages###
page_size=1000
page_number=round(plaintext$meta[["total-count"]]/page_size)+1

treasury_df=data.frame()
for (i in 1:page_number) {
  
  treasury_url=paste0(
    treasury_base_url,
    treasury_endpoint,
    "?page[number]=", i,
    "&page[size]=", 1000)
  
  api=GET(treasury_url)
  plain=fromJSON(rawToChar(api$content))
  a=plain$data
  
  treasury_df=rbind(treasury_df, a)
}

rm(api, plain, a, i)
unique(treasury_df$record_date)            #looks good#



###2f. Add additional endpoints and extract details###
#See that treasury("TIPS") returns way less than expected#
#https://fiscaldata.treasury.gov/datasets/tips-cpi-data/reference-cpi-numbers-and-daily-index-ratios-summary-table...#
#says two possible data tables can be returned, summary (53 rows) and details (134 rows)#
#guess that details changes endpoint from "/v1/accounting/od/tips_cpi_data_summary" to ...#
#"/v1/accounting/od/tips_cpi_data_detail". Checking seems correct#

tips="v1/accounting/od/tips_cpi_data_detail"            #Information on CPI and TIPS#
fx="v1/accounting/od/rates_of_exchange"                 #Quarterly info on exchange rates#
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
tips=treasury("tips")     #something going wrong now#
gold=treasury("gold")
debt=treasury("debt")
auction=treasury("auction")
rates=treasury("rates")
outstanding=treasury("outstanding")
spending=treasury("spending")
interest=treasury("interest")





#####3. Exploratory Data Analysis#####
###3a. Gold###
#The U.S. Treasury-Owned Gold dataset provides the amount of gold that is available...# 
#...across various U.S. Treasury-maintained locations. The data shows whether the gold...# 
#...is held in deep storage or working stock, that is, available to the U.S. Mint as raw...#
#...material for the creation of congressionally authorized coins. The dataset includes...# 
#...the weight of gold in troy ounces (a measurement unit still used today for precious...# 
#...metals and gunpowder) and the book value in dollars.The book value is not the market value,...#
#...but instead represents the total number of troy ounces multiplied by a value established by law ($42.222), set in 1973.#
#https://fiscaldata.treasury.gov/datasets/status-report-government-gold-reserve/u-s-treasury-owned-gold#

as.data.frame(gold[1:5,])   #no need for many of the columns#
goldcol=c("record_date", "facility_desc", "form_desc", "location_desc", "fine_troy_ounce_qty", "book_value_amt")


gold= gold |>
  select(all_of(goldcol)) 
names(gold)=c("date", "facility", "form", "location", "qty", "book_value")

unique(gold$facility)
unique(gold$form)
unique(gold$location)

gold=gold |>
  mutate(facility=str_replace(facility, " Held Gold", "")) |>
  mutate(location=str_replace(location, "All Locations- Coins, blanks, m", "M")) |>
  mutate(location=str_replace(location, "Federal Reserve Banks", "FRB")) |>
  mutate(date=as.Date(date), qty=round(as.numeric(qty),2), book_value=as.numeric(book_value))


table(gold$form, gold$facility)   #make contingency table#



###3b. FX###
#The Treasury Reporting Rates of Exchange dataset provides the U.S. government's...# 
#...authoritative exchange rates to ensure consistency for foreign currency units and...# 
#...U.S. dollar equivalents across all reporting done by agencies of the government...# 
#...This report covers any foreign currencies in which the U.S. government has an interest,...# 
#...including: receipts and disbursements, accrued revenues and expenditures, authorizations,...# 
#...obligations, receivables and payables, refunds, and similar reverse transaction items.# 
#...The Secretary of the Treasury has the sole authority to establish the exchange rates for...# 
#...all foreign currencies or credits reported by government agencies under federal law.# 
#...For pulling specific exchange rates based on country or currency please see the Notes and Known Limitations below.#
#https://fiscaldata.treasury.gov/datasets/treasury-reporting-rates-exchange/treasury-reporting-rates-of-exchange#

as.data.frame(fx[1:5,])   #no need for many of the columns#

fx=fx |>
  select(effective_date, country, currency, exchange_rate) |>
  rename(date=effective_date, rate_per_usd=exchange_rate) |>
  mutate(date=as.Date(date), rate_per_usd=as.numeric(rate_per_usd))

unique(fx$country)
unique(fx$currency)
unique(fx$date)

print(fx[which(fx$country=="China"),], n=74)



###3b. Interest###
#The Interest Expense on the Public Debt Outstanding dataset provides monthly and...# 
#...fiscal year-to-date values for interest expenses on federal government debt, that is, ...#
#the cost to the U.S. for borrowing money (calculated at a specified rate and period of time)...#
#This dataset is useful for those who wish to track the cost of maintaining federal debt.#
#https://fiscaldata.treasury.gov/datasets/interest-expense-debt-outstanding/interest-expense-on-the-public-debt-outstanding#

as.data.frame(interest[1:5,])

interest=interest |> 
  rename(date=record_date,
         category=expense_catg_desc,
         group=expense_group_desc,
         type=expense_type_desc,
         mtd=month_expense_amt,
         ytd=fytd_expense_amt) |>
  select(date, category, group, type, mtd, ytd) |>
  mutate(category=str_replace(category, "INTEREST EXPENSE ON ", "")) |>
  mutate(date=as.Date(date), mtd=as.numeric(mtd), ytd=as.numeric(ytd))

unique(interest$category)
unique(interest$group)
unique(interest$type)




###3d. Rates#
#The Average Interest Rates on U.S. Treasury Securities dataset provides average interest rates...# 
#...on U.S. Treasury securities on a monthly basis. Its primary purpose is to show the average interest...#
#...rate on a variety of marketable and non-marketable Treasury securities. Marketable securities consist of...#
#...Treasury Bills, Notes, Bonds, Treasury Inflation-Protected Securities (TIPS), Floating Rate Notes (FRNs),...#
#...and Federal Financing Bank (FFB) securities. Non-marketable securities consist of Domestic Series, Foreign Series,...#
#...State and Local Government Series (SLGS), U.S. Savings Securities, and Government Account Series (GAS) securities...#
#...Marketable securities are negotiable and transferable and may be sold on the secondary market. Non-marketable securities...#
#...are not negotiable or transferrable and are not sold on the secondary market. This is a useful dataset for investors and...# 
#...bond holders to compare how interest rates on Treasury securities have changed over time.#
#https://fiscaldata.treasury.gov/datasets/average-interest-rates-treasury-securities/average-interest-rates-on-u-s-treasury-securities#

rates=treasury("rates")

rates=rates|>
  rename(date=record_date, type=security_type_desc, security=security_desc, rate=avg_interest_rate_amt) |>
  select(date, type, security, rate) |>
  suppressWarnings(
    mutate(date=as.Date(date), as.numeric(rate)))

unique(rates$type)
unique(rates$security)



###3e. TIPS###
#The TIPS and CPI Data dataset contains data on Treasury Inflation Protected Securities (TIPS) and the Consumer Price Index (CPI).#
#The principal of a TIPS fluctuates with inflation and deflation. While the interest rate is fixed, the amount of interest paid...#
#...every six months may vary based on any change in the principal. Those changes are tied to the Consumer Price Index from the U.S.#
#Department of Labor, Bureau of Labor Statistics. This dataset is useful in calculating inflation-adjusted interest payments.#
#https://fiscaldata.treasury.gov/datasets/tips-cpi-data/reference-cpi-numbers-and-daily-index-ratios-summary-table#

#/v1/accounting/od/tips_cpi_data_summary vs 	/v1/accounting/od/tips_cpi_data_detail#
#xxxxxxxdo left_join inside formula, change term to numericxxxxxxxx#
#dated_date is when interest starts acruing#
#index ratio is dividing the reference CPI on the issue date of the security...# 
#... by the reference CPI on the original dated date of the security being announced/auctioned.#

a=GET(paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/",
             "/v1/accounting/od/tips_cpi_data_summary"))
p1=fromJSON(rawToChar(a$content))
tips_summary=as_tibble(p1$data)      
rm(a, p1)

tips
tips_summary

tips_summary=tips_summary |>
  rename(term=security_term,
         auction_date=original_auction_date,
         cpi_on_dated_date=ref_cpi_on_dated_date) |>
  select(cusip, interest_rate, term, auction_date, dated_date, cpi_on_dated_date)

tips=tips|>
  rename(issue=original_issue_date, cpi=ref_cpi) |>
  select(cusip, issue, index_date, cpi, index_ratio) |>
  left_join(tips_summary, by= c("cusip" = "cusip"))



###3f. Debt###
#The Debt to the Penny dataset provides information about the total outstanding public debt ...#
#...and is reported each day. Debt to the Penny is made up of intragovernmental holdings and debt...#
#...held by the public, including securities issued by the U.S. Treasury. Total public debt outstanding...#
#...is composed of Treasury Bills, Notes, Bonds, Treasury Inflation-Protected Securities (TIPS), ...#
#...Floating Rate Notes (FRNs), and Federal Financing Bank (FFB) securities, as well as Domestic Series,...#
#...Foreign Series, State and Local Government Series (SLGS), U.S. Savings Securities, and...#
#...Government Account Series (GAS) securities. Debt to the Penny is updated at the end of each business...#
#...day with data from the previous business day.#
#https://fiscaldata.treasury.gov/datasets/debt-to-the-penny/debt-to-the-penny#

debt=debt|>
  rename(date=record_date,
         debt_held_public=debt_held_public_amt,
         intragov_hold=intragov_hold_amt,
         debt_outstanding=tot_pub_debt_out_amt) |>
  select(date, debt_held_public, intragov_hold, debt_outstanding) |>
  mutate(date=as.Date(date), 
         debt_held_public=suppressWarnings(as.numeric(debt_held_public)),
         intragov_hold=suppressWarnings(as.numeric(intragov_hold)),
         debt_outstanding=suppressWarnings(as.numeric(debt_outstanding))) 



###3g. Auction###
#The Treasury Securities Auctions Data dataset provides data on announced and auctioned...
#...marketable Treasury securities. The issue date, security type, security term, maturity date, ...#
#...along with other descriptors are provided for the auctioned/announced Treasury securities.#
#Treasury uses an auction process to sell marketable securities and determine their rate, yield, ...#
#...or discount margin. The value of Treasury marketable securities fluctuates with changes in interest...#
#rates and market demand.#
#https://fiscaldata.treasury.gov/datasets/treasury-securities-auctions-data/treasury-securities-auctions-data#
#100s of variables to look through#


###3h. Outstanding###
#Historical Debt Outstanding is a dataset that provides a summary of the U.S. government's...#
#...total outstanding debt at the end of each fiscal year from 1789 to the current year.#
#Between 1789 and 1842, the fiscal year began in January. From January 1842 until 1977, ...#
#...the fiscal year began in July. From July 1977 onwards, the fiscal year has started in October.#
#Between 1789 and 1919, debt outstanding was presented as of the first day of the next fiscal year.#
#From 1920 onwards, debt outstanding has been presented as of the final day of the fiscal year.#
#This is a high-level summary of historical public debt and does not contain a breakdown of the debt components.#
#https://fiscaldata.treasury.gov/datasets/historical-debt-outstanding/historical-debt-outstanding#

outstanding=outstanding |>
  rename(date=record_date, debt=debt_outstanding_amt) |>
  select(date, debt) |>
  mutate(date = as.Date(date)) |>
  mutate(debt= as.numeric(debt))

outstanding


###3i. Spending###
#The Receipts by Department dataset is part of the Combined Statement of Receipts, Outlays, ...#
#...and Balances published by the Bureau of the Fiscal Service at the end of each fiscal year.#
#The Combined Statement is recognized as the official publication of receipts and outlays.#
#This dataset contains department receipt amounts broken out by type, account, and line item.
#https://fiscaldata.treasury.gov/datasets/receipts-by-department/receipts-by-department#

spending=spending |>
  rename(date=record_date, 
         line_item=receipt_line_item_nm, 
         agency_id=aid_cd, 
         amount=receipt_amt) |>
  select(date, line_item, agency_id, amount) |>
  mutate(date=as.Date(date), 
         agency_id=as.numeric(agency_id),
         amount=as.numeric(amount))


###3j. Put all The Above Filtering In One Function Call###
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
             mtd=month_expense_amt,
             ytd=fytd_expense_amt) |>
      select(date, category, group, type, mtd, ytd) |>
      mutate(category=str_replace(category, "INTEREST EXPENSE ON ", "")) |>
      mutate(date=as.Date(date), mtd=as.numeric(mtd), ytd=as.numeric(ytd))
    
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



###3k. Save all objects so don't have to keep querying API###
debt=treasury("debt")
fx=treasury("fx")
gold=treasury("gold")
interest=treasury("interest")
outstanding=treasury("outstanding")
rates=treasury("rates")
spending=treasury("spending")

save(debt, file="debt.R")
save(fx, file="fx.R")
save(gold, file="gold.R")
save(interest, file="interest.R")
save(outstanding, file="outstanding.R")
save(rates, file="rates.R")
save(spending, file="spending.R")

load("debt.R")
load("fx.R")
load("gold.R")
load("interest.R")
load("outstanding.R")
load("rates.R")
load("spending.R")






#####4. Visuals and Summaries#####
###4a. TIPS###

cpi=tips[!duplicated(tips$index_date), ]


###4b. FX###
#maybe make a vector of countries?#
#Make so countries with multiple currencies work#

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
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    theme_bw() +
    theme(plot.title=element_text(hjust=0.5))
}

fx_rate("Germany")
fx_rate("Germany", "Mark")
fx_rate("China")
fx_rate("Canada")
fx_rate("Russia")



###4c. Gold###
gold_location=function(df, default_date="2024-05-31") {
  
  user_date=as.Date(default_date)
  
  closest_date <- df |>
    filter(!is.na(date)) |>
    filter(date <= user_date) |>
    slice(which.max(date - user_date)) |>
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

gold_location(gold)
gold_location(gold, "2022-12-21")




###4d. Rates###
#Maybe put Mark vs non-mark into contingency table with O/U#


###4e. Debt###
debt_plot=function(df, t1="1993-04-01", t2="2024-07-02", type="default") {
  a=df |> 
    filter(date >= t1 & date <=t2) |>
    mutate(trillions=debt_outstanding/1000000000000)
  
  ggplot(a, aes(x=date, y=trillions)) +
    geom_area(fill=555, alpha=0.3) +
    geom_line(col="red", size=1) +
    labs(x="Date", y="Debt Load (Trillions USD)", title="U.S. Federal Debt") +
    theme_bw()+
    theme(plot.title=element_text(hjust=0.5))
}

debt_plot(debt)



###4f. Outstanding###

outstanding_plot=function(df, t1="1993-04-01", t2="2024-07-02") {
  a=df |> 
    filter(date >= t1 & date <=t2) |>
    mutate(billions=debt/1000000000)
  
  ggplot(a, aes(x=date, y=billions)) +
    geom_area(fill=555, alpha=0.3) +
    geom_line(col="red", linewidth=1) +
    labs(x="Date", y="Debt Load (Billions USD)", title="U.S. Federal Debt") +
    ylim(0, max(a$billions)) +
    theme_bw()+
    theme(plot.title=element_text(hjust=0.5))
}

outstanding_plot(outstanding)



###4f. interest###
#Maybe break into group/type over time#


#sharing app by hosting on shinyappis.io#
#also host files on giHub and run locally#
#shiny::runGitHub("<your repo name>", "<your user name>")#


#########scrap#############################################################################################
install.packages("datasauRus")
library(datasauRus) 
library(ggplot2) 
library(gganimate)

p <- ggplot(datasaurus_dozen, aes(x=x,y=y)) +
  geom_point() +
  theme_minimal() +
  transition_states(dataset,3,1) + 
  ease_aes() 

anim_save("myfilename.gif",p)



library(ggplot2)
library(gganimate)

# Generate example data
set.seed(123)
dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
values <- cumsum(rnorm(length(dates)))
data <- data.frame(date = dates, value = values)

# Create ggplot object with animation
p <- ggplot(data, aes(x = date, y = value)) +
  geom_line() +
  labs(title = "Evolution of Values over Time") +
  theme_minimal() +
  transition_reveal(date)

# Animate the plot
animate(p, renderer = gifski_renderer("animation.gif"), nframes = 50, fps = 10)
plot("animation.gif")


interest="v2/accounting/od/interest_expense"            #Interest Payments on debt#
auction="v1/accounting/od/auctions_query"               #treasury auction data#
outstanding="v2/accounting/od/debt_outstanding"         #total debt outstanding#
spending="v1/accounting/od/receipts_by_department"      #spending by dept#

img(src ="treasury.jpg" )


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
  
  #Format resulting data#
  if(data_type=="fx") {
    df=df |>
      select(effective_date, country, currency, exchange_rate) |>
      rename(date=effective_date, rate_per_usd=exchange_rate) |>
      mutate(date=as.Date(date), rate_per_usd=as.numeric(rate_per_usd))
  }
  return(as_tibble(df))
}



