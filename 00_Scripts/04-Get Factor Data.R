#Get Factor Base
library(BatchGetSymbols)
#library(GetTDData)
library(reshape2)
library(xts)

first_date <- '2005-01-01' # first date in sample
last_date <- Sys.Date() # set Sys.Date() for current date 
my_ticker <- c('^BVSP') #BOVA11.SA' #'FIXA11.SA', 'USDBRL=X') # Ibovespa ticker (fell free to change to any 
# other from YFinance: ^GSCP, ^FTSE, ITSA3.SA
# check https://finance.yahoo.com/ for more tickers
series_name <- c('Ibovespa')# 'DI',"Dolar") # Name of index/stock that will show up in all plots

## END OPTION

if (!dir.exists('data')) dir.create('data')

l_out <- BatchGetSymbols(tickers = my_ticker, 
                         first.date = first_date, 
                         last.date = last_date)

df_prices <- l_out$df.tickers %>%
  select(ref.date, ticker, price.adjusted) %>%
  group_by(ticker)%>%
  mutate(log_ret = log(price.adjusted/dplyr::lag(price.adjusted) ),
         arim_ret = price.adjusted/dplyr::lag(price.adjusted) - 1) %>%
  na.omit()

#Get NTN-B data
#source("fcts/get_TD_data.R")
#df<-get_td_data("NTN-B", '150850')
#converte p wide
df_prices.wide<-dcast(df_prices, ref.date~ticker, value.var = "log_ret", fun.aggregate = mean)
#remove NaN
df_prices.wide<-df_prices.wide[complete.cases(df_prices.wide),]
#convert to xts
df_prices.wide<-xts(df_prices.wide[,-1], order.by = df_prices.wide$ref.date)

#convert td data to xts
#df<-xts(df[,"yield.bid"],order.by = df$ref.date)

rm(l_out, series_name, first_date, last_date, my_ticker, df_prices)
