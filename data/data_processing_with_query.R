sysenv = Sys.getenv()

if('XPC_SERVICE_NAME' %in% names(unlist(sysenv))) {
  #if (sysenv[['UDEPLOY_DEPLOYMENT_NAME']] %in% c('production', 'staging')) {
  savepath <- "~/Projects/dummy_dashboard/dummy_dashboard/data/"
  filepath <- "~/Projects/dummy_dashboard/dummy_dashboard/"
  #}
} else {
  #filepath <- "/home/rstudio/ShinyApps/dummy_dashboard/"
  #savepath <- "/home/rstudio/ShinyApps/dummy_dashboard/data/"
  filepath <- "/home/rstudio/ShinyApps/dummy_dashboard/"
  savepath <- "/home/rstudio/ShinyApps/dummy_dashboard/data/"
  #.libPaths("/usr/local/lib/R/site-library")
  #preloaded.pkgs <- c("shiny", "htmltools", "httpuv", "digest", "xtable", "mime", "R6", "Rcpp")
  #sapply(preloaded.pkgs, unloadNamespace)
}

options(scipen = 999) #no scientific notation, show all numbers

set.seed(90)

library(dplyr)
library(reshape2)
library(stringr)
library(tibble)
library(quantmod)
library(purrr)
library(ggplot2) #we are using 'economics' dataset from ggplot2 as a base for our dummy data

#--------------------------------
#dummy data (quantmod)
#--------------------------------

#symbols = c("XAU/USD", "XPT/USD", "USD/JPY", "CAD/USD", "AUD/USD", "NZD/USD", "EUR/USD")

symbols = c("AAPL", "GOOG", "AMZN", "TSLA", "MSFT", "FB", "IBM")
symbols_char = str_replace(symbols, "[^A-Z]", "")

getSymbols(symbols, src = "yahoo", from = "2017-08-01")
#getSymbols(symbols, src = "oanda")

all_data = lapply(symbols_char, function(x) {get(x)})
#all_data = do.call(merge, lapply(symbols_char, function(x) get(x)))

clean_xts = function(xts_list) {
  
  to_df = function(xts) {
    df = data.frame(xts) %>%
      rownames_to_column() %>%
      rename(date = rowname) %>%
      mutate(date = as.Date(date)) 
    
    split_colnames = str_split(names(df), "\\.")
    
    ticker_symbol = lapply(split_colnames, function(split_colname) {
      if(length(split_colname) > 1) {
        split_colname[1] #this is the ticker symbol
      }}) %>%
      unlist() %>%
      unique()
    
    replacement_colnames = lapply(split_colnames, function(split_colname) {
      if(length(split_colname) > 1) {
        split_colname[2] #this is the coltype (eg. Close, Volume, Adjusted)
      } else {
        split_colname[1] #this is 'date' column that we also want to retain
      }}) %>%
      unlist() %>%
      str_to_lower()
    
    names(df) = replacement_colnames
    
    df = df %>%
      mutate(ticker = ticker_symbol)
    
    return(df)
  }
  
  lapply(xts_list, to_df)
}

all_data_df_list = clean_xts(all_data)
all_data_df = reduce(all_data_df_list, full_join) #recursively join all list elements
#all_data_df = data.frame(all_data)
#all_data_df$date = index(all_data)

portfolio = all_data_df %>%
  mutate(daily_growth = round(close - lag(close), 4)
         , percentage_daily_growth = round((daily_growth / close), 4)
         , ticker = as.factor(ticker)) %>%
  filter(!is.na(daily_growth)) %>%
  select(date, ticker, open, percentage_daily_growth, volume, everything()) %>%
  rename(instrument = ticker) %>%
  as.tibble()

#portfolio = all_data_df %>%
#  melt(id.vars = "date") %>%
#  rename(instrument = variable
#         , daily_price = value) %>%
#  mutate(volume = )

#--------------------------------
#dummy data (not used as data is not good looking and unnecessarily big for a minimal example)
#--------------------------------
function() {
  portfolio = economics %>% 
    mutate(date = seq(as.Date("2016-06-02"), as.Date("2017-12-27"), by = 1)) %>% 
    rename(XAUUSD = pce, USDIDR = pop, AUDUSD = psavert, USDCAD = uempmed, USDJPY = unemploy) %>%
    melt(id = c("date")) %>%
    rename(instrument = variable
           , ask_price = value) %>%
    mutate(bid_price = round(ask_price * (1 - round(rnorm(n(), mean = 0, sd = 1) / 100, 5)), 1)
           , price_difference = ask_price - bid_price
           , mid_price = (ask_price + bid_price) / 2
           , return = mid_price - lag(mid_price)
           , percentage_return = round((return / mid_price), 4)
           , volume = round(abs(rnorm(n(), mean = 0, sd = 1)) * abs(return) * 2000, 3)) %>%
    filter(!is.na(return)) %>%
    group_by(instrument) %>%
    mutate(cumulative_return = cumsum(return)) %>%
    dplyr::select(date, instrument, ask_price, percentage_return, return, cumulative_return, volume, ask_price, bid_price, price_difference, mid_price) %>%
    as.tibble()
}

#--------------------------------
#formula 
#--------------------------------

#formula table for calculating average of ratios, because you cannot do (average/average) as it would give equal importance to all cities (numbers for Jakarta will be given same weight as Banjarmasin, for example). so instead you will have to sum all the constituent metrics and then calculate the average by (sum/sum)

#formula_raw <- tribble(
#  ~metric,                        ~formula,                           ~metric_type,
#  "percentage_return",            "return / mid_price",                "percentage"
#)

formula_raw <- tribble(
  ~metric,                        ~formula,                           ~metric_type,
  "percentage_daily_growth",            "daily_growth / close",                "percentage"
)

variable_extractor = function(string) {
  (str_split(string, " ") %>% unlist()) %>%
    .[str_which(., "[a-z]")]
}

formula_rds = formula_raw %>%
  mutate(constituents = lapply(formula, variable_extractor))

#-------------------------
#saving to rds
#-------------------------
objects = c("portfolio", "formula_rds")

sapply(objects, function(object) {save(list = object, file = paste0(savepath, object, '.rds'))} )