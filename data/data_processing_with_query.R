filepath = "~/Projects/dummy_dashboard/dummy_dashboard/"
savepath = "~/Projects/dummy_dashboard/dummy_dashboard/data/"

options(scipen = 999) #no scientific notation, show all numbers

set.seed(90)

library(dplyr)
library(reshape2)
library(stringr)
library(tibble)
library(ggplot2) #we are using 'economics' dataset from ggplot2 as a base for our dummy data

#--------------------------------
#dummy data
#--------------------------------

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

#--------------------------------
#formula 
#--------------------------------

#formula table for calculating average of ratios, because you cannot do (average/average) as it would give equal importance to all cities (numbers for Jakarta will be given same weight as Banjarmasin, for example). so instead you will have to sum all the constituent metrics and then calculate the average by (sum/sum)

formula_raw <- tribble(
  ~metric,                               ~formula,                                                           ~metric_type,
  "percentage_return",                             "return / mid_price",                                   "percentage"
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