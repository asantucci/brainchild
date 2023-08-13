# Load packages.
library(dplyr)
library(haven)
library(tidyverse)
library(data.table)
library(glue)
library(lubridate)
library(plm)
library(zoo)
library(quantmod)
library(parallel)
library(readxl)
library(bizdays)

library(brainchild)

# Set working directory.
#setwd("/Users/thomas/Desktop/stock_research/")
setwd("C:/Users/asantucci/Desktop/Wyzant/R/Thomas_Brainchild_Project_Finance/")
lst <- read_excel('congress-trading-all.xlsx', sheet = 'Sheet1')

setDT(lst)

lst[, c("LowAmount", "HighAmount") := ConvertRange(Range)]
lst[!is.na(LowAmount) & is.na(HighAmount), HighAmount := LowAmount]

################################################################################
# Get stock data.

# Get the unique tickers from the list of data frames
ticker_timelines <- lst[, .(beg_date = min(TransactionDate), end_date = max(TransactionDate)), by = Ticker]
setorder(ticker_timelines, Ticker)
if (!dir.exists("ticker_data")) {
  dir.create("ticker_data")
}

# Download data for each ticker
#cl <- makeCluster(detectCores(), outfile = "parallel_log.txt")
#clusterCall(cl, fun = function(x) {
#  require(data.table)
#  require(quantmod)
#})
#clusterExport(cl, varlist = c("getSymbols", "index", "coredata", "ticker_timelines"))
#clusterMap(cl, get_data_for_ticker, ticker = ticker_timelines$Ticker, beg_date = ticker_timelines$beg_date, end_date = ticker_timelines$end_date, MoreArgs = list(path_prefix = "."), .scheduling = "dynamic")
#already_scraped <- list.files(path = "ticker_data") %>% gsub("\\.csv", "", .)
#ticker_timelines <- ticker_timelines[!Ticker %in% already_scraped]
ticker_timelines[, end_date := as.Date(end_date)]
mapply(GetDataForTicker, ticker = ticker_timelines$Ticker, beg_date = ticker_timelines$beg_date, end_date = pmin(ticker_timelines$end_date + 180, Sys.Date() - 10), MoreArgs = list(path_prefix = "ticker_data"))

# Collect ticker data into a common DF.
fnames <- list.files(path = "ticker_data", pattern = "csv$", full.names = TRUE)
#dfs <- lapply(fnames, fread) %>% rbindlist(fill=TRUE)

bizdays::load_builtin_calendars()
lst[, TransactionDate := as.Date(TransactionDate)]
lst[, ShortTermReturnDate := adjust.next(TransactionDate + LENGTH_OF_SHORT_TERM_TRADE_IN_DAYS, "weekends")]
lst[, LongTermReturnDate := adjust.next(TransactionDate + LENGTH_OF_LONG_TERM_TRADE_IN_DAYS, "weekends")]

shorts <- merge(lst, lst, by.x = c("Ticker", "TransactionDate"), by.y = c("Ticker", "ShortTermReturnDate"))
longs <- merge(lst, lst, by.x = c("Ticker", "TransactionDate"), by.y = c("Ticker", "LongTermReturnDate"))

lst[, TransactionDate := as.Date(TransactionDate)]
returns <- lapply(fnames, function(fname) {
  print(paste("Now processing", fname))
  CalculateReturns(stock_prices=fread(fname) %>% CleanStockPrices(), congressional_trades=lst)
}) %>% rbindlist()
