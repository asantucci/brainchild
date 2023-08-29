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

ANDREAS <- "asantucci"
THOMAS <- "trowley"
USER <- readline(prompt=glue("Enter username from set {ANDREAS} OR {THOMAS}: " ))

# Set working directory.
if (USER == THOMAS) {
  setwd("/Users/thomas/Desktop/stock_research/")
} else if (USER == ANDREAS) {
  setwd("C:/Users/asantucci/Desktop/Wyzant/R/Thomas_Brainchild_Project_Finance/")
}
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
downloaded_tickers <- list.files("ticker_data") %>% gsub("\\.csv", "", .)
to_download <- ticker_timelines[!Ticker %in% downloaded_tickers]
if (0) {
  mapply(GetDataForTicker, ticker = to_download$Ticker, beg_date = to_download$beg_date, end_date = pmin(to_download$end_date + LENGTH_OF_LONG_TERM_TRADE_IN_DAYS, Sys.Date() - 10), MoreArgs = list(path_prefix = "ticker_data"))
}
downloaded_tickers <- list.files("ticker_data") %>% gsub("\\.csv", "", .)
api_errors <- ticker_timelines[!Ticker %in% downloaded_tickers]

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

#fwrite(returns, "C:/Users/asantucci/Desktop/returns.csv")
#returns <- fread("C:/Users/asantucci/Desktop/returns.csv")

# Explore missing trades
unaccounted_representative_trades <- lst[!returns, on = c("TransactionDate", "Ticker", "Representative", "Transaction")]

tickers_to_check <- unaccounted_representative_trades[
    !api_errors,
    on = c("Ticker")
  ][
    TransactionDate < Sys.Date() - LENGTH_OF_LONG_TERM_TRADE_IN_DAYS,
    .(TransactionDate, Ticker)
  ] %>%
  unique()

dts <- sapply(tickers_to_check$Ticker, function(ticker) fread(glue("ticker_data/{ticker}.csv")) %>% CleanStockPrices(), simplify = FALSE)
tickers_to_check[, is_ticker_data_available_for_calculating_expected_returns :=
                   mapply(IsTickerDateAvailableForExpectedReturns, ticker_dt = dts, date = tickers_to_check$TransactionDate) %>%
                   sapply(function(x) length(x) && x)]
# There are ~1.8k ticker-dates for which there were congressional-trades but we don't have data.
tickers_to_check[, sum(!is_ticker_data_available_for_calculating_expected_returns)]
tickers_to_check[(is_ticker_data_available_for_calculating_expected_returns)]  # Returns only 68 ticker-dates for which we supposedly have data...

# Visualize expected returns, excluding outliers.
bounds <- returns[, quantile(return, probs = c(0.01, 0.99), na.rm = TRUE)]
average_return <- returns[data.table::between(return, bounds[1], bounds[2]),
                          .(avg_return = mean(return, na.rm = TRUE)), by = is_short_term_return]
returns[, is_short_term_return := as.factor(is_short_term_return)]
ggplot(returns[data.table::between(return, bounds[1], bounds[2])], aes(x = return, fill = is_short_term_return)) +
  geom_histogram(bins = 100) +
  scale_y_continuous() +
  labs(title = "Univariate Distribution of Expected Returns",
       subtitle = paste("Avg. Short vs. Long term return:", paste(paste0(round(average_return$avg_return, 2), "%"), collapse = ", ")))

ggsave("plots/expected_returns.png", width = 11, height = 8.5)

# Reshape data to wide(r) format, with short and long-term returns in separate columns.
returns[, trade_id := 1:.N, by = .(Ticker, TransactionDate, Representative, is_short_term_return)]
# returns_wide <- dcast(data = returns, Ticker + TransactionDate + Representative + trade_id ~ is_short_term_return, value.var = c('return_percent_per_share', 'profit_in_dollars_per_share'), fill = NA)
setnames(returns_wide, gsub("FALSE", "long_term", colnames(returns_wide)))
setnames(returns_wide, gsub("TRUE", "short_term", colnames(returns_wide)))


# Build Portfolios.
portfolios <- lapply(fnames, function(file)
  AccumulatePortfolio(lst, fread(file) %>% CleanStockPrices())
) %>% rbindlist() %>%
  CalculateReturnsViaPortfolio()

avg_profit <- portfolios[, mean(profit, na.rm=TRUE)]
med_profit <- portfolios[, quantile(profit, probs = 0.5, na.rm = TRUE)]
ggplot(portfolios, aes(x = profit)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(limits = quantile(portfolios$profit, probs = c(0.05, 0.95), na.rm = TRUE)) +
  labs(
    title = glue("The average congressional trade yields a profit of ${round(avg_profit, 2)}\n",
                 "The median still earns a profit of ${round(med_profit, 2)}"),
    subtitle = "Is this expected given the market conditions?"
  )

