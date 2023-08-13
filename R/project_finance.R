.datatable.aware=TRUE

#' Strips away non-numeric characters and creates a column for Lo and Hi amount.
#'
#' @param amount A vector with entries of the form, "$beg - $end"
#' @return A two column data.table describing the lo and high amount.
#' @export
ConvertRange <- function(range) {
  tstrsplit(range, split = "-| - ") %>%
    lapply(gsub, pattern = "\\$", replacement = "",) %>%
    lapply(gsub, pattern = " |,", replacement = "",) %>% # Note that there can also be leading whitespace we need to trim to avoid spurious NA's.
    lapply(as.numeric)
}

#' Downloads data for each ticker.
#'
#' @export
GetDataForTicker <- function(path_prefix, ticker, beg_date, end_date) {
  # Try to download the data
  tryCatch({
    # Download the data and store it in the global environment
    fname <- glue("{path_prefix}/{ticker}.csv")
    if (file.exists(fname)) return("Already fetched data")
    getSymbols(ticker, src = "yahoo", from = beg_date, to = end_date)
    fwrite(data.frame(Date=index(get(ticker)), coredata(get(ticker))), file = fname)
    print(paste("Fetched data for ticker:", ticker, " between dates: ", beg_date, " ", end_date))
    rm(ticker)
  }, error = function(e) {
    # If there's an error, print a message and continue with the next ticker
    print(paste("Failed to download data for ticker:", ticker, " between dates: ", beg_date, " ", end_date))
  })
}

#' Cleans raw stock prices fetched from Yahoo (quantmod) API.
#' @export
CleanStockPrices <- function(stock_prices) {
  setDT(stock_prices)
  ticker_string <- colnames(stock_prices)[2] %>% gsub("\\.Open", "", .)
  stock_prices[, ticker := ticker_string]
  # Clean column names.
  cols <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  setnames(stock_prices,
           old = grep(paste0(cols, collapse = "|"), colnames(stock_prices), value = TRUE),
           new = cols)
  stock_prices[, TransactionDate := as.Date(Date)]
  return(stock_prices)
}

LENGTH_OF_SHORT_TERM_TRADE_IN_DAYS = 30
LENGTH_OF_LONG_TERM_TRADE_IN_DAYS = 180

#' Calculates percent returns for short and long term trades.
#'
#' @export
CalculateReturns <- function(stock_prices, congressional_trades) {
  setDT(stock_prices)
  setDT(congressional_trades)
  # We need all.y = TRUE in the subsequent command to ensure that we can calculate future returns on any given date.
  stock_prices <- merge(congressional_trades, stock_prices, all.y = TRUE)
  shorts <- merge(stock_prices, stock_prices, by.x = c("Ticker", "TransactionDate"), by.y = c("Ticker", "ShortTermReturnDate"),
                  suffixes = c(".future", ".present"))
  longs <- merge(stock_prices, stock_prices, by.x = c("Ticker", "TransactionDate"), by.y = c("Ticker", "LongTermReturnDate"),
                 suffixes = c(".future", ".present"))
  shorts[, short_term_percent_return := 100*(Close.future / Close.present - 1)]
  longs[, long_term_percent_return := 100*(Close.future / Close.present - 1)]
  # As of this point, the "TransactionDate" column in short-and-long term returns data.frame's correspond to the
  # closing date relative to the congressional trade and the time window (30 or 180 days). To match back up with
  # congressional trades, we need to align dates (by subtracting 30 or 180 days).
  shorts[, exact_short_term_date_diff := TransactionDate - TransactionDate.present]
  longs[, exact_long_term_date_diff := TransactionDate - TransactionDate.present]
  returns <- rbind(shorts[, .(Ticker, TransactionDate = TransactionDate - exact_short_term_date_diff, beg_period_price = Close.present, end_price = Close.future, is_short_term_return = TRUE, return = short_term_percent_return)],
                   longs[, .(Ticker, TransactionDate = TransactionDate - exact_long_term_date_diff, beg_period_price = Close.present, end_price = Close.future, is_short_term_return = FALSE, return = long_term_percent_return)], fill = TRUE)
  results <- merge(congressional_trades, returns, by = c("Ticker", "TransactionDate"))
  return(results)
}
