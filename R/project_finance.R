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
#' Uses a try-catch to check if there's any sort of error when fetching the data
#' and writing to desk.
#'
#' @param path_prefix A path prefix denoting where to write the data files
#'     to disk.
#' @param ticker A string describing a ticker.
#' @param beg_date A date describing when to start fetching data from.
#' @param end_date A date describing when to finish collecting data.
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
#'
#' The raw stock prices are fetched with column names that reflect the Ticker.
#' We'd like to normalize the columns before joining together datasets from
#' many different tickers. We perform minimal formatting on these datasets.
#'
#' @param stock_prices A raw file describing daily prices for a given ticker.
#' @returns A data.frame with columns that are guaranteed to be consistent
#'     across invocations of this helper function with different inputs.
#' @export
CleanStockPrices <- function(stock_prices) {
  setDT(stock_prices)
  ticker_string <- colnames(stock_prices)[2] %>% gsub("\\.Open", "", .)
  stock_prices[, Ticker := ticker_string]
  # Clean column names.
  cols <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  setnames(stock_prices,
           old = grep(paste0(cols, collapse = "|"), colnames(stock_prices), value = TRUE),
           new = cols)
  stock_prices[, TransactionDate := as.Date(Date)]
  return(stock_prices)
}

#' Defines the number of days for a "short-term" trade.
#'
#' @export
LENGTH_OF_SHORT_TERM_TRADE_IN_DAYS = 30

#' Defines the number of days for a "long-term" trade.
#'
#' @export
LENGTH_OF_LONG_TERM_TRADE_IN_DAYS = 180

#' Calculates percent returns for short and long term trades.
#'
#' Takes as input a data.frame of stock prices (cleaned and well-formatted
#' according to `CleanStockPrices()` above), as well as a data.frame
#' describing Congressional Trades. Examines the expected yield of a short
#' and long term trade.
#'
#' NOTE: This function currently doesn't consider the TRANSACTION column.
#' I.e. whether the representative is buying or selling shares.
#' This may be relevant in so far as: if a representative is selling their last
#' set of shares, then they can't make any future profits.
#'
#' TODO: discuss with Thomas what the research question is, and learn how
#' to more appropriately incorporate this "Transaction" column into our
#' analysis.
#'
#' @param stock_prices A data.frame containing Tickers and Dates and Closing
#'     prices for each day.
#' @param congressional_trades A data.frame with 1-row per representative
#'     x ticker x trade combination. Note that a representative can make
#'     multiple transactions for the same ticker on the same day, in which
#'     case that grouping of rows would repeat (with possibly different
#'     Transaction types or amounts...).
#' @returns A data.frame describing the expected profit per trade per
#'     congressional individual.
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
