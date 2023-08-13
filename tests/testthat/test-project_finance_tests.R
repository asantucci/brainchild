library(brainchild)
library(data.table)
library(testthat)

test_that("ConvertRange works as intended.", {
  expect_equal(list(5000, 10000),
               ConvertRange("$5,000 - $10,000"))
})

test_that("ConvertRange works when no upper-bound specified.", {
  expect_equal(list(1500),
               ConvertRange("$1,500"))
})

test_that("CleanStockPrices works as intended.", {
  raw_stock_prices <- structure(
    list(Date = "2022-01-14",
         X0QZI.IL.Open = 180,
         X0QZI.IL.High = 200,
         X0QZI.IL.Low = 174,
         X0QZI.IL.Close = 185,
         X0QZI.IL.Volume = 100,
         X0QZI.IL.Adjusted = 170),
    class = "data.frame"
  )
  setDT(raw_stock_prices)
  stock_prices <- CleanStockPrices(raw_stock_prices)
  expect_contains(colnames(stock_prices), "ticker")
  expect_length(unique(stock_prices$ticker), 1)
})

test_that("CalculateReturns works on a simple test case: 1 Representative and 1 Trade", {
  origin_date <- as.Date("2023-01-01")
  clean_stock_prices <- data.frame(
    "TransactionDate" = c(origin_date, origin_date + LENGTH_OF_SHORT_TERM_TRADE_IN_DAYS, origin_date + LENGTH_OF_LONG_TERM_TRADE_IN_DAYS),
    "Ticker" = "A",
    "Close" = c(100, 110, 150)
  )
  congressional_trades <- data.frame(
    "Representative" = "Joseph",
    "Ticker" = "A",
    "TransactionDate" = as.Date("2023-01-01"),
    "ShortTermReturnDate" = origin_date + LENGTH_OF_SHORT_TERM_TRADE_IN_DAYS,
    "LongTermReturnDate" = origin_date + LENGTH_OF_LONG_TERM_TRADE_IN_DAYS,
    "Transaction" = "Purchase",
    "Amount" = 1
  )
  expected_output <- data.frame(
    "Ticker" = "A",
    "TransactionDate" = as.Date("2023-01-01"),
    "Representative" = "Joseph",
    "ShortTermReturnDate" = origin_date + LENGTH_OF_SHORT_TERM_TRADE_IN_DAYS,
    "LongTermReturnDate" = origin_date + LENGTH_OF_LONG_TERM_TRADE_IN_DAYS,
    "Transaction" = "Purchase",
    "Amount" = 1,
    "beg_price" = 100,
    "end_price" = c(110, 150),
    "is_short_term_return" = c(TRUE, FALSE),
    "return" = c(10, 50)
  )
  expect_equal(CalculateReturns(clean_stock_prices, congressional_trades), expected_output, ignore_attr=TRUE)
})
