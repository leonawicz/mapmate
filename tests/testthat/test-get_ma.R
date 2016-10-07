library(mapmate)
suppressMessages({ library(data.table); library(dplyr) })
context("get_ma [functions.R]")

data(monthlytemps)

test_that("get_ma works with serial processing", {
  res <- "seasonal" # annual, seasonal, monthly
  season <- "winter" # winter, spring, summer, autumn
  idx <- switch(season, winter=c(12,1,2), spring=3:5, summer=6:8, autumn=9:11)
  x <- if(res=="seasonal") dplyr::filter(monthlytemps, Month %in% idx) else monthlytemps
  x0 <- get_ma(data.frame(x), res, season)
  x1 <- get_ma(x, res, season)
  x2 <- get_ma(list(x, x), res, season)
  x3 <- get_ma(x, res, season, format="list")
  expect_is(x0, "data.frame")
  expect_is(x1, "data.frame")
  expect_is(x2, "data.frame")
  expect_identical(x0, x1)
  expect_identical(x1, bind_rows(x3))

  n <- 5
  res <- "annual"
  x <- monthlytemps
  x0 <- get_ma(data.frame(x), res, season, size=n)
  x0b <- get_ma(data.table(x), res, season, size=n)
  x1 <- get_ma(x, res, season)
  x2 <- get_ma(list(x, x), res, season)
  x3 <- get_ma(x, res, season, format="list")
  expect_is(x0, "data.frame")
  expect_is(x0b, "data.frame")
  expect_is(x1, "data.frame")
  expect_is(x2, "data.frame")
  expect_identical(x0, x0b)
  expect_equal(nrow(x0), nrow(x1)+5)
  expect_identical(x1, bind_rows(x3))

  n <- 20
  res <- "monthly"
  x <- monthlytemps
  x0 <- get_ma(data.frame(x), res, season, size=n)
  x0b <- get_ma(data.table(x), res, season, size=n)
  x1 <- get_ma(x, res, season)
  x2 <- get_ma(list(x, x), res, season)
  x3 <- get_ma(x, res, season, format="list")
  expect_is(x0, "data.frame")
  expect_is(x0b, "data.frame")
  expect_is(x1, "data.frame")
  expect_is(x2, "data.frame")
  expect_identical(x0, x0b)
  expect_equal(nrow(x0), nrow(x1)-10*12)
  expect_identical(x1, bind_rows(x3))
})
