library(mapmate)
suppressMessages({ library(dplyr) })
context("save_ts [functions.R]")

data(annualtemps)
temps <- mutate(annualtemps, frameID = Year - min(Year) + 1) %>%
  group_by(Year, frameID) %>% summarise(z=mean(z))
xlm <- range(temps$Year)
ylm <- range(temps$z)

g1a <- save_ts(temps, "Year", "z", i=round(nrow(temps)/2), col="red", xlm=xlm, ylm=ylm,
               axes.only=FALSE, axes.space=TRUE, save.plot=FALSE, return.plot=TRUE)
g1b <- save_ts(temps, "Year", "z", i=nrow(temps), col="#0000FF", xlm=xlm, ylm=ylm,
               axes.only=FALSE, axes.space=FALSE, save.plot=FALSE, return.plot=TRUE)
g2a <- save_ts(temps, "Year", "z", i=2, col="black", xlm=xlm, ylm=ylm,
               axes.only=TRUE, axes.space=FALSE, save.plot=FALSE, return.plot=TRUE)
g2b <- save_ts(temps, "Year", "z", i=2, col=1, xlm=xlm, ylm=ylm,
               axes.only=TRUE, axes.space=TRUE, save.plot=FALSE, return.plot=TRUE)

test_that("save_ts returns ggplot objects", {
  expect_is(g1a, "ggplot")
  expect_is(g1b, "ggplot")
  expect_is(g2a, "ggplot")
  expect_is(g2b, "ggplot")
})
