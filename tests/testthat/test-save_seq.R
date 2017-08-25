library(mapmate)
suppressMessages({
  library(dplyr)
  library(purrr)
  library(RColorBrewer)
})
context("save_seq [functions.R]")

data(annualtemps)
rng <- range(annualtemps$z, na.rm=TRUE)
pal <- rev(brewer.pal(11, "RdYlBu"))
temps <- mutate(annualtemps, idx = Year - min(Year) + 1)
frame1 <- filter(temps, idx==1) # subset to first frame
id <- "idx"
n <- 2

# wrapping save_map
g1 <- save_seq(frame1, z.name="z", id=id, ortho=FALSE, n.frames=n, col=pal,
               type="maptiles", z.range=rng, save.plot=FALSE, return.plot=TRUE)
g2 <- save_seq(frame1, z.name="z", id=id, n.frames=n, col=pal, type="maptiles",
               z.range=rng, save.plot=FALSE, return.plot=TRUE)

data(borders)
borders <- map(1:n, ~mutate(borders, idx = .x)) %>% bind_rows

gg <- save_seq(borders, id=id, n.frames=n, col="purple", type="maplines",
               save.plot=FALSE, return.plot=TRUE)

# wrapping save_ts
x <- group_by(temps, Year, idx) %>% summarise(z=mean(z))
xlm <- range(x$Year)
ylm <- range(x$z)
gg2 <- save_seq(filter(x, idx <= 3), style="tsline", x="Year", y="z",
                id=id, col="blue", xlm=xlm, ylm=ylm, save.plot=FALSE, return.plot=TRUE)

test_that("save_seq returns ggplot objects from save_map", {
  expect_is(g1, "list")
  expect_is(g2, "list")
  expect_equal(length(g1), 1)
  expect_equal(length(g2), 1)
  expect_is(g1[[1]], "ggplot")
  expect_is(g2[[1]], "ggplot")

  expect_is(gg, "list")
  expect_equal(length(gg), 2)
  expect_is(gg[[1]], "ggplot")
  expect_is(gg[[2]], "ggplot")

  expect_is(gg2, "list")
  expect_equal(length(gg2), 3)
  expect_is(gg2[[1]], "ggplot")
  expect_is(gg2[[2]], "ggplot")
  expect_is(gg2[[3]], "ggplot")
})
