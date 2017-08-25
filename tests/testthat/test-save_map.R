library(mapmate)
suppressMessages({
  library(dplyr)
  library(purrr)
  library(RColorBrewer)
})
context("save_map.R")

data(annualtemps)
pal <- rev(brewer.pal(11, "RdYlBu"))
temps <- mutate(annualtemps, idx = Year - min(Year) + 1)
frame1 <- filter(temps, idx==1) # subset to first frame
id <- "idx"

g1 <- save_map(frame1, z.name="z", id=id, ortho=FALSE, col=pal, type="maptiles", save.plot=FALSE, return.plot=TRUE)
g2 <- save_map(frame1, z.name="z", id=id, col=pal, type="maptiles", save.plot=FALSE, return.plot=TRUE)

data(borders)
n <- 2
borders <- map(1:n, ~mutate(borders, idx = .x))
f <- function(x) save_map(x, id=id, lon=-70, lat=50, n.period=30, n.frames=n,
                          col="orange", type="maplines", save.plot=FALSE, return.plot=TRUE)
gg.list <- map(borders, ~f(.x))
gg.oops <- walk(borders, ~f(.x))

test_that("save_map returns ggplot objects", {
  expect_is(g1, "ggplot")
  expect_is(g2, "ggplot")

  g3a <- save_map(frame1, z.name=NULL, id=id, col=2, type="points",
                  save.plot=F, return.plot=T, ortho=FALSE)
  expect_is(g3a, "ggplot")
  g3b <- save_map(frame1, z.name=NULL, id=id, col=c(2, 3), type="points",
                  save.plot=F, return.plot=T, ortho=FALSE)
  expect_is(g3b, "ggplot")
  expect_equal(g3a, g3b)
  expect_error(save_map(frame1, z.name=NULL, id="a", col=pal, type="points",
                        save.plot=F, return.plot=T, ortho=FALSE),
               "'id' must refer to a column name.")
  g3c <- save_map(frame1, id=id, type="points", save.plot=F, return.plot=T, ortho=FALSE)
  expect_is(g3c, "ggplot")

  geom <- rep(c("polygon", "tile"), each=3)
  con <- rep(c("none", "overlay", "only"), 3)
  for(i in seq_along(geom)){
    expect_error(save_map(frame1, z.name="a", id=id, col=pal, type="density",
                          save.plot=F, return.plot=T, ortho=FALSE),
                 "'z' must refer to a column name.")
    g4 <- save_map(frame1, z.name="z", id=id, col=pal, type="density", save.plot=F,
                   return.plot=T, density.geom=geom[i], contour=con[i], ortho=FALSE)
    expect_is(g4, "ggplot")
    g4 <- save_map(frame1, z.name=NULL, id=id, col=pal, type="density", save.plot=F,
                   return.plot=T, density.geom=geom[i], contour=con[i], ortho=FALSE)
    expect_is(g4, "ggplot")
  }

  expect_is(gg.list, "list")
  expect_equal(length(gg.list), 2)
  expect_is(gg.list[[1]], "ggplot")
  expect_is(gg.list[[2]], "ggplot")

  expect_is(gg.oops, "list")
  expect_equal(length(gg.oops), 2)
  expect_is(gg.oops[[1]], "data.frame")
  expect_is(gg.oops[[2]], "data.frame")
})
