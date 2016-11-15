library(mapmate)
suppressMessages({ library(dplyr) })
context("networks.R")

data(network)

test_that("gc_endpoints returns valid output", {
  expect_error(gc_endpoints(network, "lon0", "lat0"), "Use a different column name than 'lon0'.")
  expect_error(gc_endpoints(network, "lon", "lat0"), "Use a different column name than 'lat0'.")

  d <- gc_endpoints(network, "lon", "lat")
  expect_is(d, "tbl_df")
  expect_equal(nrow(d), nrow(network)^2)
  expect_equal(ncol(d), 4)

  d <- gc_endpoints(network, "lon", "lat", distance=FALSE)
  expect_is(d, "tbl_df")
  expect_equal(nrow(d), nrow(network)^2)
  expect_equal(ncol(d), 3)

  d <- gc_endpoints(network, "lon", "lat", keep=FALSE)
  expect_is(d, "tbl_df")
  expect_equal(nrow(d), nrow(network)^2)
  expect_equal(ncol(d), 3)

  d <- gc_endpoints(network, "lon", "lat", distance=FALSE, keep=FALSE)
  expect_is(d, "tbl_df")
  expect_equal(nrow(d), nrow(network)^2)
  expect_equal(ncol(d), 2)
})

distFun <- function(x) 1 - x / max(x) # simple inverse distance weighting
d0 <- gc_endpoints(network, "lon", "lat") %>% mutate(Dist_wts=distFun(Dist)) %>%
  sample_n(500, replace=TRUE, weight=(Pop_wts0 + Pop_wts1)/2 + Dist_wts)

test_that("gc_arcs returns valid output", {
  expect_error(gc_arcs(d0, "lon0", "lat0", "lon1", "lat1", n=c(1,2)), "'n' must have length 1 or length equal to the number of rows in 'data'.")
  expect_error(gc_arcs(d0, "lon0", "lat0", "lon1", "lat1", n=0), "Column 'n' must contain positive integers.")
  expect_error(gc_arcs(d0, "lon0", "lat0", "lon1", "lat1", n="a"), "If 'n' is character, it must refer to a column in 'data'.")

  d <- gc_arcs(slice(d0, 1), "lon0", "lat0", "lon1", "lat1", n=10, addStartEnd=FALSE)
  expect_is(d, "tbl_df")
  expect_equal(nrow(d), 1*(10 + 0))
  expect_equal(ncol(d), 3)
  expect_equal(length(unique(d$group)), 1)

  d <- gc_arcs(slice(d0, 1:2), "lon0", "lat0", "lon1", "lat1", n=20)
  expect_is(d, "data.frame")
  expect_equal(nrow(d), 2*(20 + 2))
  expect_equal(ncol(d), 3)
  expect_equal(length(unique(d$group)), 2)

  d <- gc_arcs(mutate(d0, n=Dist_wts*15 + 5) %>% slice(1:3), "lon0", "lat0", "lon1", "lat1", n="n")
  expect_is(d, "data.frame")
  expect_equal(ncol(d), 3)
  expect_equal(length(unique(d$group)), 3)
})

