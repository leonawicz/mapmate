library(mapmate)
suppressMessages({ library(dplyr) })
context("networks.R")

data(network)
distFun <- function(x) 1 - x / max(x) # simple inverse distance weighting
endpoints <- gc_endpoints(network, "lon", "lat") %>% mutate(Dist_wts=distFun(Dist)) %>%
  sample_n(500, replace=TRUE, weight=(Pop_wts0 + Pop_wts1)/2 + Dist_wts)
d <- gc_arcs(endpoints, "lon0", "lat0", "lon1", "lat1")

test_that("gc_arcs returns valid output", {
  expect_error(gc_arcs(d, "lon0", "lat0", "lon1", "lat1", n=c(1,2)), "'n' must have length 1 or length equal to the number of rows in 'data'.")
  expect_error(gc_arcs(d, "lon0", "lat0", "lon1", "lat1", n=1), "Column 'n' must contain positive integers.")
  expect_error(gc_arcs(d, "lon0", "lat0", "lon1", "lat1", n="a"), "If 'n' is character, it must refer to a column in 'data'.")

  d <- gc_arcs(slice(d, 1), "lon0", "lat0", "lon1", "lat1", n=10, addStartEnd=FALSE)
  expect_is(d, "tbl_df")
  expect_equal(nrow(d), 1*(10 + 0))
  expect_equal(ncol(d), 3)
  expect_equal(length(unique(d$group)), 1)

  d <- gc_arcs(slice(d, 1:2), "lon0", "lat0", "lon1", "lat1", n=20)
  expect_is(d, "data.frame")
  expect_equal(nrow(d), 2*(20 + 2))
  expect_equal(ncol(d), 3)
  expect_equal(length(unique(d$group)), 2)

  d <- gc_arcs(mutate(d, n=Dist_wts*15 + 5) %>% slice(1:3), "lon0", "lat0", "lon1", "lat1", n="n")
  expect_is(d, "data.frame")
  expect_equal(ncol(d), 3)
  expect_equal(length(unique(d$group)), 3)
})

