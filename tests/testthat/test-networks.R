library(mapmate)
suppressMessages({ library(dplyr) })
context("networks.R")

data(arcs)
d <- arcs

test_that("project_to_hemisphere returns valid output", {
  expect_is(project_to_hemisphere(0,0,0,0), "tbl_df")
  expect_is(project_to_hemisphere(0,0,0,0), "tbl")
  expect_is(project_to_hemisphere(0,0,0,0), "data.frame")
  expect_equal(ncol(project_to_hemisphere(0,0,0,0)), 3)
  expect_equal(nrow(project_to_hemisphere(0,0,0,0)), 1)
  expect_is(project_to_hemisphere(0,0,0,0)$inview, "logical")
  expect_is(project_to_hemisphere(-180:180, seq(-90, 90, length.out=361), 40, -60), "tbl_df")
  expect_equal(nrow(project_to_hemisphere(-180:180, seq(-90, 90, length.out=361), -60, 40)), 361)
  expect_error(project_to_hemisphere(1:360,1:359,0,0), "lon and lat must be equal length")
  expect_error(project_to_hemisphere(1:91,1:91,0,0), "latitudes must be >= -90 and <= 90")
  expect_error(project_to_hemisphere(-181,0,0,0), "longitudes must be >= -180 and <= 180")

  expect_error(arc_paths(d, "lon0", "lat0", "lon1", "lat1", n=c(1,2)), "'n' must have length 1 or length equal to the number of rows in 'data'.")
  expect_error(arc_paths(d, "lon0", "lat0", "lon1", "lat1", n=1), "Column 'n' must contain positive integers.")
  expect_error(arc_paths(d, "lon0", "lat0", "lon1", "lat1", n="a"), "If 'n' is character, it must refer to a column in 'data'.")

  d <- arc_paths(slice(d, 1), "lon0", "lat0", "lon1", "lat1", n=10, addStartEnd=FALSE)
  expect_is(d, "tbl_df")
  expect_equal(nrow(d), 1*(10 + 0))
  expect_equal(ncol(d), 3)
  expect_equal(length(unique(d$group)), 1)

  d <- arc_paths(slice(d, 1:2), "lon0", "lat0", "lon1", "lat1", n=20)
  expect_is(d, "data.frame")
  expect_equal(nrow(d), 2*(20 + 2))
  expect_equal(ncol(d), 3)
  expect_equal(length(unique(d$group)), 2)

  d <- arc_paths(mutate(d, n=Dist_wts*15 + 5) %>% slice(1:3), "lon0", "lat0", "lon1", "lat1", n="n")
  expect_is(d, "data.frame")
  expect_equal(ncol(d), 3)
  expect_equal(length(unique(d$group)), 3)
})

