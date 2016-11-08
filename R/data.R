#' Spatially aggregated example global bathymetry surface
#'
#' A bathymetry data set containing columns of longitude, latitude and depths/elevations relative to sea level,
#' modified from the 10-minute resolution source bathymetry map downloaded via the \code{marmap} package.
#'
#' @format A data table with 5832 rows and 3 variables:
#' \describe{
#'   \item{lon}{longitude}
#'   \item{lat}{latitude}
#'   \item{z}{depth below sea level}
#' }
#' @source \url{https://cran.r-project.org/web/packages/marmap/index.html}
"bathymetry"

#' Global national political borders
#'
#' A data set containing global national political boundary lines, modified from the world map in the \code{maps} package.
#'
#' @format A data frame with 99338 rows and 6 variables:
#' \describe{
#'   \item{lon}{longitude}
#'   \item{lat}{latitude}
#'   \item{group}{grouping for polygon lines}
#'   \item{order}{order of lines in group}
#'   \item{region}{boundary regions}
#'   \item{subregion}{boundary subregions}
#' }
#' @source \url{https://cran.r-project.org/web/packages/maps/index.html}
"borders"

#' 2010-2099 global projected annual average temperature anomalies
#'
#' A data set containing low spatial resolution 2010-2099 global projected annual average temperatures.
#' This is a toy data set used for example purposes.
#' Anomalies are element-wise (cell by cell) delta change from a historical baseline.
#'
#' @format A data table with 55080 rows and 4 variables:
#' \describe{
#'   \item{lon}{longitude}
#'   \item{lat}{latitude}
#'   \item{Year}{year}
#'   \item{z}{annual average temperature in degrees Celcius}
#' }
#' @source \url{https://github.com/leonawicz/mapmate}
"annualtemps"

#' 2010-2099 single map grid cell projected monthly average temperatures anomalies
#'
#' A data set containing 2010-2099 projected annual average temperature anomalies for a single low resolution grid cell
#' taken from a global monthly data set also used as the basis for the \code{annualtemps} data set in this package.
#' This is a toy data set used for example purposes.
#' This data set includes data from only one grid cell because it is sufficient for examples but also keeps the size of the data small.
#' Anomalies are delta change from a historical baseline.
#'
#' @format A data table with 1080 rows and 5 variables:
#' \describe{
#'   \item{lon}{longitude}
#'   \item{lat}{latitude}
#'   \item{Year}{year}
#'   \item{Month}{January-December month index 01 through 12}
#'   \item{z}{annual average temperature in degrees Celcius}
#' }
#' @source \url{https://github.com/leonawicz/mapmate}
"monthlytemps"

#' Simulated set of world cities locations and population-based weights
#'
#' A data set containing simulated world cities locations and population-based weights.
#' This is a toy data set used for example purposes.
#' Sampling and weights are based on the square root of the original populations and are rescaled and rounded.
#'
#' @format A data table with 1000 rows and 3 variables:
#' \describe{
#'   \item{lon}{longitude}
#'   \item{lat}{latitude}
#'   \item{Pop_wts}{population-based weights}
#' }
#' @source \url{https://github.com/leonawicz/mapmate}
"network"
