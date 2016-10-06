#' Spatially aggregated example global bathymetry surface
#'
#' A bathymetry dataset containing columns of longitude, latitude and depths/elevations relative to sea level,
#' modified from the 10-minute resolution source bathymetry map downloaded via the \code{marmap} package.
#'
#' @format A data table with 5832 rows and 3 variables:
#' \describe{
#'   \item{long}{longitude}
#'   \item{lat}{latitude}
#'   \item{z}{depth below sea level}
#' }
#' @source \url{https://cran.r-project.org/web/packages/marmap/index.html}
"bathymetry"

#' Global national political borders.
#'
#' A dataset containing global national political boundary lines, modified from the world map in the \code{maps} package.
#'
#' @format A data frame with 99338 rows and 6 variables:
#' \describe{
#'   \item{long}{longitude}
#'   \item{lat}{latitude}
#'   \item{group}{grouping for polygon lines}
#'   \item{order}{order of lines in group}
#'   \item{region}{boundary regions}
#'   \item{subregion}{boundary subregions}
#' }
#' @source \url{https://cran.r-project.org/web/packages/maps/index.html}
"borders"
