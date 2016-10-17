#' Run Shiny app example
#'
#' Launch a Shiny app which provides an example of the effect of longitude, latitude and axis orientation argument settings on the displayed orthographic globe projection.
#'
#' @export
#'
#' @examples
#' # not run
runOrtho <- function() {
  appDir <- system.file("shiny-examples", "orthographic", package="mapmate")
  if (appDir=="") {
    stop("Could not find app directory. Try re-installing `mapmate`.", call.=FALSE)
  }
  shiny::runApp(appDir, display.mode="normal")
}
