#' Generate a table of location pair samples
#'
#' Expand a table of location samples to location pairs.
#'
#' \code{gc_endpoints} expands a data frame of with longitude and latitude coordinate columns into one with four columns of \code{lon0}, \code{lat0}, \code{lon1} and \code{lat1} representing pairs of locations.
#' This is done in preparation for generating great circle arcs between each pair of locations. \code{gc_endpoints} provides the endpoints of these arcs.
#' See \code{gc_lines}, which is used subsequently on output from \code{gc_endpoints}, to generate an expanded data frame of the corresponding great circle ars.
#'
#' \code{gc_endpoints} merely splits a data frame into its first and second halves of rows and then expands a table that has rows for every combination of pairs between the first and second halves of the original table.
#' This is a handy utility function for preparing data for simulated examples of network maps in the current version of \code{mapmate}.
#' In future versions a more robust \code{gc_endpoints} may offer other more controllable options for assembling location pairs.
#'
#' When \code{keep=TRUE} columns in \code{data} other than those of longitudes and latitudes are similarly appended with \code{0} and \code{1} in the output,
#' corresponding to the columns \code{lon0/lat0} and to \code{lon1/lat1}, respectively.
#'
#' If \code{data} contains \code{n} rows, then the data frame returned by \code{gc_endpoints} will contain \eqn{(n/2)^2} rows.
#'
#' @param data a data frame.
#' @param lon character, the column in \code{data} referring to longitudes.
#' @param lat character, the column in \code{data} referring to latitudes.
#' @param distance logical, include a column of distances between locations along the shorter arc of the implied great circle. Defaults to \code{TRUE}.
#' @param keep logical, retain copies of other columns in \code{data} if \code{data} contained more than longitude and latitude columns. Defaults to \code{TRUE}.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' \dontrun{library(dplyr)
#' data(network)
#' gc_endpoints(network, "lon", "lat")
#' gc_endpoints(network, "lon", "lat", distance=FALSE)
#' gc_endpoints(network, "lon", "lat", keep=FALSE)}
gc_endpoints <- function(data, lon, lat, distance=TRUE, keep=TRUE){
  n <- nrow(data)
  if(n %% 10 == 1) n <- n - 1
  idx <- split(sample(1:n, size=n), rep(1:2, each=n/2))
  idx <- expand.grid(R1=idx[[1]], R2=idx[[2]])
  v0 <- c("lon0", "lat0", "lon1", "lat1")
  v1 <- c("lon0"=lon, "lat0"=lat)
  v2 <- c("lon1"=lon, "lat1"=lat)
  sfx <- c("0", "1")
  if(lon == "lon0") stop("Use a different column name than 'lon0'.")
  if(lat == "lat0") stop("Use a different column name than 'lat0'.")
  y <- data.table::data.table(lon0=data[[lon]][idx$R1], lat0=data[[lat]][idx$R1],
                              lon1=data[[lon]][idx$R2], lat1=data[[lat]][idx$R2]) %>% dplyr::tbl_df()
  if(keep){
    y <- dplyr::left_join(
      dplyr::left_join(y, data, by=v1, suffix=sfx),
      dplyr::left_join(y, data, by=v2, suffix=sfx),
      by=v0, suffix=sfx)
  }
  if(distance){
    distances <- geosphere::distMeeus(dplyr::select_(y, "lon0", "lat0"), dplyr::select_(y, "lon1", "lat1"))
    y <- dplyr::mutate(y, Dist=distances)
  }
  dplyr::sample_n(y, nrow(y))
}

#' Generate a table of great circle arcs
#'
#' Expand a table of pairs of great circle arc endpoint coordinates to a larger table also containing a series of points in between.
#'
#' \code{gc_arcs} takes a data frame as generated by \code{gc_endpoints} and fills in a series of points between each pair of endpoints (each row) in the input data frame.
#' The amount by which this expands the data frame depends on \code{n}, the number of points in between the endpoints. The total points will be \eqn{n+2} if \code{addStartEnd=TRUE}.
#'
#' \code{n} can be different for each arc if a vector is supplied or if \code{n} refers to a column in \code{data}.
#' This is useful, for example, to allow longer arcs to be composed of more points, scaling \code{n} based on some function of distance between endpoints.
#' A scalar integer value can be supplied (defaults to \code{n=50}) if all arcs are to be composed of the same number of points regardless of the distance they cover.
#'
#' In the context of animating a sequence of plot frames, and holding other factors constant,
#' the general rule of thumb is animations will appear to traverse the path of longer great circle arcs with greater speed than of shorter great circle arcs,
#' taking about the same amount of time (number of frames) to complete the journey from endpoint to endpoint because each arc is composed of the same number of points.
#' On the other hand, the more points an arc consists of, the slower it will be traversed.
#' Another factor is the length of each segment along the path of the arc and how much each successive segment is allowed to overlap the previous one from one plot to the next.
#' See \code{gc_paths} regarding this latter factor.
#'
#' Overall, there is an interplay between the number of points composing an entire arc, the segment length (number of points composing the successive subsets of an arc),
#' and degree of segment overlap (how the arc is broken into segments to trace the path along the arc).
#' Longer segments made up of more points means the arc is traversed more quickly.
#' More segment overlap will slow down the journey.
#' More points composing the entire arc shrinks the distance covered by each segment.
#'
#' @param data a data frame.
#' @param lon0 character, the column in \code{data} referring to starting longitudes.
#' @param lat0 character, the column in \code{data} referring to starting latitudes.
#' @param lon1 character, the column in \code{data} referring to ending longitudes.
#' @param lat1 character, the column in \code{data} referring to ending latitudes.
#' @param n an integer scalar or vector, or character. The number of points along the shorter great circle arc between two endpoints in \code{data} or the name of a column in \code{data}. See details.
#' @param breakAtDateLine logical, for flat maps it is important to break a line at the international dateline into two separate lines. Defaults to \code{FALSE}.
#' @param addStartEnd logical, include the original endpoints rather than just the \code{n} points in between them. Defaults to \code{TRUE}.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' \dontrun{library(dplyr)
#' set.seed(192)
#' data(network)
#' distFun <- function(x) 1 - x / max(x) # simple inverse distance weighting
#' endpoints <- gc_endpoints(network, "lon", "lat")
#' endpoints <- mutate(endpoints, Dist_wts=distFun(Dist))
#'
#' # take a weighted sample, e.g., favoring larger averaged populations and shorter distances
#' endpoints <- sample_n(endpoints, 500, replace=TRUE, weight=(Pop_wts0 + Pop_wts1)/2 + Dist_wts)
#'
#' # expand data frame from endpoints to arcs, each composed of a sequence of points
#' arcs <- gc_arcs(endpoints, "lon0", "lat0", "lon1", "lat1")}
gc_arcs <- function(data, lon0, lat0, lon1, lat1, n=50, breakAtDateLine=FALSE, addStartEnd=TRUE){
  x <- list(lon0, lat0)
  y <- list(lon1, lat1)
  if(is.character(n)){
    if(!n %in% names(data)) stop("If 'n' is character, it must refer to a column in 'data'.")
    if(!is.integer(data[[n]])) data[[n]] <- as.integer(data[[n]])
    n <- data[[n]]
  }
  if(length(n) != 1 & length(n) != nrow(data))
    stop("'n' must have length 1 or length equal to the number of rows in 'data'.")
  if(any(n < 1)) stop("Column 'n' must contain positive integers.")
  data <- geosphere::gcIntermediate(dplyr::select_(data, .dots=x), dplyr::select_(data, .dots=y),
                         n=n, breakAtDateLine=breakAtDateLine, addStartEnd=addStartEnd)
  if(!is.list(data)){
    rownames(data) <- NULL
    data <- list(data)
  }
  f <- function(x, idx){
    x <- if(is.list(x)) purrr::map2(
      x, idx + c(0, 0.5), ~data.frame(.x, .y)) %>% dplyr::bind_rows() else data.frame(x, idx)
    x <- stats::setNames(x, c("lon", "lat", "group"))
    dplyr::tbl_df(x)
  }
  purrr::map2(data, seq_along(data), ~f(x=.x, idx=.y)) %>% dplyr::bind_rows()
}

# nolint start
#' Generate a table of incremental great circle arc segments
#'
#' Expand a table of great circle arcs to a larger table of great circle arc segments that sequentially traverse the original arcs.
#'
#' \code{gc_paths} takes a data frame as generated by \code{gc_arcs} and breaks each set of points defining each arc
#' into a sequence of points defining arc segments that cover the entire original arc in order from one endpoint to the other.
#' Segments along the path that covers an arc may overlap to varying degrees based on the segment size, which is variable.
#'
#' \code{group} keeps different network pathways distinct within each plot frame when multiple great circle arcs are traversed simultaneously.
#'
#' \code{size} describes the maximum number of points composing a great circle arc segment.
#' It must be at least \code{2} and is used to sample integers uniformly between \eqn{[2, size]}.
#' This defines the range of values from which the actual segment length is sampled. There are not any other options in the current package version for specifying segment lengths.
#' Length is also fixed across all segments in a given arc, but is allowed to vary between arcs.
#'
#' \code{replicates} simply duplicates a sequence of segments for a given arc, which are then allowed to commence path traversal during unique plot frames
#' based on \code{max.offset}. For example, if \code{replicates=2} and \code{max.offset=2},
#' two instances of the same arc segment path sequence for a specific arc can begin on plot frames \code{0}, \code{1}, or \code{2}.
#' \code{max.offset} must be at least \code{replicates - 1}.
#'
#' For \code{direction="fixed"}, no change are made to \code{data}.
#' \code{direction="reverse"} will switch the order of the points in the arc and \code{direction="random"} gives a probability of 0.5 that order is reversed.
#' This is useful in the context of applying \code{gc_paths} over multiple great circle arcs as well as for simulations.
#'
#' In the context of animating a sequence of plot frames, and holding other factors constant,
#' the general rule of thumb is animations will appear to traverse the path of longer great circle arcs with greater speed than of shorter great circle arcs,
#' taking about the same amount of time (number of frames) to complete the journey from endpoint to endpoint because each arc is composed of the same number of points.
#' On the other hand, the more points an arc consists of, the slower it will be traversed.
#' Another factor is the length of each segment along the path of the arc and how much each successive segment is allowed to overlap the previous one from one plot to the next.
#' See \code{gc_arcs} regarding generating great circle arcs composed of varying numbers of points.
#'
#' Overall, there is an interplay between the number of points composing an entire arc, the segment length (number of points composing the successive subsets of an arc),
#' and degree of segment overlap (how the arc is broken into segments to trace the path along the arc).
#' Longer segments made up of more points means the arc is traversed more quickly.
#' More segment overlap will slow down the journey.
#' More points composing the entire arc shrinks the distance covered by each segment.
#'
#' @param data a data frame.
#' @param group character, the column in \code{data} referring to the grouping variable.
#' @param size integer, the maximum number of points used to define a great circle arc segment.
#' @param replicates integer, the number of replicates of an arc. Defaults to \code{1}.
#' @param direction character. Defaults to \code{fixed}.
#' @param max.offset integer, maximum allowable offset for the plot frame on which great circle arc path traversal commences.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' \dontrun{library(dplyr)
#' set.seed(192)
#' data(network)
#' distFun <- function(x) 1 - x / max(x) # simple inverse distance weighting
#' endpoints <- gc_endpoints(network, "lon", "lat")
#' endpoints <- mutate(endpoints, Dist_wts=distFun(Dist))
#'
#' # take a weighted sample, e.g., favoring larger averaged populations and shorter distances
#' endpoints <- sample_n(endpoints, 500, replace=TRUE, weight=(Pop_wts0 + Pop_wts1)/2 + Dist_wts)
#'
#' # expand data frame from endpoints to arcs, each composed of a sequence of points
#' arcs <- gc_arcs(endpoints, "lon0", "lat0", "lon1", "lat1")
#'
#' paths <- gc_paths(arcs, group="group", size=5)}
gc_paths <- function(data, group, size, replicates=1, direction="fixed", max.offset=0){
  if(missing(group)) stop("Must provided 'group'.")
  if(missing(size)) stop("Must provided 'size'.")
  n.min <- dplyr::group_by_(data, "group") %>% dplyr::summarise(n=n()) %>%
    dplyr::summarise(n=min(n)) %>% unlist
  if (n.min < 3) stop("Insufficient data.")
  if (size < 2)
    stop("Maximum segment size too small; line composition requires at least two points.")
  if(replicates < 1) stop("'replicates' must be >= 1.")
  if(replicates - 1 > max.offset){
    e <- "Replicate paths have uniquely staggerred random starting points (frame IDs);"
    e <- paste(e, "'replicates' must be <= 'max.offset' + 1.")
    stop(e)
  }
  split(data, data[[group]]) %>% purrr::map(
    ~.gc_paths_internal(.x, group, size, replicates, direction, max.offset)) %>%
    dplyr::bind_rows() %>% dplyr::tbl_df()
}

.gc_paths_internal <- function(data, group, size, replicates=1,
                               direction="fixed", max.offset=0){
  n <- nrow(data)
  offset <- sample(0:max.offset, replicates)
  if(direction == "reverse")
    data <- dplyr::mutate(data, lon = rev(lon), lat = rev(lat))
  if(direction == "random" && stats::rnorm(1) < 0)
    data <- dplyr::mutate(data, lon = rev(lon), lat = rev(lat))

  z <- sort(round(stats::runif(2, 2, size)))
  z[z > n] <- n
  n1 <- ceiling(n / z[1]) + 1
  trim <- function(x, min, max) x[x >= min & x <= max]
  idx <- purrr::map(1:n1, ~((z[1] * .x - z[2]):(z[1] * .x) %>% trim(1, n)))
  idx <- idx[unlist(purrr::map(idx, ~length(.x) > 0))]
  data <- purrr::map(idx, ~dplyr::slice(data, .x))

  f <- function(k, data, offset){
    string <- paste0(".%0", ceiling(log(replicates, base=10)), "d")
    purrr::map(offset, ~dplyr::mutate(data, group =
      ifelse(replicates == 1, group, group + as.numeric(sprintf(string, k-1))),
      id = .x + k)) %>% dplyr::bind_rows() %>% dplyr::tbl_df()
  }

  purrr::map2(seq_along(data), data, ~f(.x, .y, offset)) %>%
    dplyr::bind_rows() %>% dplyr::arrange_("group", "id")
}
# nolint end
