#' Title
#'
#' @param x
#' @param lon
#' @param lat
#' @param distance
#' @param keep
#'
#' @return
#' @export
#'
#' @examples
expand_table <- function(x, lon, lat, distance=TRUE, keep=TRUE){
  n <- nrow(x)
  if(n %% 10 == 1) n <- n - 1
  idx <- split(sample(1:n, size=n), rep(1:2, each=n/2))
  idx <- expand.grid(R1=idx[[1]], R2=idx[[2]])
  v0 <- c("lon0", "lat0", "lon1", "lat1")
  v1 <- c("lon0"=lon, "lat0"=lat)
  v2 <- c("lon1"=lon, "lat1"=lat)
  sfx <- c("0", "1")
  if(lon == "lon0") stop("Use a different column name than 'lon0'.")
  if(lat == "lat0") stop("Use a different column name than 'lat0'.")
  y <- data.table::data.table(lon0=x[[lon]][idx$R1], lat0=x[[lat]][idx$R1],
                              lon1=x[[lon]][idx$R2], lat1=x[[lat]][idx$R2]) %>% dplyr::tbl_df()
  if(keep){
    y <- dplyr::left_join(
      dplyr::left_join(y, x, by=v1, suffix=sfx),
      dplyr::left_join(y, x, by=v2, suffix=sfx),
      by=v0, suffix=sfx)
  }
  if(distance){
    distances <- geosphere::distMeeus(dplyr::select(y, lon0, lat0), dplyr::select(y, lon1, lat1))
    y <- dplyr::mutate(y, Dist=distances)
  }
  dplyr::sample_n(y, nrow(y))
}

#' Title
#'
#' @param data
#' @param lon0
#' @param lat0
#' @param lon1
#' @param lat1
#' @param n
#' @param breakAtDateLine
#' @param addStartEnd
#'
#' @return
#' @export
#'
#' @examples
arc_paths <- function(data, lon0, lat0, lon1, lat1, n=50, breakAtDateLine=FALSE, addStartEnd=TRUE){
  x <- list(lon0, lat0)
  y <- list(lon1, lat1)
  if(is.character(n)){
    if(!n %in% names(data)) stop("If 'n' is character, it must refer to a column in 'data'.")
    if(!is.integer(data[[n]])) data[[n]] <- as.integer(data[[n]])
    n <- data[[n]]
  }
  if(length(n) != 1 & length(n) != nrow(data)) stop("'n' must have length 1 or length equal to the number of rows in 'data'.")
  if(any(n < 1)) stop("Column 'n' must contain positive integers.")
  data <- geosphere::gcIntermediate(dplyr::select_(data, .dots=x), dplyr::select_(data, .dots=y),
                         n=n, breakAtDateLine=breakAtDateLine, addStartEnd=addStartEnd)
  if(!is.list(data)) { rownames(data) <- NULL; data <- list(data) }
  f <- function(x, idx){
    x <- if(is.list(x)) purrr::map2(x, idx + c(0, 0.5), ~data.frame(.x, .y)) %>% dplyr::bind_rows() else data.frame(x, idx)
    x <- setNames(x, c("lon", "lat", "group"))
    tbl_df(x)
  }
  purrr::map2(data, seq_along(data), ~f(x=.x, idx=.y)) %>% dplyr::bind_rows()
}

#' Title
#'
#' @param d
#' @param size
#' @param n.frames
#' @param replicates
#' @param direction
#'
#' @return
#' @export
#'
#' @examples
path_segments <- function(d, size, n.frames, replicates=1, direction="fixed"){
  n <- nrow(d)
  if (n < 3) stop("Data not appropriate for this operation.")
  if (size < 3) stop("Segment size too small.")
  z <- round(runif(2, 2, size))
  z[z > n] <- n
  n1 <- ceiling(diff(c((z[1] - z[2]), n))/z[1])
  if (n.frames - n1 < 100) stop("Insufficient frames")
  offset <- sample(0:(n.frames - n1), replicates)

  f <- function(k, d, n, n1, z, offset){
    ind2 <- z[1] * k
    ind1 <- max(ind2 - z[2], 1)
    if (ind2 > n) ind2 <- n
    d <- dplyr::slice(d, ind1:ind2)
    purrr::map(offset, ~dplyr::mutate(d, group =
      ifelse(replicates == 1, group, group + as.numeric(sprintf(".%d", k))),
      id = .x + k)) %>% dplyr::bind_rows() %>% dplyr::tbl_df()
  }

  if(direction == "reverse") d <- dplyr::mutate(d, long = rev(long), lat = rev(lat))
  if(direction == "random" && rnorm(1) < 0) d <- dplyr::mutate(d, long = rev(long), lat = rev(lat))
  purrr::map(1:n1, ~f(.x, d, n, n1, z, offset)) %>% dplyr::bind_rows %>% dplyr::arrange(group, id)
}
