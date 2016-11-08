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
