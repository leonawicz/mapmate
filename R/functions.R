globalVariables(c(".", "inview", "mo", "Year", "Month", "lon", "lat", "z", "group", "frameID", "Mean"))
#' Obtain moving average map series
#'
#' Obtain a moving average for monthly, annual or seasonal resolution data from monthly map data.
#'
#' \code{get_ma} takes a single data frame or a list of data frames.
#' A list is useful for example if the table is very large and is pre-split into a list of smaller data frames for parallel processing on a Linux cluster with many CPU cores available.
#' If \code{x} is a data frame rather than a data table, it will be converted to a data table.
#'
#' For each data frame \code{get_ma} computes and returns the moving or rolling average,
#' after first summarizing monthly data to seasonal or annual averages if applicable.
#' Winter begins in December and each season is three consecutive months (DJF, MAM, JJA, SON).
#' Whether \code{x} is a data frame or list of data frames, the output can be returned as a single data frame or list of data frames
#'
#' Parallel processing is Linux-only (uses \code{mclapply}) and the default of 32 CPU cores will have to be changed if you don't have that many.
#' It is convenient for my usage and this package version is not intended for general use.
#' Most other users will probably not be using parallel processing at all, in which case \code{n.cores} is ignored.
#'
#' @param x a data frame (or data table) or a list of these.
#' @param type character, one of \code{"monthly"}, \code{"annual"}, or \code{"seasonal"}.
#' @param season \code{NULL} or character, one of \code{"winter"}, \code{"spring"}, \code{"summer"}, or \code{"autumn"}. Default is \code{NULL}.
#' @param size number of years for the moving average window. Default \code{10}.
#' @param format return results as a single data frame with \code{format="table"} (default)
#' or a as a list split either on unique year for seasonal and annual moving averages or on unique year and month combination for monthly moving averages.
#' @param use_mclapply use \code{mclapply} from the \code{parallel} package (Linux). Defaults to \code{FALSE}.
#' @param mc.cores number of CPUs for parallel processing when \code{use_mclapply=TRUE}.
#'
#' @return returns a single data frame or a list of data frames.
#' @export
#'
#' @examples
#' data(monthlytemps)
#' res <- "seasonal" # annual, seasonal, monthly
#' season <- "winter" # winter, spring, summer, autumn
#' idx <- switch(season, winter=c(12,1,2), spring=3:5, summer=6:8, autumn=9:11)
#' if(res=="seasonal") monthlytemps <- dplyr::filter(monthlytemps, Month %in% idx)
#' get_ma(monthlytemps, res, season)
#' get_ma(list(monthlytemps, monthlytemps), res, season)
#' get_ma(monthlytemps, res, season, format="list")
#'
#' # not run
#' \dontrun{get_ma(list(monthlytemps, monthlytemps), res, season, use_mclapply=T, mc.cores=2)}
#' @importFrom magrittr %>%
#' @importFrom grDevices png dev.off
get_ma <- function(x, type, season=NULL, size=10, format="table", use_mclapply=FALSE, mc.cores=32){
  if(!(type %in% c("monthly", "annual", "seasonal"))) stop("invalid type")
  if(!format %in% c("table", "list")) stop("format must be 'table' or 'list'")
  is_list <- "list" %in% class(x)
  if(!is_list && "data.table" %in% class(x)) x <- dplyr::tbl_df(x)
  if(type=="monthly"){
    f <- function(x, size) dplyr::group_by(x, Month, lon, lat) %>%
      dplyr::mutate(z=RcppRoll::roll_mean(z, size, fill=NA)) %>%
      dplyr::filter(!is.na(z))
  }
  if(type=="annual"){
    f <- function(x, size) dplyr::group_by(x, lon, lat, Year) %>% dplyr::summarise(z=mean(z)) %>%
      dplyr::mutate(z=RcppRoll::roll_mean(z, size, fill=NA)) %>% dplyr::filter(!is.na(z))
  }
  if(type=="seasonal"){
    if(is.null(season) || !(season %in% c("winter", "spring", "summer", "autumn")))
      stop("If res='seasonal', season must be 'winter', 'spring', 'summer' or 'autumn'.")
    idx <- switch(season, winter=c(12,1,2), spring=3:5, summer=6:8, autumn=9:11)
    yr.lim <- if(is_list) range(x[[1]]$Year) else range(x$Year)
    f <- function(x, size) {
      x <- dplyr::mutate(x, Year=ifelse(Month==12, Year+1, Year), Month=ifelse(Month %in% idx, 1, 0)) %>%
        dplyr::filter(Year > yr.lim[1] & Year <= yr.lim[2] & Month==1) %>%
        dplyr::group_by(lon, lat, Month, Year) %>% dplyr::summarise(z=mean(z)) %>%
        dplyr::mutate(z=RcppRoll::roll_mean(z, size, fill=NA)) %>% dplyr::ungroup() %>%
        dplyr::mutate(Month=NULL) %>% dplyr::filter(!is.na(z))
    }
  }
  if(!is_list){
    x <- f(x, size)
  } else if(use_mclapply) {
    x <- parallel::mclapply(x, f, size=size, mc.cores=mc.cores)
  } else x <- purrr::map(x, ~f(.x, size))
  x <- dplyr::bind_rows(x) %>% dplyr::ungroup()
  if(format=="table") return(x)
  x <- if(type %in% c("seasonal", "annual")) x %>% split(.$Year) else x %>% split(paste(.$Year, .$Month+9))
  x
}

#' Identidy visible points on an arbitrary global hemishpere view.
#'
#' Given a global hemispheric field of view defined by a single latitudinal and longitudinal centroid focal point, project geographic points onto the hemishpere.
#'
#' \code{project_to_hemisphere} identifies whether each pair of coordinates in the \code{lat} and \code{lon} vectors is in a field of view defined by a centroid focal point \code{(lat0, lon0)}
#' and returns a data frame containing the original coordinates and a column indicating if the coordinates are in the field of view (\code{TRUE} or \code{FALSE}).
#'
#' @param lon vector of longitudes.
#' @param lat vector of latitudes.
#' @param lon0 longitude of focus coordinates.
#' @param lat0 latitude of focus coordinates.
#'
#' @return returns a data frame.
#' @export
#'
#' @examples
#' lon <- seq(-180, 180, length.out=60)
#' lat <- rep(seq(-90, 90, length.out=30), 2)
#' project_to_hemisphere(lon, lat , 0, 0)
project_to_hemisphere <- function(lon, lat, lon0, lat0){
  if(length(lon)!=length(lat)) stop("lon and lat must be equal length")
  if(lat0 < -90 || lat0 > 90 | any(lat < -90 | lat > 90)) stop("latitudes must be >= -90 and <= 90")
  if(lon0 < -180 || lon0 > 180 | any(lon < -180 | lon > 180)) stop("longitudes must be >= -180 and <= 180")
  hold <- cbind(lon, lat)
  x <- purrr::map(list(lat, lat0, lon-lon0), ~.x*pi/180)
  inview <- sin(x[[1]])*sin(x[[2]]) + cos(x[[1]])*cos(x[[2]])*cos(x[[3]]) > 0
  data.frame(lon=hold[,1], lat=hold[,2], inview=inview) %>% dplyr::tbl_df()
}

#' Pad the end of list of data frames
#'
#' Use recycling to pad the end of a list of data frames where data frame elements in the list repeat in a cyclical pattern.
#'
#' \code{pad_frames} is used on lists of data frames where sequence of data frames contains content that repeats over the list.
#' It is used in cases where the data frames contain map data (long, lat, and z), for example describing the visible hemisphere surface of a rotating globe.
#' The number of iterations in a full rotation may not be factorable by the length of the time series of map data (length of list).
#' For example, a rotating globe animation may complete 10 rotations with 60 frames per rotation, using 600 frames total,
#' but the list \code{x} may contain 550 data frames.
#' With \code{rotation="pad"}, the last data frame in \code{x} is recycled to pad the series out to 600 frames so that the animation can display
#' and "hang" on the terminal map data set until a final complete rotation is completed.
#' With \code{rotation="add"}, a full period is added to the end of the \code{x} rather than padding only far enough to make the length of the data series factorable by the rotation period length.
#'
#' @param x list of data frames.
#' @param id character, column name referring to column of \code{x} representing frame sequence integer IDs.
#' @param n.period An integer, the known period of rotation that will be part of an animation in which the map data frames in \code{x} will be sequentially plotted over. Default is 360 (1-degree increment rotations).
#' @param rotation character, one of \code{"add"} or \code{"pad"}.
#' @param force When the length of \code{x} is greater than or equal to \code{n.period} still force padding to occur. Defaults to \code{TRUE}. Otherwise return \code{x}.
#'
#' @return returns \code{x} but padded with it's final element appended repeatedly based on a specified period and type of padding method.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' data(annualtemps)
#' x <- map(1:4, ~mutate(filter(annualtemps, Year-2009==.x), idx=.x))
#' n <- 6
#' pad_frames(x, id="idx", n.period=n, rotation="add")
#' pad_frames(x, id="idx", n.period=n, rotation="pad")
pad_frames <- function(x, id, n.period=360, rotation="add", force=TRUE){
  if(!is.list(x) || is.data.frame(x)) stop("'x' must be a list.")
  if(missing(id)) stop("'id' column is missing.")
  if(!id %in% names(x[[1]])) stop("'id' must refer to a column name.")
  n <- length(x)
  if(n >= n.period & !force) return(x)
  if(id != "frameID") x <- purrr::map(x, ~rename_(.x, frameID=id))
  if(rotation=="add") x2 <- purrr::map(1:(n.period-1), ~x[[n]] %>% dplyr::mutate(frameID=.x + n))
  if(rotation=="pad") x2 <- purrr::map(1:(n.period-n), ~x[[n]] %>% dplyr::mutate(frameID=.x + n))
  x <- c(x, x2)
  if(id != "frameID"){
    idx <- which(names(x[[1]])=="frameID")
    f <- function(x, idx, id){ names(x)[idx] <- id; x }
    x <- purrr::map(x, ~f(.x, idx, id))
  }
  x
}

#' Generate a sequence of coordinates
#'
#' Generate a repeating sequence of longitude and latitude coordinates based on a period of rotation.
#'
#' \code{lon} and \code{lat} may be scalars or vectors. If scalar, \code{lat} is simply repeated \code{n.frames} times.
#' If scalar, \code{lon} is always treated as a starting longitude and a rotational sequence of longitudes is beginning from \code{lon}
#' is generated for a length equal to \code{n.frames}, repeating the sequence if necessary.
#' If \code{lon} or \code{lat} are vectors, it is assumed that predefined custom sequences of longitude and latitude have been provided,
#' so they are required to be of length \code{n.period}.
#' However long it takes to iterate through the custom coordinates sequence ought to define the period.
#' It also makes sense in this case for \code{n.frames} to remain equal to \code{n.period} if the custom sequence is not meant to be cyclical but rather a single pass.
#' After this check the custom vectors are simply bound in a list.
#'
#' @param lon vector of arbitrary longitudes or starting longitude (starting point for constant globe rotation).
#' @param lat vector of arbitrary latitudes or fixed, repeating latitude.
#' @param n.period intended length of the period.
#' @param n.frames intended number of frames in animation.
#'
#' @return returns a list containing a longitude vector and a latitude vector.
#' @export
#'
#' @examples
#' # default 360 frames of 360-length period rotation,
#' get_lonlat_seq(0, 0) # beginning from lon 0, at lat 0
#'
#' get_lonlat_seq(0, 0, n.frames=40) # same but only first 40 frames
#' get_lonlat_seq(0, 0, n.frames=400) # same but looping for 40 additional frames
#' get_lonlat_seq(-20, 30, n.period=60) # quicker period, begin from lon -20, at lat 30
#'
#' get_lonlat_seq(1:60, 2:61, n.period=60) # custom sequence is simply put in list
get_lonlat_seq <- function(lon, lat, n.period=360, n.frames=n.period){
  if(length(lon) != 1 & length(lon) != n.period) stop("lon must be length one or length n.period")
  if(length(lat) != 1 & length(lat) != n.period) stop("lat must be length one or length n.period")
  if(any(lon < -180 || lon > 180)) stop("lon invalid")
  if(any(lat < -90 || lat > 90)) stop("lat invalid")
  if(length(lon)==1){
    lon <- rep(rev(seq(lon, lon+360, length.out=n.period + 1)[-1]), length=n.frames)
    lon[lon > 180] <- lon[lon > 180] - 360
  }
  if(length(lat)==1){
    lat <- rep(lat, n.frames)
  }
  list(lon=lon, lat=lat)
}

#' Project points onto globe
#'
#' Project points in \code{data} onto the globe and filter \code{data} to points within the current field of view.
#'
#' \code{do_projection} projects the coordinates in \code{data} onto the globe and filters \code{data} to the subset of rows
#' containing data which are visible given the current field of view.
#' The field of view is defined by the centroid focus latitude and longitude pair in the sequence of latitudes and longitudes whose index
#' corresponds to the frame ID in \code{data}. \code{data} may containing rows with multiple unique frame ID values,
#' which the function will group the data by.
#' These values are used to determine position in the user-defined lon/lat sequence and the corresponding in-view subset of data
#' for each subset of \code{data} grouped by the `id` variable.
#'
#' @param data a data frame.
#' @param id character, column name referring to column of \code{data} representing frame sequence integer IDs.
#' @param lon starting longitude for rotation sequence or vector of arbitrary longitude sequence.
#' @param lat fixed latitude or vector of arbitrary latitude sequence.
#' @param n.period intended length of the period.
#' @param n.frames intended number of frames in animation.
#' @param keep, if \code{TRUE}, return the entire input data drame (no subsetting) along with the boolean \code{inview} column.
#' Otherwise only return the row-filtered data frame with its original columns. Defaults to \code{FALSE}.
#'
#' @return returns a data frame containing visible points on the globe or all points along with a boolean \code{inview} column.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' data(annualtemps)
#' temps <- mutate(annualtemps, frameID = Year - min(Year) + 1)
#' do_projection(temps, id="frameID")
#' do_projection(temps, id="frameID", keep=TRUE)
do_projection <- function(data, id, lon=0, lat=0, n.period=360, n.frames=n.period, keep=FALSE){
  if(missing(id)) stop("'id' column is missing.")
  if(!id %in% names(data)) stop("'id' must refer to a column name.")
  lonlat <- get_lonlat_seq(lon, lat, n.period, n.frames)
  data <- dplyr::left_join(data,
    dplyr::group_by_(data, id) %>%
      dplyr::do(project_to_hemisphere(.$lon, .$lat, lonlat$lon[.[[id]][1]], lonlat$lat[.[[id]][1]])))
  if(keep) return(data)
  dplyr::filter(data, inview) %>% dplyr::select(-inview)
}

.theme_blank <- function(){
  eb <- ggplot2::element_blank()
  ggplot2::theme(axis.line=eb, axis.text.x=eb, axis.text.y=eb,
    axis.ticks=eb, axis.title.x=eb, axis.title.y=eb, legend.position="none",
    panel.background=eb, panel.border=eb, panel.grid.major=eb, panel.grid.minor=eb,
    plot.background=ggplot2::element_rect(colour="transparent", fill="transparent"))
}

#' Blank ggplot2 theme with optional axes
#'
#' A blank ggplot2 theme which will draw only data, but can include axes lines, ticks, and text if the color is not set to transparent.
#'
#' \code{.theme_blank_plus} is intended for plotting data, e.g., a line plot on a blank canvas (nothing drawn but the line itself)
#' while still retaining space for axes which may be added later so that everything will line up easily in overlaid plots.
#' Hence, the other time it is used is to plot visible axes, but perhaps no data.
#'
#' A common use case is as follows: use \code{.theme_blank_plus} repeatedly while saving high-resolution images to disk of a sequence of time series plots
#' where the time series line grows from left to right through the still image sequence. The axes are fixed across all plots so there is no need to draw them every time, but space is left for them.
#' Subsequently, only a simgle plot is saved to disk of the axes (with no data drawn) and this image in layered with the sequence of data images when an animation is made.
#'
#' @param col axis line, tick and text color. Defaults to \code{"transparent"}.
#' @param size integer, annotation size for axes titles and axes label text.
#' @param legend.position legend position passed to \code{ggplot2::theme()}.
#'
#' @examples
#' # not run
.theme_blank_plus <- function(col="transparent", size=16, legend.position="none"){
  eb <- ggplot2::element_blank()
  el <- ggplot2::element_line(colour=col)
  ggplot2::theme(axis.line=el, axis.line.x=el, axis.line.y=el, axis.ticks=el,
        axis.text=ggplot2::element_text(colour=col, size=size),
        axis.title=ggplot2::element_text(colour=col, size=ggplot2::rel(1.5)), #currently non-functioning
        legend.position=legend.position,
        panel.background=eb, panel.border=eb, panel.grid.major=eb, panel.grid.minor=eb,
        plot.background=ggplot2::element_rect(colour="transparent", fill="transparent"))
}

#' Save a sequence of still images to disk
#'
#' Save a sequence of still images to disk with a single function call and data frame.
#'
#' \code{save_seq} is a convenient wrapper function for \code{save_map} and \code{save_ts}. It provides some moderate generality and abstraction
#' by moving the most proximal aspects of data preparation inside the function, i.e., breaking a data frame into a list of data frame subsets by plot ID
#' and passing each explicitly to iterative calls to either \code{save_map} or \code{save_ts}.
#' The option for parallel processing on Linux systems (by forking with \code{parallel::mclapply}) is also part of \code{save_seq}.
#' Using \code{mclapply} was chosen for convenience and may be changed in a future package version.
#'
#' It does not save much in the way of gross typing, but calling a single wrapper function, passing mostly the same arguments,
#' and not having to explicitly call \code{save_map} or \code{save_ts} withing the context of \code{map} or \code{walk} calls is arguably
#' cleaner, simpler, and less complex for some use cases.
#'
#' The additional arguments \code{...} passed to \code{save_map} or \code{save_ts} are required, not optional.
#' Any call to \code{save_seq} will consist mostly of these arguments.
#' It is best to first make sure you can successfully call \code{save_map} and \code{save_ts} directly. Then try this wrapper function.
#' See the intoductory vignette for details: \code{browseVignettes(package="mapmate")}.
#'
#' @param data a data frame containing networks, tiles, lines or polygons information.
#' @param style character, must be \code{style="map"} for maps (uses \code{save_map}) or \code{style="tsline"} for time series line graphs (uses \code{save_ts}).
#' @param use_mclapply \code{TRUE} for parallel processing. Must be \code{FALSE} (default) for non-Unix-alikes (e.g., Windows systems).
#' @param mc.cores integer, the number of CPU cores requested for parallel processing, passed to \code{mclapply}.
#' @param ... additional arguments passed to \code{save_map} or \code{save_ts}.
#'
#' @return usually returns NULL after writing files to disk. May optionally return a list of ggplot objects with or without the file writing side effect.
#' @export
#'
#' @examples
#' # not run
#' \dontrun{
#' library(dplyr)
#' library(purrr)
#' data(annualtemps)
#' temps <- mutate(annualtemps, frameID = Year - min(Year) + 1) %>%
#'   group_by(Year, frameID) %>% summarise(z=mean(z))
#' xlm <- range(temps$Year)
#' ylm <- range(temps$z)
#'
#' # should specify a dir or set working dir for file output
#' # consider running over a smaller subset of frame IDs
#' save_seq(temps, style="tsline", x="Year", y="z", id="frameID",
#'   col="blue", xlm=xlm, ylm=ylm)
#' }
save_seq <- function(data, style="map", use_mclapply=FALSE, mc.cores=1L, ...){
  if(!style %in% c("map", "tsline")) stop("'style' must be 'map' or 'tsline'.")
  if(Sys.info()["sysname"]!="Linux" & use_mclapply) stop("parallel::mclapply only available on Unix-alike systems.")
  dots <- list(...)
  id <- dots$id
  if(is.null(id)) stop("'id' column is missing.")
  return.plot <- dots$return.plot
  if(is.null(return.plot)) return.plot <- FALSE
  if(style=="map"){
    data <- split(data, data[[id]])
    if(use_mclapply){
      return(parallel::mclapply(data, save_map, ..., mc.cores=mc.cores))
    } else {
      if(return.plot) return(purrr::map(data, ~save_map(.x, ...))) else return(purrr::walk(data, ~save_map(.x, ...)))
    }
  } else if(style=="tsline"){
    if(!is.null(dots$cap)) stop("When calling 'save_seq' with style='tsline', do not pass argument 'cap' on to 'save_ts'.")
    iters <- sort(unique(data[[id]]))
    data <- purrr::map(iters, ~dplyr::filter_(data,
      .dots=list(lazyeval::interp(~y <= x, .values=list(y=as.name(id), x=as.name(".x"))))))
    if(use_mclapply){
      return(parallel::mclapply(data, save_ts, ..., mc.cores=mc.cores))
    } else {
      if(return.plot) return(purrr::map(data, ~save_ts(.x, ...))) else return(purrr::walk(data, ~save_ts(.x, ...)))
    }
  }
}

#' Save time series plots
#'
#' Save a time series plot to disk intended to be part of a as a still image sequence of a growing time series.
#'
#' For  \code{id} column frame ID values \code{i}, \code{cap} subsets \code{data} to rows where \code{i <= cap}.
#' Sequential application of \code{save_ts} should involve iterating \code{cap} over the values \code{i}.
#' A data frame passed to \code{save_map} need not be subset based on the current frame ID in advance so providing \code{cap} values is important. See example.
#'
#' When calling \code{save_ts} iteratively from the \code{save_seq} wrapper function, \code{save_ts} is applied over a list of sequentially subsetted data frames based on the frame IDs.
#' In this case, specifying \code{cap} is not needed and an error will be thrown if provided.
#'
#' Fixed axis limits must be established in advance by computing the max range or other desired range for the x and y variables that are to be plotted.
#'
#' @param data data frame containing the \code{x} and \code{y} plotting variables.
#' @param x character, the column name in \code{data} for the variable plotted along the x axis.
#' @param y character, the column name in \code{data} for the variable plotted along the y axis.
#' @param id character, column name referring to column of \code{data} representing frame sequence integer IDs.
#' @param cap time index/frame ID used to subset \code{data}.
#' The rows of data retained are all those where \code{p <= cap}, where \code{p} represents the frame ID values in column \code{id}. Defaults to all data if missing.
#' @param dir png output directory. Defaults to working directory.
#' @param col color of the time series line or the axes lines, ticks, and text. Defaults to black.
#' @param xlm x axis limits.
#' @param ylm y axis limits.
#' @param axes.only only plot axis information, no data. Defaults to \code{FALSE}.
#' @param axes.space if \code{axes.only=TRUE}, leave room for x and y axes in plot window when \code{axes.space=TRUE}.
#' Remove this marginal area so that data are plotted over the full canvas when \code{axes.space=FALSE}.
#' Defaults to \code{TRUE}. Ignored when \code{axes.only=TRUE} because of the explicit intent to draw axes.
#' @param suffix character, optional suffix to be pasted onto output filename.
#' @param png.args a list of arguments passed to \code{png}.
#' @param save.plot save the plot to disk. Defaults to \code{TRUE}. Typically only set to \code{FALSE} for demonstrations and testing.
#' @param return.plot return the ggplot object. Defaults to \code{FALSE}. Only intended for single-plot demonstrations and testing, not for still image sequence automation.
#' @param num.format number of digits including any leading zeros for image sequence frame numbering. Defaults to 4, i.e. \code{0001, 0002, ...}.
#'
#' @return usually returns NULL after writing file to disk. May return a ggplot object with or without the file writing side effect.
#' @export
#'
#' @examples
#' # not run
#' \dontrun{
#' library(dplyr)
#' library(purrr)
#' data(annualtemps)
#' temps <- mutate(annualtemps, frameID = Year - min(Year) + 1) %>%
#'   group_by(Year, frameID) %>% summarise(z=mean(z))
#' xlm <- range(temps$Year)
#' ylm <- range(temps$z)
#'
#' # should specify a dir or set working dir for file output
#' # consider running over a smaller subset of frame IDs
#' walk(temps$frameID, ~save_ts(temps, x="Year", y="z", id="frameID",
#'   cap=.x, col="blue", xlm=xlm, ylm=ylm))
#' }
save_ts <- function(data, x, y, id, cap, dir=getwd(), col="black", xlm, ylm, axes.only=FALSE, axes.space=TRUE, suffix=NULL,
                    png.args=list(width=1920, height=1080, res=300, bg="transparent"), save.plot=TRUE, return.plot=FALSE, num.format=4){
  type <- "tsline"
  if(missing(id)) stop("'id' column is missing.")
  if(!id %in% names(data)) stop("'id' must refer to a column name.")
  mx <- max(data[[id]])
  if(missing(cap)) cap <- mx
  if(cap <1) stop("'cap' must be >= 1.")

  if(!axes.only & axes.space) .theme <- .theme_blank_plus()
  if(!axes.only & !axes.space) .theme <- .theme_blank()
  if(mx >= eval(parse(text=paste0("1e", num.format))))
    warning("'num.format' may be too small for sequential file numbering given the max frameID value.")
  .dots <- list(lazyeval::interp(~y <= x, .values=list(y=as.name(id), x=cap)))
  data <- dplyr::filter_(data, .dots=.dots)
  g <- ggplot2::ggplot(data, ggplot2::aes_string(x, y))
  if(length(col) > 1){
    warning("'col' has length > 1. Only first element will be used.")
    col <- col[1]
  }
  if(axes.only){
    g <- g + ggplot2::scale_x_continuous(name="", breaks=seq(xlm[1], xlm[2], by=10), limits=xlm) +
      ggplot2::scale_y_continuous(name="", limits=ylm) + .theme_blank_plus(col)
  } else {
    g <- g + ggplot2::xlim(xlm) + ggplot2::ylim(ylm) + .theme
    if(cap != 1) g <- g + ggplot2::geom_line(colour=col, size=1)
  }
  if(save.plot){
    ext <- if(axes.only) "_axesOnly.png" else paste0("_%0", num.format, "d.png")
    if(is.character(suffix)) type <- paste(type, suffix, sep="_")
    dir.create(dir, recursive=TRUE, showWarnings=FALSE)
    filename <- sprintf(paste0(dir, "/", type, ext), cap)
    do.call(png, c(filename=filename, png.args))
    print(g)
    dev.off()
  }
  if(return.plot) return(g)
  NULL
}
