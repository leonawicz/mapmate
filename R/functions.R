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
#' #not run
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
#' # not run
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
  #if(id != "frameID"){
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
#' # not run
get_lonlat_seq <- function(lon, lat, n.period=360, n.frames=n.period){
  if(length(lon) != 1 & length(lon) != n.period) stop("lon must be length one or length n.period")
  if(length(lat) != 1 & length(lat) != n.period) stop("lat must be length one or length n.period")
  if(any(lon < -180 || lon > 180)) stop("lon invalid")
  if(any(lat < -90 || lat > 90)) stop("lat invalid")
  if(length(lon)==1){
    lon <- rep(rev(seq(lon, lon+360, length.out=n.period + 1)[-(n.period + 1)]), length=n.frames)
    lon[lon >= 360] <- lon[lon >= 360] - 360
  }
  if(length(lat)==1){
    lat <- rep(lat, n.frames)
  }
  list(lon=lon, lat=lat)
}

#' Project points onto globe
#'
#' Project points in \code{x} onto the globe and filter \code{x} to points within the current field of view.
#'
#' \code{do_projection} projects the coordinates in \code{x} onto the globe and filters \code{x} to the subset of rows
#' containing data which are visible given the current field of view.
#' The field of view is defined by the centroid focus latitude and longitude pair in the sequence of latitudes and longitudes whose index
#' corresponds to the frame ID in \code{x}.
#'
#' @param x a data frame.
#' @param lon starting longitude for rotation sequence or vector of arbitrary longitude sequence.
#' @param lat fixed latitude or vector of arbitrary latitude sequence.
#' @param n.period intended length of the period.
#' @param n.frames intended number of frames in animation.
#'
#' @return returns a data frame containing visible points on the globe.
#' @export
#'
#' @examples
#' # not run
do_projection <- function(x, lon=0, lat=0, n.period=360, n.frames=n.period){
  i <- x$frameID[1]
  lonlat <- get_lonlat_seq(lon, lat, n.period, n.frames)
  dplyr::left_join(x, project_to_hemisphere(x$lat, x$long, lonlat$lat[i], lonlat$lon[i])) %>%
    dplyr::filter(inview) %>% dplyr::select(-inview)
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


#' Save maps to disk
#'
#' Save a map to disk intended to be part of a as a still image sequence of one of three types: networks, tiles, or lines.
#'
#' \code{save_map} takes a specific type of data frame catering to networks, tiles, or lines.
#' It plots a 3D globe map with \code{ortho=TRUE} (default) or a flat map (\code{ortho=FALSE}).
#' For flat maps, \code{lon}, \code{lat}, \code{n.period}, \code{n.frames}, and \code{rotation.axis} are ignored.
#' For plotting on a globe, \code{lon} and \code{lat} are used to describe the field of view or the visible hemisphere.
#' \code{n.period} relates is eithe the period of rotation of the globe or the length of the non-repeating, arbitrary coordinates sequence.
#' \code{n.frames} is always the explicit number of frames that will make up an animation
#' regardless of the length of the series of data frames \code{x} to be plotted or the length of the rotational period or coordinates sequence.
#'
#' \code{z.name} is relevant only for fill color when drawing tiles or polygons.
#' \code{z.range} is important for \code{type="maptiles"} because it is used to ensure colors are mapped to values consistently across all plots.
#' This is not only for the case of changing data values across a series of plots of different data frames \code{x}.
#' There are also changes in the range of values for a fixed data frame when it is plotted repeatedly as the globe is rotated and different hemispheres of the map
#' (different data subsets) are in view across the image sequence. \code{z.range} will default to the range of the given \code{x} if not provided.
#'
#' The \code{color} argument is used differently depending on \code{type}.
#' For \code{maplines} it is a single color. Additional colors in a vector are ignored. For other plot types it must be a vector.
#' \code{maptiles} and \code{polygons} require a vector of at least two colors to produce a palette for their color gradient.
#' \code{network} is a special case which assumes four colors in the following order: background line, foreground line, background point, foreground point.
#' The four colors are layered in the plot in this order. Semi-transparent colors can work well in this context. Additional colors are ignored.
#' If \code{col=NULL} (default) sensible default colors are provided for each plot type.
#'
#' The png output directory will be created if it does not exist, recursively if necessary. The default is the working directory.
#' This is ignored if \code{save.plot=FALSE}.
#'
#' \code{type="polygons"} is only recommended for use with flat maps, not the orthographic projection. See the vignette for an example and description of the issue.
#' For globe plots it is best to rasterize polygons and use \code{type="maptiles"} for better results in exchange for increased processing time.
#'
#' @param x a data frame containing networks, tiles, or lines information.
#' @param z.name character, the column name of the data (\code{z}) variable in \code{x}. Only needed for \code{type="maptiles"} and \code{type="polygons"}
#' @param z.range numeric vector, the full known range for the data values across all \code{x} objects, not just the current one, e.g. \code{c(0, 5)}.
#' @param dir png output directory. Defaults to working directory.
#' @param lon starting longitude for rotation sequence or vector of arbitrary longitude sequence.
#' @param lat fixed latitude or vector of arbitrary latitude sequence.
#' @param n.period intended length of the period.
#' @param n.frames intended number of frames in animation.
#' @param ortho use an orthographic projection for globe plots. Defaults to \code{TRUE}.
#' @param col sensible default colors provided for each \code{type}
#' @param type the type of plot, one of \code{"network"}, \code{maptiles}, \code{maplines}, or \code{polygons}.
#' @param suffix character, optional suffix to be pasted onto output filename.
#' @param rotation.axis the rotation axis used when \code{ortho=TRUE} for globe plots. Defaults to 23.4 degrees.
#' @param png.args a list of arguments passed to \code{png}.
#' @param save.plot save the plot to disk. Defaults to \code{TRUE}. Typically only set to \code{FALSE} for demonstrations and testing.
#' @param return.plot return the ggplot object. Defaults to \code{FALSE}. Only intended for single-plot demonstrations and testing, not for still image sequence automation.
#' @param num.format number of digits including any leading zeros for image sequence frame numbering. Defaults to 4, i.e. \code{0001, 0002, ...}.
#'
#' @return usually returns NULL after writing file to disk as a side effect. May return a ggplot object but be careful not to use this option if looping over many plots.
#' @export
#'
#' @examples
#' # not run
save_map <- function(x, z.name=NULL, z.range=NULL, dir=getwd(), lon=0, lat=0, n.period=360, n.frames=n.period, ortho=TRUE, col=NULL, type, suffix=NULL, rotation.axis=23.4,
                     png.args=list(width=1920, height=1080, res=300, bg="transparent"), save.plot=TRUE, return.plot=FALSE, num.format=4){
  if(n.frames >= eval(parse(text=paste0("1e", num.format))))
    warning("'num.format' may be too small for sequential file numbering given the total number of files suggested by 'n.frames'.")
  if(is.null(col)) col <- switch(type,
    network=c("#1E90FF25", "#FFFFFF25", "#FFFFFF", "#1E90FF50"),
    maptiles=c("black", "white"),
    maplines="white",
    polygons=c("royalblue", "purple", "orange", "yellow"))
  i <- x$frameID[1]
  lonlat <- get_lonlat_seq(lon, lat, n.period, n.frames)
  if(type=="maptiles"){
    if(is.null(z.name)) stop("Must provide 'z.name'.")
    if(length(col) < 2) stop("'col' must be a vector of at least two colors for map tiles color palette gradient.")
    if(is.null(z.range)) z.range <- range(x[[z.name]], na.rm=TRUE)
    g <- ggplot2::ggplot(x, ggplot2::aes_string("lon", "lat", fill=z.name)) + ggplot2::geom_tile() +
      ggplot2::scale_fill_gradientn(colors=col, limits=z.range)
  } else if(type=="polygons"){
    if(is.null(z.name)) stop("Must provide 'z.name'.")
    if(length(col) < 2) stop("'col' must be a vector of at least two colors for polygon fill color palette gradient.")
    if(is.null(z.range)) z.range <- range(x[[z.name]], na.rm=TRUE)
    g <- ggplot2::ggplot(x, ggplot2::aes_string("lon", "lat", group="group", fill=z.name)) + ggplot2::geom_polygon() +
      ggplot2::geom_path(color="white") + ggplot2::scale_fill_gradientn(colours=col, limits=z.range)
  } else {
    g <- ggplot2::ggplot(x, ggplot2::aes_string("lon", "lat", group="group"))
    if(type=="maplines") g <- g + ggplot2::geom_path(colour=col[1])
    if(type=="network"){
      x.lead <- dplyr::group_by(x, group) %>% dplyr::slice(dplyr::n())
      g <- g + ggplot2::geom_path(colour=col[1]) + ggplot2::geom_path(colour=col[2]) +
        ggplot2::geom_point(data=x.lead, colour=col[3], size=0.6) +
        ggplot2::geom_point(data=x.lead, colour=col[4], size=0.3)
    }
  }

  g <- g + .theme_blank()
  if(ortho) g <- g + ggplot2::coord_map("ortho", orientation=c(lonlat$lat[i], lonlat$lon[i], rotation.axis))
  if(save.plot){
    if(is.character(suffix)) type <- paste(type, suffix, sep="_")
    dir.create(dir, recursive=TRUE, showWarnings=FALSE)
    filename <- sprintf(paste0(dir, "/", type, "_%0", num.format, "d.png"), i)
    do.call(png, c(filename=filename, png.args))
    print(g)
    dev.off()
  }
  if(return.plot) return(g)
  NULL
}

#' Save time series plots
#'
#' Save a time series plot to disk intended to be part of a as a still image sequence of a growing time series.
#'
#' \code{i} subsets \code{x} to rows with \code{frameID <= i}. \code{i} defaults to 1, which is sufficient and convenient for the \code{axes_only=TRUE} case but not required.
#' Fixed axis limits must be established in advance by computing the max range or other desired range for the x and y variables that are to be plotted.
#'
#' @param data data frame containing the \code{x} and \code{y} plotting variables.
#' @param x character, the column name in \code{data} for the variable plotted along the x axis.
#' @param y character, the column name in \code{data} for the variable plotted along the y axis.
#' @param i current time index used to subset \code{x}.
#' @param dir png output directory. Defaults to working directory.
#' @param col color of the time series line or the axes lines, ticks, and text.
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
#' @return usually returns NULL after writing file to disk as a side effect. May return a ggplot object but be careful not to use this option if looping over many plots.
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
#' # should specify a dir or set working dir for file output
#' # consider running over a smaller subset of frame IDs
#' walk(temps$frameID, ~save_ts(temps, "Year", "z", i=.x, col="blue", xlm, ylm))
#' }
save_ts <- function(data, x, y, i=1, dir=getwd(), col, xlm, ylm, axes.only=FALSE, axes.space=TRUE, suffix=NULL,
                    png.args=list(width=1920, height=1080, res=300, bg="transparent"), save.plot=TRUE, return.plot=FALSE, num.format=4){
  type <- "tsline"
  if(!axes.only & axes.space) .theme <- .theme_blank_plus()
  if(!axes.only & !axes.space) .theme <- .theme_blank()
  if(max(data$frameID) >= eval(parse(text=paste0("1e", num.format))))
    warning("'num.format' may be too small for sequential file numbering given the max frameID value.")
  data <- dplyr::filter(data, frameID <= i)
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
    if(i != 1) g <- g + ggplot2::geom_line(colour=col, size=1)
  }
  if(save.plot){
    ext <- if(axes.only) "_axesOnly.png" else paste0("_%0", num.format, "d.png")
    if(is.character(suffix)) type <- paste(type, suffix, sep="_")
    dir.create(dir, recursive=TRUE, showWarnings=FALSE)
    filename <- sprintf(paste0(dir, "/", type, ext), i)
    do.call(png, c(filename=filename, png.args))
    print(g)
    dev.off()
  }
  if(return.plot) return(g)
  NULL
}
