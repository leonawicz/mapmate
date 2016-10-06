#' Obtain a list of monthly climatologies
#'
#' \code{get_clim} subsets and summarizes a data frame of monthly map data over a specified time period.
#'
#' \code{get_clim} takes a five-column data frames containing map data (long, lat, z, Year, Month) describing a temporal sequence of map data.
#' The data frame is subset to a specified range of years. The data (z) is summarized over time for each month and a length-12 list of data frames containing period average map data is returned.
#'
#' @param x a data frame.
#' @param limits inclusive lower and upper bound limits for subsetting the years (climatology period).
#'
#' @return returns a list of data frames.
#' @export
#'
#' @examples
#' #not run
get_clim <- function(x, limits=c(1961, 1990)){
  x <- purrr::map(x, ~as.matrix(.x))
  x <- purrr::map(unique(mo), ~x[yr >= limits[1] & yr <= limits[2] & mo==.x])
  purrr::map(x, ~Reduce("+", .x)[,3]/(diff(limits) + 1))
}

#' Obtain moving average map series
#'
#' Obtain a moving average for monthly, annual or seasonal resolution data from monthly map data.
#'
#' \code{get_ma} takes a list of data frames and for each data frame computes and returns the moving or rolling average
#' after first summarizing monthly data to seasonal or annual averages if applicable.
#' Winter begins in December and each season is three consecutive months (DJF, MAM, JJA, SON).
#' Parallel processing is Linux-only (uses \code{mclapply}) and the default of 32 CPU cores will have to be changed if you don't have that many.
#' It is convenient for my usage and this package version is not intended for general use.
#' Most other users will probably not be using parallel processing at all, in which case \code{n.cores} is ignored.
#'
#' @param x list of data frames.
#' @param type character, one of \code{"monthly"}, \code{"annual"}, or \code{"seasonal"}.
#' @param season \code{NULL} or character, one of \code{"winter"}, \code{"spring"}, \code{"summer"}, or \code{"autumn"}. Default is \code{NULL}.
#' @param size number of years for the moving average window. Default \code{10}.
#' @param n.cores number of CPUs for parallel processing.
#'
#' @return returns a list of data frames.
#' @export
#'
#' @examples
#' # not run
get_ma <- function(x, type, season=NULL, size=10, n.cores=32){
  if(!(type %in% c("monthly", "annual", "seasonal"))) stop("invalid type.")
  if(type=="monthly"){
    x <- mclapply(x,
                  function(x, size) group_by(x, Month, long, lat) %>%
                    mutate(z=roll_mean(z, size, fill=NA), idx=NULL) %>% filter(!is.na(z)),
                  size=size, mc.cores=n.cores)
  }
  if(type=="annual"){
    x <- mclapply(x,
                  function(x, size) group_by(x, long, lat, Year) %>%
                    summarise(z=mean(z)) %>% mutate(z=roll_mean(z, size, fill=NA), idx=NULL) %>% filter(!is.na(z)),
                  size=size, mc.cores=n.cores)
  }
  if(type=="seasonal"){
    if(is.null(season) || !(season %in% c("winter", "spring", "summer", "autumn")))
      stop("If res='seasonal', season must be 'winter', 'spring', 'summer' or 'autumn'.")
    idx <- switch(season, winter=c(12,1,2), spring=3:5, summer=6:8, autumn=9:11)
    yr.lim <- range(x[[1]]$Year)
    x <- mclapply(x,
                  function(x, size){
                    mutate(x, Year=ifelse(Month==12, Year+1, Year), Month=ifelse(Month %in% idx, 1, 0)) %>%
                      filter(Year > yr.lim[1] & Year <= yr.lim[2] & Month==1) %>%
                      group_by(long, lat, Month, Year) %>% summarise(z=mean(z)) %>%
                      mutate(z=roll_mean(z, size, fill=NA), Month=NULL, idx=NULL) %>% filter(!is.na(z))
                  }, size=size, mc.cores=n.cores)
  }
  x <- bind_rows(x) %>% group_by # %>% arrange_(.dots=arr)
  x <- if(type %in% c("seasonal", "annual")) x %>% split(.$Year) else x %>% split(paste(.$Year, .$Month+9))
  x
}

#' Identidy visible points on an arbitrary global hemishpere view.
#'
#' Given a global hemispheric field of view defined by a single latitudinal and longitudinal centroid focal point, project geographic points onto the hemishpere.
#'
#' \code{project_to_hemisphere} identifies whether each pair of coordinates in the \code{lat} and \code{long} vectors is in a field of view defined by a centroid focal point \code{(lat0, long0)}
#' and returns a data table containing the original coordinates and a column indicating if the coordinates are in the field of view (\code{TRUE} or \code{FALSE}).
#'
#' @param lat vector of latitudes.
#' @param long vector of longitudes.
#' @param lat0 latitude of focus coordinates.
#' @param long0 longitude of focus coordinates.
#'
#' @return returns a data table.
#' @export
#'
#' @examples
#' #not run
project_to_hemisphere <- function(lat, long, lat0, long0){
  hold <- cbind(lat, long)
  x <- purrr::map(list(lat, lat0, long-long0), ~.x*pi/180)
  inview <- sin(x[[1]])*sin(x[[2]]) + cos(x[[1]])*cos(x[[2]])*cos(x[[3]]) > 0
  data.table(long=hold[,2], lat=hold[,1], inview=inview)
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
#' @param n.period An integer, the known period of rotation that will be part of an animation in which the map data frames in \code{x} will be sequentially plotted over. Default is 360 (1-degree increment rotations).
#' @param rotation character, one of \code{"add"} or \code{"pad"}.
#' @param force
#'
#' @return returns \code{x} but padded with it's final element appended repeatedly based on a specified period and type of padding method.
#' @export
#'
#' @examples
#' # not run
pad_frames <- function(x, n.period=360, rotation="add", force=TRUE){
  n <- length(x)
  if(n >= n.period & !force) return(x)
  if(rotation=="add") x2 <- purrr::map(1:(n.period-1), ~x[[n]] %>% mutate(frameID=.x + n))
  if(rotation=="pad") x2 <- purrr::map(1:(n.period-n), ~x[[n]] %>% mutate(frameID=.x + n))
  c(x, x2)
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
  if(length(lon)==1){
    lon <- rep(rev(seq(lon, lon+360, length.out=n.period + 1)[-(n.period + 1)]), length=n.frames)
    lon[lon >= 360] <- lon[lon >= 360] - 360
  }
  if(length(lat)==1){
    if(lat < -90 || lat > 90) stop("lat invalid")
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
  left_join(x, project_to_hemisphere(x$lat, x$long, lonlat$lat[i], lonlat$lon[i])) %>%
    filter(inview) %>% dplyr::select(-inview)
}

theme_blank <- function(){
  eb <- element_blank()
  theme(axis.line=eb, axis.text.x=eb, axis.text.y=eb,
    axis.ticks=eb, axis.title.x=eb, axis.title.y=eb, legend.position="none",
    panel.background=eb, panel.border=eb, panel.grid.major=eb, panel.grid.minor=eb,
    plot.background=element_rect(colour="transparent", fill="transparent"))
}

#' Blank ggplot2 theme with optional axes
#'
#' A blank ggplot2 theme which will draw only data, but can include axes lines, ticks, and text is the color is not set to transparent.
#'
#' \code{theme_blank_plus} is intended for plotting data, e.g., a line plot on a blank canvas (nothing drawn but the line itself)
#' while still retaining space for axes which may be added later so that everything will line up easily in overlaid plots.
#' Hence, the other time it is used is to plot visible axes, but perhaps no data.
#'
#' A common use case is as follows: use \code{theme_blank_plus} repeatedly while saving high-resolution images to disk of a sequence of time series plots
#' where the time series line grows from left to right through the still image sequence. The axes are fixed across all plots so there is no need to draw them every time, but space is left for them.
#' Subsequently, only a simgle plot is saved to disk of the axes (with no data drawn) and this image in layered with the sequence of data images when an animation is made.
#'
#' @param col axis line, tick and text color. Defaults to \code{"transparent"}.
#'
#' @export
#'
#' @examples
#' # not run
theme_blank_plus <- function(col="transparent"){
  eb <- element_blank()
  el <- element_line(colour=col)
  theme(axis.line=el, axis.line.x=el, axis.line.y=el, axis.ticks=el,
        axis.text=element_text(colour=col, size=18), legend.position="none",
        panel.background=eb, panel.border=eb, panel.grid.major=eb, panel.grid.minor=eb,
        plot.background=element_rect(colour="transparent", fill="transparent"))
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
#' \code{z.range} is important for \code{type="maptiles"} because it is used to ensure colors are mapped to values consistently across all plots.
#' This is not only for the case of changing data values across a series of plots of different data frames \code{x}.
#' There are also changes in the range of values for a fixed data frame when it is plotted repeatedly as the globe is rotated and different hemispheres of the map
#' (different data subsets) are in view across the image sequence.
#'
#' @param x a data frame containing networks, tiles, or lines information.
#' @param lon starting longitude for rotation sequence or vector of arbitrary longitude sequence.
#' @param lat fixed latitude or vector of arbitrary latitude sequence.
#' @param n.period intended length of the period.
#' @param n.frames intended number of frames in animation.
#' @param ortho use an orthographic projection for globe plots. Defaults to \code{TRUE}.
#' @param col sensible default colors provided for each \code{type}
#' @param type the type of plot, one of \code{"network"}, \code{maptiles}, or \code{maplines}.
#' @param suffix character, optional suffix to be pasted onto output filename.
#' @param z.range the full known range for the data values across all \code{x} objects, not just the current one.
#' @param rotation.axis the rotation axis used when \code{ortho=TRUE} for globe plots. Defaults to 23.4 degrees.
#'
#' @return NULL
#' @export
#'
#' @examples
#' # not run
save_map <- function(x, lon=0, lat=0, n.period=360, n.frames=n.period, ortho=TRUE, col=NULL, type="network", suffix=NULL, z.range=NULL, rotation.axis=23.4){
  if(is.null(col)) col <- switch(type,
    network=c("#FFFFFF25", "#1E90FF25", "#FFFFFF", "#1E90FF50"),
    maptiles=c("black", "steelblue4"),
    maplines="white")
  i <- x$frameID[1]
  lonlat <- get_lonlat_seq(lon, lat, n.period, n.frames)
  if(type=="network") x.lead <- group_by(x, group) %>% slice(n())
  g <- ggplot(x, aes(long, lat))
  if(type=="maptiles"){
    if(is.null(z.range)) z.range <- range(x$z, na.rm=TRUE)
    g <- ggplot(x, aes(long, lat, fill=z)) + geom_tile() +
      scale_fill_gradientn(colors=col, limits=z.range)
  } else {
    g <- ggplot(x, aes(long, lat, group=group))
    if(type=="maplines") g <- g + geom_path(colour=col[1])
    if(type=="network") g <- g + geom_path(colour=col[2]) + geom_path(colour=col[1]) +
        geom_point(data=x.lead, colour=col[3], size=0.6) +
        geom_point(data=x.lead, colour=col[4], size=0.3)
  }

  g <- g + theme_blank()
  if(ortho) g <- g + coord_map("ortho", orientation=c(lonlat$lat[i], lonlat$lon[i], rotation.axis))
  if(is.character(suffix)) type <- paste(type, suffix, sep="_")
  dir.create(outDir <- file.path("frames", type), recursive=TRUE, showWarnings=FALSE)
  png(sprintf(paste0(outDir, "/", type, "_%04d.png"), i),
      width=4*1920, height=4*1080, res=300, bg="transparent")
  print(g)
  dev.off()
  NULL
}

#' Save time series plots
#'
#'Save a time series plot to disk intended to be part of a as a still image sequence of a growing time series.
#'
#' @param i current time index used to subset \code{x}.
#' @param x data frame for plotting, currently assuming columns of \code{Year} and \code{Mean} as the x and y plotting variables.
#' @param label a subdirectory label. Plots are saved in the working directory under \code{frames/{label}}.
#' @param col color of the time series line or the axes lines, ticks, and text.
#' @param xlm x axis limits.
#' @param ylm y axis limits.
#' @param axes_only only plot axis information, no data. Defaults to \code{FALSE}.
#'
#' @return NULL
#' @export
#'
#' @examples
#' # not run
save_ts <- function(i, x, label, col, xlm, ylm, axes_only=FALSE){
  x <- filter(x, frameID <= i)
  g <- ggplot(x, aes(Year, Mean))
  if(axes_only){
    if(i!=1) return()
    g <- g + scale_x_continuous(name="", breaks=seq(xlm[1], xlm[2], by=10), limits=xlm) +
      scale_y_continuous(name="", limits=ylm) + theme_blank_plus(col)
    dir.create(outDir <- "frames", showWarnings=FALSE)
    png(paste0(outDir, "/ts_axes_fixed_bkgd.png"), width=4*1920, height=4*1080, res=300, bg="transparent")
    print(g)
    dev.off()
    return()
  }
  g <- g + geom_line(colour=col, size=1) + xlim(xlm) + ylim(ylm) + theme_blank()
  dir.create(outDir <- file.path("frames", label), recursive=TRUE, showWarnings=FALSE)
  png(sprintf(paste0(outDir, "/", label, "_%04d.png"), i),
      width=4*1920, height=4*1080, res=300, bg="transparent")
  print(g)
  dev.off()
  NULL
}
