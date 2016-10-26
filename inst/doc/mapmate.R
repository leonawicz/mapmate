## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="#>", message=F, warning=F, error=F, eval=F, tidy=T) # for html_vignette only
#knitr::opts_chunk$set(collapse=TRUE, comment="#>", message=F, warning=F, error=F, eval=F, tidy=T, fig.width=0.8*5.33, fig.height=0.8*3) # for html_document only

## ---- eval=TRUE----------------------------------------------------------
library(mapmate)
library(dplyr)
library(purrr)
data(monthlytemps)
monthlytemps

get_ma(monthlytemps, type="seasonal", season="winter")

get_ma(monthlytemps, type="annual", size=20)

## ---- eval=TRUE----------------------------------------------------------
data(annualtemps)
annualtemps

## ---- eval=TRUE, fig.show='hold', fig.cap="2D flat map and 3D globe"-----
library(RColorBrewer)
pal <- rev(brewer.pal(11,"RdYlBu"))
temps <- mutate(annualtemps, frameID = Year - min(Year) + 1)
frame1 <- filter(temps, frameID==1) # subset to first frame
id <- "frameID"

save_map(frame1, z.name="z", id=id, ortho=FALSE, col=pal, type="maptiles", save.plot=FALSE, return.plot=TRUE)
save_map(frame1, z.name="z", id=id, col=pal, type="maptiles", save.plot=FALSE, return.plot=TRUE)


## ------------------------------------------------------------------------
#  rng <- range(annualtemps$z, na.rm=TRUE)
#  n <- length(unique(annualtemps$Year))
#  suffix <- "annual_2D"
#  temps <- split(temps, temps$frameID)
#  walk(temps, ~save_map(.x, z.name="z", id=id, ortho=FALSE, col=pal, type="maptiles", suffix=suffix, z.range=rng))
#  

## ------------------------------------------------------------------------
#  suffix <- "annual_3D_fixed"
#  walk(temps, ~save_map(.x, z.name="z", id=id, lon=rep(-70, n), lat=50, n.period=n, n.frames=n, col=pal, type="maptiles", suffix=suffix, z.range=rng))
#  

## ------------------------------------------------------------------------
#  data(borders)
#  borders <- map(1:n, ~mutate(borders, frameID = .x))
#  suffix <- "borders_3D_rotating"
#  walk(borders, ~save_map(.x, id=id, lon=-70, lat=50, n.period=30, n.frames=n, col="orange", type="maplines", suffix=suffix))
#  

## ------------------------------------------------------------------------
#  temps1 <- map(1:n, ~mutate(temps[[1]], frameID = .x))
#  rng1 <- range(temps1[[1]]$z, na.rm=TRUE)
#  suffix <- "year1_3D_rotating"
#  walk(temps1, ~save_map(.x, z.name="z", id=id, lon=-70, lat=50, n.period=30, n.frames=n, col=pal, type="maptiles", suffix=suffix, z.range=rng1))
#  

## ------------------------------------------------------------------------
#  suffix <- "annual_3D_rotating"
#  walk(temps, ~save_map(.x, z.name="z", lon=-70, lat=50, n.period=30, n.frames=n, col=pal, type="maptiles", suffix=suffix, z.range=rng))
#  

## ------------------------------------------------------------------------
#  data(bathymetry)
#  bath <- map(1:n, ~mutate(bathymetry, frameID = .x))
#  rng_bath <- range(bath[[1]]$z, na.rm=TRUE)
#  pal_bath <- c("black", "steelblue4")
#  
#  walk(bath, ~save_map(.x, z.name="z", id=id, n.frames=n, col=pal_bath, type="maptiles", suffix="background", z.range=rng_bath))
#  walk(borders, ~save_map(.x, id=id, n.frames=n, col="black", type="maplines", suffix="foreground"))
#  walk(temps, ~save_map(.x, z.name="z", id=id, n.frames=n, col=pal, type="maptiles", suffix="timeseries", z.range=rng))
#  

## ------------------------------------------------------------------------
#  library(parallel)
#  mclapply(bath, save_map, z.name="z", id=id, n.frames=n, col=pal_bath, type="maptiles", suffix="background", z.range=rng_bath, mc.cores=32)
#  mclapply(borders, save_map, id=id, n.frames=n, col="orange", type="maplines", suffix="foreground", mc.cores=32)
#  mclapply(temps, save_map, z.name="z", id=id, n.frames=n, col=pal, type="maptiles", suffix="timeseries", z.range=rng, mc.cores=32)
#  

## ------------------------------------------------------------------------
#  # Serial
#  save_seq(bath, z.name="z", id=id, n.frames=n, col=pal_bath, type="maptiles", suffix="background", z.range=rng_bath)
#  save_seq(borders, id=id, n.frames=n, col="black", type="maplines", suffix="foreground")
#  save_seq(temps, z.name="z", id=id, n.frames=n, col=pal, type="maptiles", suffix="timeseries", z.range=rng)
#  
#  # Parallel, Linux, with 32 CPU cores available
#  save_seq(bath, use_mclapply=TRUE, mc.cores=32,
#    z.name="z", id=id, n.frames=n, col=pal_bath, type="maptiles", suffix="background", z.range=rng_bath)
#  save_seq(borders, use_mclapply=TRUE, mc.cores=32,
#    id=id, n.frames=n, col="black", type="maplines", suffix="foreground")
#  save_seq(temps, use_mclapply=TRUE, mc.cores=32,
#    z.name="z", id=id, n.frames=n, col=pal, type="maptiles", suffix="timeseries", z.range=rng)
#  

## ------------------------------------------------------------------------
#  library(rworldmap)
#  library(rworldxtra) # required for "high" resolution map
#  library(maptools) # required for fortify to work
#  # also recommend installing rgeos
#  
#  spdf <- joinCountryData2Map(countryExData, mapResolution="high")
#  spdf@data$id <- rownames(spdf@data)
#  bio <- ggplot2::fortify(spdf, region="id") %>%
#    left_join(subset(spdf@data, select=c(id, BIODIVERSITY)), by="id")
#  n <- 30
#  bio <- map(1:n, ~mutate(bio, frameID = .x) %>% rename(lon=long))
#  suffix <- "bioDivPolygons_3D_rotating"
#  x1 <- "BIODIVERSITY"
#  clrs <- c("royalblue", "purple", "orange", "yellow")
#  
#  # Return a test map
#  save_map(bio[[1]], z.name=x1, id=id, lon=0, lat=20, n.period=n, n.frames=n, col=clrs, type="polygons", suffix=suffix, save.plot=FALSE, return.plot=TRUE)

## ------------------------------------------------------------------------
#  # Walk over all maps
#  walk(bio, ~save_map(.x, z.name=x1, id=id, lon=0, lat=20, n.period=n, n.frames=n, col=clrs, type="polygons", suffix=suffix))
#  

## ------------------------------------------------------------------------
#  library(raster)
#  proj4 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +to wgs84=0,0,0"
#  r <- raster(extent(-180,180,-90,90), nrow=180, ncol=360, proj4)
#  bio2 <- rasterize(spdf, r, field=x1) %>% rasterToPoints %>%
#    tbl_df() %>% setNames(c("lon", "lat", x1))
#  bio2 <- map(1:n, ~mutate(bio2, frameID = .x))
#  suffix <- "bioDivMaptiles_3D_rotating"
#  
#  # Return a test map
#  save_map(bio2[[1]], z.name=x1, id=id, lon=0, lat=20, n.period=n, n.frames=n, col=clrs, type="maptiles", suffix=suffix, save.plot=FALSE, return.plot=TRUE)

## ------------------------------------------------------------------------
#  # Walk over all maps
#  walk(bio2, ~save_map(.x, z.name=x1, id=id, lon=0, lat=20, n.period=n, n.frames=n, col=clrs, type="maptiles", suffix=suffix))
#  

## ------------------------------------------------------------------------
#  means <- group_by(annualtemps, Year) %>% summarise(Mean=mean(z)) %>%
#    ungroup() %>% mutate(frameID=1:n())
#  xlm <- range(means$Year)
#  ylm <- range(means$Mean)
#  lab <- paste0("ts_", means$frameID[1])
#  walk(means$frameID, ~save_ts(means, x="Year", y="Mean", id=id, cap=.x, col="blue", xlm=xlm, ylm=ylm))
#  
#  # Using implicit iteration looks like this.
#  # Note that the cap argument is not needed.
#  save_seq(means, style="tsline", x="Year", y="Mean", id=id, col="blue", xlm=xlm, ylm=ylm)

