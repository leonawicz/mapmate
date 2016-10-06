library(rgdal)
library(raster)
library(data.table)
library(dplyr)

file <- "data-raw/marmap_coord_-180;-90;180;90_res_10.csv"
nam <- c("long", "lat", "z")
agg <- 20

d.bath <- read.csv(file) %>%  data.table %>% setnames(nam)
r <- raster(extent(-180, 180, -90, 90), res=1/6)
projection(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
r <- setValues(r, d.bath$z)

bathymetry <- aggregate(r, agg) %>% rasterToPoints %>% data.table %>% setnames(nam)
borders <- ggplot2::map_data("world")
