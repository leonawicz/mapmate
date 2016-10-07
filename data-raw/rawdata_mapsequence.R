# @knitr setup
library(parallel)
library(rgdal)
library(raster)
library(RcppRoll)
library(data.table)
library(dtplyr)

dir.create(outDir <- "/atlas_scratch/mfleonawicz/projects/map_animations", showWarnings=FALSE)
setwd(outDir)
source("functions.R")
mainDir <- "/Data/Base_Data/Climate/World/World_10min"
histDir <- "historical/AR5_CMIP5_models/GISS-E2-R/tas"
projDir <- "projected/AR5_CMIP5_models/rcp60/GISS-E2-R/tas"
files.hist <- list.files(file.path(mainDir, histDir), full=TRUE)
files.proj <- list.files(file.path(mainDir, projDir), full=TRUE)
files <- c(files.hist, files.proj)
mo <- strsplit(basename(files), "_") %>% purrr::map(7) %>% unlist
yr <- strsplit(basename(files), "_") %>% purrr::map(~substr(.x[8], 1, 4)) %>% unlist
ord <- order(paste(yr, mo))
mo <- mo[ord]
yr <- as.integer(yr[ord])
files <- files[ord]
n <- length(files)

# @knitr load_data
f <- function(i, files){
  print(n-i)
  raster(files[i]) %>% aggregate(40) %>% rotate %>% rasterToPoints %>% data.table %>% tbl_dt() %>%
    mutate(Year=yr[i], Month=mo[i]) %>% setnames(c("lon", "lat", "z", "Year", "Month"))
}
d <- mclapply(1:n, f, files=files, mc.cores=32) %>% bind_rows

# @knitr climate_deltas
clim <- filter(d, Year > 1960 & Year <= 1990) %>% group_by(lon, lat, Month) %>% summarise(z=mean(z))
d <- left_join(d, clim, c("lon", "lat", "Month")) %>% mutate(z=z.x-z.y) %>%
  select(-z.x, -z.y) %>% group_by(lon, lat, Year) %>% filter(Year >= 2010 & Year < 2100)

d.ann <- summarise(d, z=round(mean(z), 2)) %>% ungroup %>% arrange(Year, lon, lat)
saveRDS(d.ann, file="/workspace/UA/mfleonawicz/tmpDir/annualtemps.rds")

d.mon.pt <- ungroup(d) %>% arrange(Year, Month, lon, lat) %>% group_by(Year, Month) %>%
  slice(1) %>% ungroup %>% mutate(Month=as.integer(Month), z=round(z, 2))
saveRDS(d.mon.pt, file="/workspace/UA/mfleonawicz/tmpDir/monthlytemps.rds")

#local
annualtemps <- readRDS("data-raw/annualtemps.rds")
monthlytemps <- readRDS("data-raw/monthlytemps.rds")
devtools::use_data(annualtemps, overwrite=TRUE)
devtools::use_data(monthlytemps, overwrite=TRUE)
