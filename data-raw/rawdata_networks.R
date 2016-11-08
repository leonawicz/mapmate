library(dplyr)

d <- readRDS("data-raw/worldcities.rds") %>% tbl_df %>% rename(lon=Longitude, lat=Latitude)
set.seed(47)
min_pop <- 10000
size <- 10000
wtsFun <- function(x) (x - min(x)) / (max(x) - min(x)) # simple rescale to [0,1]
distFun <- function(x) 1 - x / max(x) # inverse distance weighting

d <- filter(d, Population >= min_pop) %>% select(lon, lat, Population) %>%
  mutate(Pop_wts=wtsFun(sqrt(Population))) %>% sample_n(size, weight=Pop_wts) %>% # use population-based weights
  expand_table("lon", "lat") %>% mutate(Dist_wts=distFun(Dist)) %>%
  sample_n(100, replace=TRUE, weight=(Pop_wts0 + Pop_wts1)/2 + Dist_wts) # use distance-based weights
arcs <- arc_paths(d, "lon0", "lat0", "lon1", "lat1")
devtools::use_data(arcs, overwrite=TRUE)
