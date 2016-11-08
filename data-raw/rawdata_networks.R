library(dplyr)

d <- readRDS("data-raw/worldcities.rds") %>% tbl_df %>% rename(lon=Longitude, lat=Latitude)
set.seed(47)
min_pop <- 10000
size <- 1000
final_size <- 500
wtsFun <- function(x) (x - min(x)) / (max(x) - min(x)) # simple rescale to [0,1]
distFun <- function(x) 1 - x / max(x) # inverse distance weighting

d <- filter(d, Population >= min_pop) %>% select(lon, lat, Population) %>%
  mutate(Pop_wts=wtsFun(sqrt(Population))) %>% sample_n(size, weight=Pop_wts) # use population-based weights

network <- select(d, -Population) %>% mutate(Pop_wts=round(Pop_wts, 3))
devtools::use_data(network, overwrite=TRUE)
