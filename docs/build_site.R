wd <- getwd()
setwd("C:/github/mapmate/docs")
rmarkdown::render_site()
setwd(wd)
