build_site <- function(new_dir, files=NULL){
  wd <- getwd()
  setwd(new_dir)
  if(is.null(files)) rmarkdown::render_site() else rmarkdown::render_site(files)
  on.exit(setwd(wd))
}
