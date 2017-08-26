# Clone this repository

When this repository is cloned, a number of untracked files will be missing,
specifically for website documentation via `pkgdown::build_site`.
These will (mostly) be regenerated automatically when rebuilding the site, but can take some time from scratch.
This is because hundreds of image files must be created for making animations.
Additionally, there will not yet be any cache directories associated with `knitr` activity.
Once the files are regenerated and chaching has occurred, subsequent website building is relatively quick.

## Special note

`networks.Rmd` should be run interactively the first time in order to generate `networks2D.gif` and `networks3D.gif`.
Make sure these outputs are in the `docs/articles` directory.
This is necessary because most code blocks in this vignette are not evaluated,
but when it is run, it expects to find these two gifs in the directory in order to display them in the document.

## Unincluded data files

Project data is backed up and can be found here if needed:

* /workspace/UA/mfleonawicz/data/mapmate/data-raw/
