# mapmate 0.2.1 (Release date: 2016-12-01)

* Added `ffmpeg` function to allow users to make video from image sequences directly from R. This function is still under active development, however, and can only meet basic needs.
`mapmate` is still intended for image sequence generation, not direct animation, and users are encouraged to not depend on `ffmpeg` to work with their image sequences.
* Added tutorial web page providing `ffmpeg` usage and current limitations.
* Replaced `suffix` argument in `save_map` and `save_seq` with 'file', now the full filename preceding the file sequence numbering and file extension, defaulting to `'Rplot'`.
* Updated all package web pages, tutorials and help documentation.
* Minor bug fixes.

# mapmate 0.2.0 (Release date: 2016-11-15)

* Added functions to assist with network maps: `gc_endpoints`, `gc_arcs`, and `gc_paths`.
* Added help documentation and runnable examples of the network-related functions.
* Added `network` data set to package.
* Added unit tests for network-related functions.
* Added tutorial/examples for network maps to the package Github pages.
* Included simple animated gif examples in above page, piggybacked on `animation` package.
* Bug fixes.

# mapmate 0.1.0 (Release date: 2016-11-01)

* Add `type` argument options to `save_map` for points, contour lines, filled contour maps, density/intensity maps
* Added ability to choose between tile- and polygon-based plotting with `type="density"`.
* Contour lines can be added on top of density maps, underneath points, or plotted alone as a substitute for either `type="points"` or `type="density"` maps.
* Added a new web page document that provides plenty of examples with visuals on the usage of different `save_map` arguments
and more thorough converage of the limitations associated with using polygons.
* Remade package website/Github pages to include home page, introductory vignette page, and page showcasing visual examples and current mapping limitations.
* Added orthographic projection example Shiny app into package website page.

# mapmate 0.0.2 (Release date: 2016-10-26)

* Updated functions, help documentation and examples.
* Refactored introductory vignette.
* Additional unit tests included.
* Change to the behavior of `get_lonlat_seq`.
* Additional restrictions imposed on acceptable inputs to functions.
* Explicit, required `id` argument replaced the previously assumed presence of a `frameID` data frame column.
* No more internal function conversion of `id` column name to `frameID` when originally named something else in any of the package functions. Non-standard evaluation is used to avoid dependence on a fixed name.
* Added convenient wrapper function, `save_seq`, for maps or time series line plots processed in either series or parallel (Linux-only, via `parallel::mclapply`).
* `do_projection` has been generalized to operate on data frames containing multiple unique plot frame ID values in the specified `id` column.
* `do_projection` has been generalized to output the subsetted data frame with its original columns as before, or with `keep=TRUE`, the entire input data frame along with an additional boolean `inview` column.
* Bug fixes

# mapmate 0.0.1 (Release date: 2016-10-20)

* Added a `NEWS.md` file to track changes to the package.
* Initial non-`.9000` development release.
* Added stability and more consistent functionality.
* Added examples to introduction vignette.
