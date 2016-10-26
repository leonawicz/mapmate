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
