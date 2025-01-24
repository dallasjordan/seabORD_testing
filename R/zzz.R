#' @importFrom memoise memoise
.onLoad <- function(libname, pkgname) {
  # Memoize calc_foragecapture on package load
  memoised_calc_foragecapture <<- memoise::memoise(calc_foragecapture)
}
