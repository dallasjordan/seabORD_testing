#' Memoized Version of calc_foragecapture
#'
#' A memoized version of \code{\link{calc_foragecapture}} using \code{\link[memoise]{memoise}}.
#' This version caches results for improved performance on repeated calls with the same arguments.
#'
#' @usage memoised_calc_foragecapture(...)
#' @seealso \code{\link{calc_foragecapture}}, \code{\link[memoise]{memoise}}
#' @export
memoised_calc_foragecapture <- NULL
