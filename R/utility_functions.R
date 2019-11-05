#' AICc (corrected AIC)
#'
#' @noRd
AICc <- function(n, k, lhat) {
  2 * k - 2 * -lhat + ( 2 * k * (k + 1) ) / ( n - k )
}

#' Insolation at a given latitude (approximation)
#' @export
insolation <- function(lat) {
  lat <- lat * pi/180 # to radians
  y <- sin(lat)
  0.25 * 1367 * (1 - 0.482 * (3*y^2 - 1)/2)
}
