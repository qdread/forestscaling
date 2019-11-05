#' AICc (corrected AIC)
#'
#' @noRd
AICc <- function(n, k, lhat) {
  2 * k - 2 * -lhat + ( 2 * k * (k + 1) ) / ( n - k )
}

#' Insolation at a given latitude (approximation)
#'
#' @param lat Latitude in decimal degrees.
#'
#' @export
insolation <- function(lat) {
  lat <- lat * pi/180 # to radians
  y <- sin(lat)
  0.25 * 1367 * (1 - 0.482 * (3*y^2 - 1)/2)
}

#' Proportion light captured by a crown
#'
#' @param depth Depths of crowns in meters (vector)
#' @param k Light extinction coefficient (>0)
#'
#' @export
pct_light_captured <- function(depth, k) 1 - exp(-k * depth)

#' Midpoints of a vector
#'
#' @param a Numeric vector
#'
#' @return Vector with length \code{length(a) - 1}
#'
#' @export
midpts <- function(a) a[-length(a)] + diff(a)/2

#' Evenly spaced sequence in log space
#'
#' @param x If length 1, the minimum value. If length 2, the range of values.
#' @param y The maximum value if not provided in x.
#' @param l Length of the sequence
#'
#' @export
logseq <- function(x, y = NULL, l) {
  if (length(x) == 2) y <- x[2]
  exp(seq(log(x[1]), log(y), length.out = l))
}
