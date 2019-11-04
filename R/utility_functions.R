#' AICc (corrected AIC)
#'
#' @noRd
AICc <- function(n, k, lhat) {
  2 * k - 2 * -lhat + ( 2 * k * (k + 1) ) / ( n - k )
}

