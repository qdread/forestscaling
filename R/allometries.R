#' Calculate equivalent diameter at 1.3 m
#'
#' given a diameter measurement in cm (d) taken at a nonstandard height in m(h),
#' and a taper parameter (b1). This is the overall best taper model
#' from Metcalf et el. (2009)
#' @export
apply.eqn1 <- function(d,h,b1) {d/(exp(-b1*(h-1.3)))}

#' Estimate aboveground biomass
#'
#' from wood specific gravity (wsg), tree diameter in centimeters (dbh),
#' and total tree height in meters (H)
#' using allometries from Chave et al. (2005) for most tropical forests.
#' @export
agb.allometry <- function(wsg,dbh,H) {0.0509*wsg*((dbh)^2)*H}

#' Calculate the taper-corrected DBH for all individuals in one data frame.
#' @export
taper_correct_dbh <- function(dat) {
  indiv_coeffs <- spcoeffs[match(dat$sp, names(spcoeffs))] # Match individuals with their species taper coefficients
  indiv_coeffs[is.na(indiv_coeffs)] <- spcoeffs['other']   # All other individuals get "other" coefficient
  b1 <- exp(mdbh * log(dat$dbh/10) + mhom * log(dat$hom) + indiv_coeffs)
  dbh_corr <- dplyr::case_when(
    is.na(dat$pom) ~ as.numeric(NA),                               # If not measured, return NA
    dat$pom == 1.3 | dat$sp %in% palmsp ~ dat$dbh,                 # Don't correct if already at 1.3, or if it's a palm
    TRUE ~ 10 * apply.eqn1(d = dat$dbh / 10, h = dat$hom, b1 = b1) # Otherwise apply the correction.
  )
  return(data.frame(dbh_corr = dbh_corr))
}

#' Generalized Michaelis-Menten function
#'
#' @export
gMM <- function(x, a, b, k) {
  (a * x ^ b) / (k + x ^ b)
}

#' Correction factor to remove bias from log-transformation
#'
#' Cite: Sprugel 1983 (Ecology).
#'
#' @param log_y Log-transformed observed y-values
#' @param log_y_pred Log-transformed fitted y-values
#' @param n_pars Number of parameters in regression (used to calculate degrees of freedom)
#' @param base Base of the log transformation. Default to natural logarithm.
#'
#' @export
log_correction_factor <- function(log_y, log_y_pred, n_pars, base = exp(1)) {
  std_err_est <- (sum((log_y - log_y_pred)^2) / (length(log_y) - n_pars)) ^ 0.5
  base ^ ((std_err_est^2)/2)
}
