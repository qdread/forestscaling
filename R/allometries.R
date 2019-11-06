#' Calculate equivalent diameter at 1.3 m
#'
#' given a diameter measurement in cm (d) taken at a nonstandard height in m(h),
#' and a taper parameter (b1). This is the overall best taper model
#' from Metcalf et el. (2009)
#' @export
apply.eqn1 <- function(d,h,b1) {d/(exp(-b1*(h-1.3)))}

#' Estimate total tree height (m) from diameter (cm)
#'
#' with uncorrected (notaper.H) or taper-corrected (taper.H) allometries parameterized from
#' data from the BCI plot. The function is the weibull function as described in
#' Feldpausch et al. 2012
#' @export
notaper.H <- function(dbh) {43.1427*(1-
                                       exp(-0.04003*dbh^0.82585))
}

#' Estimate total tree height (m) from diameter (cm)
#'
#' with uncorrected (notaper.H) or taper-corrected (taper.H) allometries parameterized from
#' data from the BCI plot. The function is the weibull function as described in
#' Feldpausch et al. 2012
#' @export
taper.H <- function(dbh) {43.4375*(1-
                                     exp(-0.04469*dbh^0.78339))
}


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
