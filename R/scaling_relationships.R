#' PDF for density model 1 part
#' @export
pdf_pareto <- function(x, xmin, alpha) (alpha * xmin^alpha) / (x ^ (alpha+1))

#' PDF for density model 2 part
#' @export
pdf_2part <- function(x, xmin, alpha_low, alpha_high, tau) {
  C_con <- tau ^ (alpha_low - alpha_high)
  C_norm <- ( (C_con / alpha_low) * (xmin ^ (-alpha_low) - tau ^ (-alpha_low)) + ( tau ^ (-alpha_high) ) / alpha_high ) ^ -1

  prob <- case_when(
    x < tau ~ C_con * C_norm * ( x ^ - (alpha_low + 1) ),
    x >= tau ~ C_norm * ( x ^ - (alpha_high + 1) )
  )
  return(prob)
}

#' PDF for density model 3 part
#' @export
pdf_3part <- function(x, xmin, alpha_low, alpha_mid, alpha_high, tau_low, tau_high) {
  C_con_low <- tau_low ^ (alpha_low - alpha_mid)
  C_con_high <- tau_high ^ (alpha_high - alpha_mid)
  C_norm <- ( (C_con_low / alpha_low) * (xmin ^ -alpha_low - tau_low ^ -alpha_low) + (1 / alpha_mid) * (tau_low ^ -alpha_mid - tau_high ^ -alpha_mid) + (C_con_high / alpha_high) * (tau_high ^ -alpha_high) ) ^ -1

  prob <- case_when(
    x < tau_low ~ C_con_low * C_norm * ( x ^ - (alpha_low + 1) ),
    x >= tau_low & x <= tau_high ~ C_norm * ( x ^ - (alpha_mid + 1) ),
    x > tau_high ~ C_con_high * C_norm * ( x ^ - (alpha_high + 1) )
  )

  return(prob)
}

#' Function for production model 1 part
#' @export
powerlaw_log <- function(x, beta0, beta1) beta0 * x^beta1

#' Function for production model 2 parts (hinged)
#' @export
powerlaw_hinge_log <- function(x, x0, beta0, beta1_low, beta1_high, delta) {
  xdiff <- log(x) - log(x0)
  exp( log(beta0) + beta1_low * xdiff + (beta1_high - beta1_low) * delta * log(1 + exp(Brobdingnag::as.brob(xdiff / delta))) )
}
