create_rdump <- function(dat, xvar, yvar, file_name = NULL, subsample = NULL, random_seed = NULL, x_range = c(1, 286)) {

  # Subset of data if specified
  if (!is.null(subsample) && nrow(dat) > subsample) {
    if (!is.null(random_seed)) set.seed(random_seed)
    dat <- dat[sample(nrow(dat), subsample, replace = FALSE), ]
  }

  # Create list. Min and max x are taken from data, lower and upper limits are specified manually (the latter is a legacy from the Weibull fits)
  x <- dat[, xvar]
  y <- dat[, yvar]
  xdat <- list(N = length(x), x = x, y = y, x_min = min(x), x_max = max(x), LL = x_range[1], UL = x_range[2])

  if (!is.null(file_name)) {
    with(xdat, rstan::stan_rdump(names(xdat), file = file_name))
  } else {
    return(xdat)
  }
}
