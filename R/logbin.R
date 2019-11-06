#' Logarithmic binning function
#'
#' @param x Vector of values to be binned in the x dimension. Usually diameter.
#' @param y Vector of values to be binned in the y dimension (not required, defaults to \code{NULL}). Usually something like biomass production.
#' @param n Integer, number of bins
#'
#' @return A data frame with columns \code{bin_midpoint} (midpoint of bins on x-axis), \code{bin_value} (height of bins), \code{bin_count} (number of individuals in bins), \code{bin_min} (lower edge of bins on x-axis), \code{bin_max} (upper edge of bins on x-axis).
#'
#' @seealso \code{\link{logbin_setedges}}
#' @export
logbin <- function(x, y = NULL, n) {
  logx <- log10(x)                                           # log transform x value (biomass)
  bin_edges <- seq(min(logx), max(logx), length.out = n + 1) # get edges of bins
  logxbin <- rep(NA, length(logx))                           # create data structure to assign trees to bins
  b <- bin_edges                                             # add a little to the biggest bin temporarily
  b[length(b)] <- b[length(b)] + 1                           # (so that the biggest single tree is put in a bin)
  for (i in 1:length(logx)) {
    logxbin[i] <- sum(logx[i] >= b)                          # assign each tree to a bin
  }
  bin_midpoints <- numeric(n)
  for (i in 1:n) {
    bin_midpoints[i] <- mean(10^(bin_edges[i:(i+1)]))        # backtransform bin edges to linear, and get midpoints
  }
  bin_widths <- diff(10^bin_edges)                           # get linear width of each bin
  bin_factor <- factor(logxbin, levels=1:n)                  # convert bin to factor (required to deal with zeroes if present)
  bin_counts <- table(bin_factor)                            # find number of trees in each bin
  if (!is.null(y)) {
    rawy <- tapply(y, bin_factor, sum)                       # sum y value (production) in each bin
    rawy[is.na(rawy)] <- 0                                   # add zeroes back in if present
    bin_values <- as.numeric(rawy/bin_widths)                # divide production by width for each bin
  }
  else {
    bin_values <- as.numeric(bin_counts/bin_widths)          # 1-dimensional case.
  }

  return(data.frame(bin_midpoint = bin_midpoints,            # return result!
                    bin_value = bin_values,                  # also add bin min and max for bar plot purposes
                    bin_count = as.numeric(bin_counts),
                    bin_min = 10^bin_edges[1:n],
                    bin_max = 10^bin_edges[2:(n+1)]))

}

#' Logarithmic binning function with predefined edges
#'
#' @param x Vector of values to be binned in the x dimension. Usually diameter.
#' @param y Vector of values to be binned in the y dimension (not required, defaults to \code{NULL}). Usually something like biomass production.
#' @param edges Data frame that is output by \code{logbin()}. The bin edges from this data frame are used as pre-defined bin edges to bin \code{x} and \code{y}.
#'
#' @return A data frame with columns \code{bin_midpoint} (midpoint of bins on x-axis), \code{bin_value} (height of bins), \code{bin_count} (number of individuals in bins), \code{bin_min} (lower edge of bins on x-axis), \code{bin_max} (upper edge of bins on x-axis).
#'
#' @seealso \code{\link{logbin}}
#' @export
logbin_setedges <- function(x, y = NULL, edges) {
  logx <- log10(x)                                           # log transform x value (biomass)
  bin_edges <- log10(c(edges$bin_min, edges$bin_max[length(edges$bin_max)]))
  n <- length(edges$bin_min)
  logxbin <- rep(NA, length(logx))                           # create data structure to assign trees to bins
  b <- bin_edges                                             # add a little to the biggest bin temporarily
  b[length(b)] <- b[length(b)] + 1                           # (so that the biggest single tree is put in a bin)
  for (i in 1:length(logx)) {
    logxbin[i] <- sum(logx[i] >= b)                          # assign each tree to a bin
  }
  bin_midpoints <- edges$bin_midpoint
  bin_widths <- diff(10^bin_edges)                           # get linear width of each bin
  bin_factor <- factor(logxbin, levels=1:n)                  # convert bin to factor (required to deal with zeroes if present)
  bin_counts <- table(bin_factor)                            # find number of trees in each bin
  if (!is.null(y)) {
    rawy <- tapply(y, bin_factor, sum)                       # sum y value (production) in each bin
    rawy[is.na(rawy)] <- 0                                   # add zeroes back in if present
    bin_values <- as.numeric(rawy/bin_widths)                # divide production by width for each bin
  }
  else {
    bin_values <- as.numeric(bin_counts/bin_widths)          # 1-dimensional case.
  }

  return(data.frame(bin_midpoint = bin_midpoints,            # return result!
                    bin_value = bin_values,                  # also add bin min and max for bar plot purposes
                    bin_count = as.numeric(bin_counts),
                    bin_min = 10^bin_edges[1:n],
                    bin_max = 10^bin_edges[2:(n+1)]))

}

#' Combine bins across multple censuses
#' @export
bin_across_years <- function(binlist) {
  binvals <- do.call('cbind', lapply(binlist, '[', , 'bin_value'))
  binindivs <- do.call('cbind', lapply(binlist, '[', , 'bin_count'))
  data.frame(bin_midpoint = binlist[[1]]$bin_midpoint,
             bin_min = binlist[[1]]$bin_min,
             bin_max = binlist[[1]]$bin_max,
             bin_yvalue = apply(binvals, 1, median),
             bin_ymin = apply(binvals, 1, min),
             bin_ymax = apply(binvals, 1, max),
             mean_n_individuals = apply(binindivs, 1, mean))
}

#' Create bins of continuous data across one or more censuses
#' @export
fakebin_across_years <- function(dat_values, dat_classes, edges, mean_type = 'geometric', n_census = 5) {
  qprobs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  # add some padding just in case
  mins <- edges$bin_min
  mins[1] <- 0
  maxes <- edges$bin_max
  maxes[length(maxes)] <- Inf

  binstats <- t(sapply(1:length(mins), function(x) {
    indivs <- dat_values[dat_classes >= mins[x] & dat_classes < maxes[x]]
    if (mean_type == 'geometric') {
      mean_n <- exp(mean(log(indivs)))
      sd_n <- exp(sd(log(indivs)))
      ci_width <- qnorm(0.975) * sd(log(indivs)) / sqrt(length(indivs))
      ci_min <- exp(mean(log(indivs)) - ci_width)
      ci_max <- exp(mean(log(indivs)) + ci_width)

    } else {
      mean_n <- mean(indivs)
      sd_n <- sd(indivs)
      ci_width <- qnorm(0.975) * sd(indivs) / sqrt(length(indivs))
      ci_min <- mean_n - ci_width
      ci_max <- mean_n + ci_width
    }
    c(mean = mean_n,
      sd = sd_n,
      quantile(indivs, probs = qprobs),
      ci_min = ci_min,
      ci_max = ci_max)
  }))
  dimnames(binstats)[[2]] <- c('mean', 'sd', 'q025', 'q25', 'median', 'q75', 'q975', 'ci_min', 'ci_max')
  data.frame(bin_midpoint = edges$bin_midpoint,
             bin_min = edges$bin_min,
             bin_max = edges$bin_max,
             mean_n_individuals = edges$bin_count / n_census,
             binstats)
}

#' Fake binning for production by light per crown area
#' @export
binprod <- function(dat, bindat) {
  dat <- do.call('rbind', dat)
  dat$prod_area <- dat$production/dat$crownarea
  dat$light_area <- dat$light_received/dat$crownarea
  dat <- subset(dat, !is.na(light_received))
  with(dat, fakebin_across_years(dat_values = prod_area, dat_classes = light_area, edges = bindat, n_census = 2))
}

#' Fake binning for PCA values
#' @export
binscore <- function(dat, bindat, score_column, class_column) {
  dat <- do.call('rbind', dat)
  dat <- dat[!is.na(dat$light_received) & !is.na(dat[,score_column]), ]
  dat$light_area <- dat$light_received/dat$crownarea
  with(dat, fakebin_across_years(dat_values = dat[,score_column], dat_classes = dat[,class_column], edges = bindat, mean = 'arithmetic', n_census = 2))
}
