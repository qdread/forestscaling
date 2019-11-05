#' Extract parameter values and their credible intervals from Stan fit
#' @export
param_values <- function(fit, pars) {
  summ_fit <- summary(fit)
  param_cis <- summ_fit[[1]][pars, , drop = FALSE]

  param_cis <- data.frame(parameter = dimnames(param_cis)[[1]], param_cis)
  names(param_cis)[5:9] <- c('q025', 'q25', 'q50', 'q75', 'q975')

  return(param_cis)
}

#' Get fitted values and credible/prediction intervals from Stan fit
#'
#' Works for density, for production, or for total production. Fit should be a list. If total production, it's \code{list(density, production)}
#'
#'
#' @export
fitted_predicted_values <- function(fit, variable, dbh_pred, dens_form = NA, prod_form = NA, x_min = NULL, n_indiv = 1, ll = 1.1, ul = 316, pars_to_get, scaling_var = 'dbh') {

  # Use high and low normal quantiles to get prediction intervals
  prediction_quantile <- function(p, fitted_values) {
    pred_raw <- apply(fitted_values, 2, function(mus) exp(qnorm(p, mean = log(mus), sd = pars_prod[,'sigma'])))
    apply(pred_raw, 2, median, na.rm = TRUE)
  }

  qprobs <- c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)

  if (variable == 'density' | variable == 'total_production') {
    pars_dens <- as.data.frame(do.call('cbind', rstan::extract(fit[['density']], pars_to_get[['density']])))

    if (dens_form == '1') {
      dens_fitted <- sapply(dbh_pred, pdf_pareto, xmin = x_min, alpha = pars_dens[,'alpha'])
    }
    if (dens_form == '2') {
      dens_fitted <- sapply(dbh_pred, pdf_2part, xmin = x_min, alpha_low = pars_dens[,'alpha_low'], alpha_high = pars_dens[,'alpha_high'], tau = pars_dens[,'tau'])
    }
    if (dens_form == '3') {
      dens_fitted <- sapply(dbh_pred, pdf_3part, xmin = x_min, alpha_low = pars_dens[,'alpha_low'], alpha_mid = pars_dens[,'alpha_mid'], alpha_high = pars_dens[,'alpha_high'], tau_low = pars_dens[,'tau_low'], tau_high = pars_dens[,'tau_high'])
    }
    if (dens_form == 'w') {
      # Must manually rescale and remove upper and lower truncations
      trunc_pts <- purrr::pmap(pars_dens, function(m, n, ...) pweibull(q = c(ll,ul), shape = m, scale = n))
      dens_fitted <- sapply(dbh_pred, dweibull, shape = pars_dens[,'m'], scale = pars_dens[,'n'])
      dens_fitted <- t(sapply(1:nrow(dens_fitted), function(i) {
        x <- dens_fitted[i,]/diff(trunc_pts[[i]])
        x
      }))
    }
    if (dens_form == 'ln') {
      dens_fitted <- sapply(dbh_pred, dlnorm, meanlog = pars_dens[,'mu_logn'], sdlog = pars_dens[,'sigma_logn'])
    }
    dens_fitted <- dens_fitted * n_indiv
    dens_fitted_quant <- apply(dens_fitted, 2, quantile, probs = qprobs, na.rm = TRUE)
  }

  if (variable == 'production' | variable == 'total_production') {
    pars_prod <- as.data.frame(do.call('cbind', rstan::extract(fit[['production']], c(pars_to_get[['production']]))))
    if (prod_form == '1') {
      prod_fitted <- sapply(dbh_pred, powerlaw_log, beta0 = pars_prod[,'beta0'], beta1 = pars_prod[,'beta1'])
    }
    if (prod_form == '2') {
      prod_fitted <- sapply(dbh_pred, powerlaw_hinge_log, beta0 = pars_prod[,'beta0'], beta1_low = pars_prod[,'beta1_low'], beta1_high = pars_prod[,'beta1_high'], x0 = pars_prod[,'x0'], delta = pars_prod[,'delta'])
    }
    prod_fitted_quant <- apply(prod_fitted, 2, quantile, probs = qprobs, na.rm = TRUE)
    prod_pred_quant <- purrr::map_dfc(qprobs, prediction_quantile, fitted_values = prod_fitted)
  }

  if (variable == 'total_production') {
    totalprod_fitted <- dens_fitted * prod_fitted

    totalprod_fitted_quant <- apply(totalprod_fitted, 2, quantile, probs = qprobs, na.rm = TRUE)
    totalprod_pred_quant <- purrr::map_dfc(qprobs, prediction_quantile, fitted_values = totalprod_fitted)
  }

  if (variable == 'density') {
    out <- data.frame(dbh = dbh_pred,
                      variable = rep('density', length(dbh_pred)),
                      t(dens_fitted_quant))
  }
  if (variable == 'production') {
    out <- data.frame(dbh = dbh_pred,
                      variable = rep(c('production','production_fitted'), each = length(dbh_pred)),
                      rbind(as.matrix(prod_pred_quant), t(prod_fitted_quant)))
  }
  if (variable == 'total_production') {
    out <- data.frame(dbh = dbh_pred,
                      variable = rep(c('total_production','total_production_fitted'), each = length(dbh_pred)),
                      rbind(as.matrix(totalprod_pred_quant), t(totalprod_fitted_quant)))
  }

  setNames(out, c(scaling_var, 'variable', 'q025', 'q05', 'q25', 'q50', 'q75', 'q95', 'q975'))
}


#' Get the fitted log slope values at many points from Stan fit
#' @export
fitted_slope_ci <- function(fit, variable, dbh_pred, dens_form, prod_form, x_min = NULL, n_indiv = 1, ll = 1.1, ul = 316, pars_to_get, scaling_var = 'dbh') {

  qprobs <- c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)

  # Here, get the fitted log slope for all the predicted values. (x is dbh_pred and y is each column of fitted value)
  # Uses formula x/y dy/dx for log slope.
  log_slope <- function(x, y) (x[-1]/y[-1]) * diff(y)/diff(x)

  if (variable == 'density' | variable == 'total_production') {
    pars_dens <- as.data.frame(do.call('cbind', rstan::extract(fit[['density']], pars_to_get[['density']])))

    if (dens_form == '1') {
      dens_fitted <- sapply(dbh_pred, pdf_pareto, xmin = x_min, alpha = pars_dens[,'alpha'])
    }
    if (dens_form == '2') {
      dens_fitted <- sapply(dbh_pred, pdf_2part, xmin = x_min, alpha_low = pars_dens[,'alpha_low'], alpha_high = pars_dens[,'alpha_high'], tau = pars_dens[,'tau'])
    }
    if (dens_form == '3') {
      dens_fitted <- sapply(dbh_pred, pdf_3part, xmin = x_min, alpha_low = pars_dens[,'alpha_low'], alpha_mid = pars_dens[,'alpha_mid'], alpha_high = pars_dens[,'alpha_high'], tau_low = pars_dens[,'tau_low'], tau_high = pars_dens[,'tau_high'])
    }
    if (dens_form == 'w') {
      # Must manually rescale and remove upper and lower truncations
      trunc_pts <- purrr::pmap(pars_dens, function(m, n, ...) pweibull(q = c(ll,ul), shape = m, scale = n))
      dens_fitted <- sapply(dbh_pred, dweibull, shape = pars_dens[,'m'], scale = pars_dens[,'n'])
      dens_fitted <- t(sapply(1:nrow(dens_fitted), function(i) {
        x <- dens_fitted[i,]/diff(trunc_pts[[i]])
        x
      }))
    }
    if (dens_form == 'ln') {
      dens_fitted <- sapply(dbh_pred, dlnorm, meanlog = pars_dens[,'mu_logn'], sdlog = pars_dens[,'sigma_logn'])
    }
    dens_fitted <- dens_fitted * n_indiv
    dens_fittedslopes <- purrr::map_dfr(as.data.frame(t(dens_fitted)), ~ log_slope(dbh_pred, .))
    dens_fittedslopes_quant <- apply(dens_fittedslopes, 1, quantile, probs = qprobs, na.rm = TRUE)
  }

  if (variable == 'production' | variable == 'total_production') {
    pars_prod <- as.data.frame(do.call('cbind', rstan::extract(fit[['production']], pars_to_get[['production']])))
    if (prod_form == '1') {
      prod_fitted <- sapply(dbh_pred, powerlaw_log, beta0 = pars_prod[,'beta0'], beta1 = pars_prod[,'beta1'])
    }
    if (prod_form == '2') {
      prod_fitted <- sapply(dbh_pred, powerlaw_hinge_log, beta0 = pars_prod[,'beta0'], beta1_low = pars_prod[,'beta1_low'], beta1_high = pars_prod[,'beta1_high'], x0 = pars_prod[,'x0'], delta = pars_prod[,'delta'])
    }
    prod_fittedslopes <- purrr::map_dfr(as.data.frame(t(prod_fitted)), ~ log_slope(dbh_pred, .))
    prod_fittedslopes_quant <- apply(prod_fittedslopes, 1, quantile, probs = qprobs, na.rm = TRUE)
  }

  if (variable == 'total_production') {
    totalprod_fitted <- dens_fitted * prod_fitted


    totalprod_fittedslopes <- purrr::map_dfr(as.data.frame(t(totalprod_fitted)), ~ log_slope(dbh_pred, .))
    totalprod_fittedslopes_quant <- apply(totalprod_fittedslopes, 1, quantile, probs = qprobs, na.rm = TRUE)
  }

  if (variable == 'density') {
    out <- data.frame(dbh = dbh_pred[-1],
                      variable = rep('density', length(dbh_pred)-1),
                      t(dens_fittedslopes_quant))
  }
  if (variable == 'production') {
    out <- data.frame(dbh = dbh_pred[-1],
                      variable = rep('production', length(dbh_pred)-1),
                      t(prod_fittedslopes_quant))
  }
  if (variable == 'total_production') {
    out <- data.frame(dbh = dbh_pred[-1],
                      variable = rep('total_production', length(dbh_pred)-1),
                      t(totalprod_fittedslopes_quant))
  }

  setNames(out, c(scaling_var, 'variable', 'q025', 'q05', 'q25', 'q50', 'q75', 'q95', 'q975'))

}


#' Get the Bayesian R-squared and its quantiles from Stan fit
#' @export
bayesian_rsquared_production <- function(fit, x, y, prod_model) {
  # 3. Extract parameter estimates.
  production_par <- list('1' = c('beta0', 'beta1'),
                         '2' = c('beta0', 'beta1_low', 'beta1_high', 'x0', 'delta'))

  pars_to_get <- production_par[[prod_model]]

  pars <- rstan::extract(fit, pars_to_get)
  pars <- as.data.frame(do.call(cbind, pars))

  # 4. Plug in dbh (x) to get posterior estimates of linear predictor of production

  # Take the log of the fitted values
  if (prod_model == '1') {
    prod_fitted <- log(do.call(rbind, purrr::pmap(pars, powerlaw_log, x = x)))
  } else {
    prod_fitted <- log(do.call(rbind, purrr::pmap(pars, powerlaw_hinge_log, x = x)))
  }

  # 5. Get residuals by subtracting log y from linear predictor
  resids <- -1 * sweep(prod_fitted, 2, log(y))

  # 6. Calculate variances and ratio
  pred_var <- apply(prod_fitted, 1, var)
  resid_var <- apply(resids, 1, var)
  r2s <- pred_var / (pred_var + resid_var)

  # Quantiles of rsq
  r2_quant <- quantile(r2s, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975), na.rm = TRUE)
  setNames(r2_quant, c('q025', 'q05', 'q25', 'q50', 'q75', 'q95', 'q975'))
}
