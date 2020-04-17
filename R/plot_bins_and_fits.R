#' Plot single model fit with multiple functional groups for density
#'
#' @param year_to_plot Default 1995, currently no other years have model fits.
#' @param fg_names Vector of functional group names to plot. Defaults to plot all five plus the total: \code{c('fg1','fg2','fg3','fg4','fg5','all')}
#' @param model_fit Which model to plot. Defaults to 1 (one-segment power law) but 2 was also fit and can be plotted.
#' @param x_limits 2-length vector of minimum and maximum x-value to plot.
#' @param x_breaks Tick values to display on x-axis.
#' @param y_limits 2-length vector of minimum and maximum y-value to plot.
#' @param y_breaks Tick values to display on y-axis.
#' @param y_labels Labels to show at the tick values on the y-axis (in case you want different notation).
#' @param fill_names Vector of fill colors. Must be same length as \code{fg_names}.
#' @param color_names Vector of line colors. Must be same length as \code{fg_names}.
#' @param x_name Title of x-axis.
#' @param y_name Title of y-axis.
#' @param geom_size Size of points to plot. Default is 4.
#' @param obsdat Name of R object with observed density values.
#' @param preddat Name of R object with fitted density values.
#' @param plot_abline Whether to include reference line, default TRUE
#' @param abline_slope Slope of the optional reference line, default -2
#' @param abline_intercept Intercept of the optional reference line, default 4
#'
#'
#' @export
plot_dens <- function(year_to_plot = 1995,
                      fg_names = c('fg1','fg2','fg3','fg4','fg5','all'),
                      model_fit = 1,
                      x_limits,
                      x_breaks = c(1, 3, 10, 30, 100,300),
                      y_limits,
                      y_breaks,
                      y_labels,
                      fill_names = c("black","#BFE046", "#267038", "#27408b", "#87Cefa", "gray87"),
                      color_names = c("black","#BFE046", "#267038", "#27408b", "#87Cefa", "gray"),
                      x_name = 'Diameter (cm)',
                      y_name = expression(paste('Density (n ha'^-1,'cm'^-1,')')),
                      geom_size = 4,
                      obsdat = obs_dens,
                      preddat = pred_dens,
                      plot_abline = TRUE,
                      abline_slope = -2,
                      abline_intercept = 4
) {

  obsdat <- obsdat %>%
    dplyr::filter(fg %in% fg_names, year == year_to_plot, bin_count > 10) %>%
    dplyr::filter(bin_value > 0) %>%
    dplyr::arrange(dplyr::desc(fg))

  # Get minimum and maximum observed bin value for each group to be plotted
  # Delete points on the predicted line that are outside of this range
  obs_limits <- obsdat %>%
    dplyr::group_by(fg) %>%
    dplyr::summarize(min_obs = min(bin_midpoint), max_obs = max(bin_midpoint))

  preddat <- preddat %>%
    dplyr::left_join(obs_limits) %>%
    dplyr::filter(dens_model %in% model_fit, fg %in% fg_names, year == year_to_plot) %>%
    dplyr::filter_at(dplyr::vars(dplyr::starts_with('q')), dplyr::all_vars(. > min(y_limits))) %>%
    dplyr::filter(dbh >= min_obs & dbh <= max_obs) %>%
    dplyr::arrange(dplyr::desc(fg))

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = preddat, ggplot2::aes(x = dbh, ymin = q025, ymax = q975, group = fg, fill = fg), alpha = 0.4)

  if (plot_abline) {
    p <- p + ggplot2::geom_abline(intercept = abline_intercept, slope = abline_slope, color ="gray72",linetype="dashed", size=.75)
  }

  p +
    ggplot2::geom_line(data = preddat, ggplot2::aes(x = dbh, y = q50, group = fg, color = fg)) +
    ggplot2::geom_line(data = preddat[preddat$fg == "fg5",], ggplot2::aes(x = dbh, y = q50), color = "gray")+ # white circles get gray line
    ggplot2::geom_point(data = obsdat, ggplot2::aes(x = bin_midpoint, y = bin_value, group = fg, fill=fg), size = geom_size, shape=21,color="black") +
    ggplot2::scale_x_log10(name = x_name, limits = x_limits, breaks = x_breaks) +
    ggplot2::scale_y_log10(name = y_name, limits = y_limits, breaks = y_breaks,labels = y_labels) +
    ggplot2::scale_color_manual(values = color_names) +
    ggplot2::scale_fill_manual(values = fill_names) +
    theme_plant()


}

#' Plot single model fit with multiple functional groups for individual production
#'
#' @param year_to_plot Default 1995, currently no other years have model fits.
#' @param fg_names Vector of functional group names to plot. Defaults to plot all five plus the total: \code{c('fg1','fg2','fg3','fg4','fg5','all')}
#' @param model_fit Which model to plot. Defaults to 1 (one-segment power law) but 2 was also fit and can be plotted.
#' @param x_limits 2-length vector of minimum and maximum x-value to plot.
#' @param x_breaks Tick values to display on x-axis.
#' @param y_limits 2-length vector of minimum and maximum y-value to plot.
#' @param y_breaks Tick values to display on y-axis.
#' @param y_labels Labels to show at the tick values on the y-axis (in case you want different notation).
#' @param fill_names Vector of fill colors. Must be same length as \code{fg_names}.
#' @param color_names Vector of line colors. Must be same length as \code{fg_names}.
#' @param x_name Title of x-axis.
#' @param y_name Title of y-axis.
#' @param average Which type of average to plot for the binned points. Default is \code{"mean"} for arithmetic mean.
#' @param geom_size Size of points to plot. Default is 4.
#' @param obsdat Name of R object with observed production values.
#' @param preddat Name of R object with fitted production values.
#' @param plot_abline Whether to include reference line, default TRUE
#' @param abline_slope Slope of the optional reference line, default 2
#' @param abline_intercept Intercept of the optional reference line, default -1.6
#'
#' @export
plot_prod <- function(year_to_plot = 1995,
                      fg_names = c('fg1','fg2','fg3','fg4','fg5','all'),
                      model_fit = 1,
                      x_limits,
                      x_breaks = c(1, 3, 10, 30, 100,300),
                      y_limits,
                      y_labels,
                      y_breaks,
                      fill_names = c("#BFE046", "#267038", "#27408b", "#87Cefa", "gray87"),
                      color_names = c("#BFE046", "#267038", "#27408b", "#87Cefa", "gray"),
                      x_name = 'Diameter (cm)',
                      y_name = expression(paste('Growth (kg y'^-1,')')),
                      average = 'mean',
                      plot_errorbar = FALSE,
                      error_min = 'ci_min',
                      error_max = 'ci_max',
                      error_bar_width = 0.1,
                      error_bar_thickness = 0.5,
                      dodge_width = 0.03,
                      dodge_errorbar = TRUE,
                      geom_size = 4,
                      obsdat = obs_indivprod,
                      preddat = fitted_indivprod,
                      plot_abline = TRUE,
                      abline_slope = 2,
                      abline_intercept = -1.6
) {

  pos <- if (dodge_errorbar) ggplot2::position_dodge(width = dodge_width) else 'identity'

  obsdat <- obsdat %>%
    dplyr::filter(fg %in% fg_names, year == year_to_plot, !is.na(mean), mean_n_individuals > 10) %>%
    dplyr::group_by(bin_midpoint) %>%
    dplyr::mutate(width = error_bar_width * dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(fg))

  obs_limits <- obsdat %>%
    dplyr::group_by(fg) %>%
    dplyr::summarize(min_obs = min(bin_midpoint), max_obs = max(bin_midpoint))

  preddat <- preddat %>%
    dplyr::left_join(obs_limits) %>%
    dplyr::filter(prod_model %in% model_fit, fg %in% fg_names, year == year_to_plot) %>%
    dplyr::filter_at(dplyr::vars(dplyr::starts_with('q')), dplyr::all_vars(. > min(y_limits))) %>%
    dplyr::filter(dbh >= min_obs & dbh <= max_obs) %>%
    dplyr::arrange(dplyr::desc(fg))

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = preddat, ggplot2::aes(x = dbh, ymin = q025, ymax = q975, group = fg, fill = fg), alpha = 0.4) +
    ggplot2::geom_line(data = preddat, ggplot2::aes(x = dbh, y = q50, group = fg, color = fg))

  if (plot_errorbar) {
    p <- p + ggplot2::geom_errorbar(data = obsdat, ggplot2::aes_string(x = 'bin_midpoint', ymin = error_min, ymax = error_max, group = 'fg', color = 'fg', width = 'width'), position = pos, size = error_bar_thickness)
  }

  p <- p +
    ggplot2::geom_line(data = preddat[preddat$fg == "fg5",], ggplot2::aes(x = dbh, y = q50), color = "gray")+ # white circles get gray line
    ggplot2::geom_point(data = obsdat, ggplot2::aes_string(x = 'bin_midpoint', y = average, group = 'fg', fill = 'fg'),
               size = geom_size,color="black",shape=21, position = pos) +
    ggplot2::scale_x_log10(name = x_name, limits = x_limits, breaks = x_breaks)+
    ggplot2::scale_y_log10(name = y_name, limits = y_limits, breaks = y_breaks, labels = y_labels) +
    theme_no_x() +
    ggplot2::theme(rect = ggplot2::element_rect(fill = "transparent"))+ # all rectangles
    ggplot2::scale_color_manual(values = color_names) +
    ggplot2::scale_fill_manual(values = fill_names) +
    theme_plant()

  if (plot_abline) {
    p <- p + ggplot2::geom_abline(intercept = abline_intercept, slope = abline_slope, color ="gray72",linetype="dashed", size=.75)
  }
  p
}

#' Plot single model fit with multiple functional groups for total production
#'
#' Specify both density and production type
#'
#'
#' @param year_to_plot Default 1995, currently no other years have model fits.
#' @param fg_names Vector of functional group names to plot. Defaults to plot all five plus the total: \code{c('fg1','fg2','fg3','fg4','fg5','all')}
#' @param model_fit_density Which density model to plot. Defaults to 1 (one-segment power law) but 2 was also fit and can be plotted.
#' @param model_fit_production Which production model to plot. Defaults to 1 (one-segment power law) but 2 was also fit and can be plotted.
#' @param x_limits 2-length vector of minimum and maximum x-value to plot.
#' @param x_breaks Tick values to display on x-axis.
#' @param y_limits 2-length vector of minimum and maximum y-value to plot.
#' @param y_breaks Tick values to display on y-axis.
#' @param y_labels Labels to show at the tick values on the y-axis (in case you want different notation).
#' @param fill_names Vector of fill colors. Must be same length as \code{fg_names}.
#' @param color_names Vector of line colors. Must be same length as \code{fg_names}.
#' @param x_name Title of x-axis.
#' @param y_name Title of y-axis.
#' @param geom_size Size of points to plot. Default is 4.
#' @param obs_dens Name of R object with observed density values.
#' @param pred_dens Name of R object with fitted density values.
#' @param plot_abline Whether to include reference line, default TRUE
#' @param abline_slope Slope of the optional reference line, default 0
#' @param abline_intercept Intercept of the optional reference line, default 2
#'
#' @export
plot_totalprod <- function(year_to_plot = 1995,
                           fg_names = c('fg1','fg2','fg3','fg4','fg5','all'),
                           model_fit_density = 1,
                           model_fit_production = 1,
                           x_limits,
                           x_breaks = c(1, 3, 10, 30, 100,300),
                           y_limits = c(0.03,100),
                           y_breaks = c(0.01,0.1, 1, 10,100, 1000),
                           y_labels,
                           fill_names = c("black","#BFE046", "#267038", "#27408b", "#87Cefa", "gray87"),
                           color_names = c("black","#BFE046", "#267038", "#27408b", "#87Cefa", "gray"),
                           x_name = 'Diameter (cm)',
                           y_name = expression(paste('Production (kg ha'^-1,' cm'^-1,' y'^-1,')')),
                           geom_size = 4,
                           obsdat = obs_totalprod,
                           preddat = fitted_totalprod,
                           plot_abline = TRUE,
                           abline_slope = 0,
                           abline_intercept = 2
) {

  obsdat <- obsdat %>%
    dplyr::filter(fg %in% fg_names, year == year_to_plot, bin_count > 10) %>%
    dplyr::filter(bin_value > 0) %>%
    dplyr::arrange(dplyr::desc(fg))

  obs_limits <- obsdat %>%
    dplyr::group_by(fg) %>%
    dplyr::summarize(min_obs = min(bin_midpoint), max_obs = max(bin_midpoint))

  preddat <- preddat %>%
    dplyr::left_join(obs_limits) %>%
    dplyr::filter(dens_model %in% model_fit_density, prod_model %in% model_fit_production, fg %in% fg_names, year == year_to_plot) %>%
    dplyr::filter_at(dplyr::vars(dplyr::starts_with('q')), dplyr::all_vars(. > min(y_limits))) %>%
    dplyr::filter(dbh >= min_obs & dbh <= max_obs) %>%
    dplyr::arrange(dplyr::desc(fg))

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = preddat, aes(x = dbh, ymin = q025, ymax = q975, group = fg, fill = fg), alpha = 0.4) +
    ggplot2::geom_line(data = preddat, aes(x = dbh, y = q50, group = fg, color = fg)) +
    ggplot2::geom_point(data = obsdat, aes(x = bin_midpoint, y = bin_value,group = fg, fill=fg), size = geom_size, color = "black",shape=21) +
    ggplot2::scale_x_log10(name = x_name, limits = x_limits, breaks = x_breaks) +
    ggplot2::scale_y_log10(name = y_name, limits = y_limits, breaks = y_breaks, labels = y_labels, position = "right") +
    ggplot2::scale_color_manual(values = color_names) +
    ggplot2::scale_fill_manual(values = fill_names) +
    theme_plant() + theme(aspect.ratio = 1)

  if (plot_abline) p <- p + ggplot2::geom_abline(intercept = abline_intercept, slope = abline_slope, color ="gray72",linetype="dashed", size=.75)
  p

}

#' Plot hexbin plot of production and/or fitted production functions
#'
#' Arguments not described. Sorry. See \code{\link{plot_prod}} for similar arguments.
#'
#' @export
plot_prod_withrawdata <- function(year_to_plot = 1995,
                                  fg_names = c('fg1','fg2','fg3','fg4','fg5','unclassified'),
                                  full_names = c('Fast', 'Slow', 'Pioneer', 'Breeder', 'Medium', 'Unclassified'),
                                  func_names = c('power law', '2-segment power law'),
                                  x_limits = c(1, 300),
                                  x_breaks = c(1, 10, 100),
                                  y_limits,
                                  y_breaks,
                                  x_name = 'Diameter (cm)',
                                  y_name = expression(paste('Growth (kg yr'^-1,')')), line_types = c('dashed', 'solid'),
                                  aspect_ratio = 0.75,
                                  hex_scale = ggplot2::scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(9, 'RdYlBu')), bias=1)(50),
                                                                   trans = 'log', name = 'Individuals', breaks = c(1,10,100,1000,10000),
                                                                   labels = c(1,10,100,1000,10000), limits=c(1,10000)),
                                  obsdat = raw_prod,
                                  preddat = fitted_indivprod,
                                  plot_abline = TRUE,
                                  abline_slope = 2,
                                  abline_intercept = -2.1,
                                  plot_fits = FALSE
) {

  obsdat <- obsdat %>%
    dplyr::filter(fg %in% fg_names, year == year_to_plot)

  obs_limits <- obsdat %>%
    dplyr::group_by(fg) %>%
    dplyr::summarize(min_obs = min(production), max_obs = max(production))

  preddat <- preddat %>%
    dplyr::left_join(obs_limits) %>%
    dplyr::filter(fg %in% fg_names, year == year_to_plot) %>%
    dplyr::filter_at(dplyr::vars(dplyr::starts_with('q')), dplyr::all_vars(. > min(y_limits))) %>%
    dplyr::filter(dbh >= min_obs & dbh <= max_obs) %>%
    dplyr::mutate(prod_model = factor(prod_model, labels = func_names))

  labels <- setNames(full_names, fg_names)

  p <- ggplot2::ggplot() +
    ggplot2::geom_hex(data = obsdat, ggplot2::aes(x = dbh_corr, y = production)) +
    ggplot2::facet_wrap(~ fg, ncol = 2, labeller = ggplot2::labeller(fg = labels)) +
    ggplot2::scale_x_log10(name = x_name, limits = x_limits, breaks = x_breaks) +
    ggplot2::scale_y_log10(name = y_name, limits = y_limits, breaks = y_breaks) +
    ggplot2::scale_linetype_manual(name = 'Growth fit', values = line_types) +
    hex_scale +
    theme_plant() +
    ggplot2::coord_fixed(ratio = aspect_ratio) +
    ggplot2::theme(legend.position = c(0.7, 0.15),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size=12),
                   legend.key = ggplot2::element_rect(fill = NA))

  if (plot_abline) p <- p + ggplot2::geom_abline(slope = abline_slope, intercept = abline_intercept, linetype = "dashed") + ggplot2::guides(linetype = 'none')
  if (plot_fits[1]) p <- p + ggplot2::geom_line(data = preddat %>% dplyr::filter(prod_model %in% func_names[plot_fits]), ggplot2::aes(x = dbh, y = q50, group = prod_model, linetype = prod_model), size=0.25)

  return(p)

}

#' Another variant of production plotting function
#' @export
plot_prod_fixed <- function(year_to_plot = 1995,
                            fg_names = c('fg1','fg2','fg3','fg4','fg5','all'),
                            model_fit = 1,
                            x_limits,
                            x_breaks = c(1, 3, 10, 30, 100,300),
                            y_limits,
                            y_labels,
                            y_breaks,
                            fill_names = c("#BFE046", "#267038", "#27408b", "#87Cefa", "ivory"),
                            fill_names0 = c("#BFE046", "#267038", "#27408b", "#87Cefa", "gray"),
                            x_name = 'Diameter (cm)',
                            y_name = expression(paste('Growth (kg y'^-1,')')),
                            average = 'mean',
                            error_quantiles = c('ci_min', 'ci_max'),
                            error_bar_width = 0.03,
                            dodge_width = 0.03,
                            dodge_errorbar = TRUE,
                            geom_size = 4,
                            obsdat = obs_indivprod,
                            preddat = fitted_indivprod

) {




  pos <- if (dodge_errorbar) position_dodge(width = dodge_width) else 'identity'

  obsdat <- obsdat %>%
    dplyr::filter(fg %in% fg_names, year == year_to_plot, !is.na(mean), mean_n_individuals > 10) %>%
    dplyr::group_by(bin_midpoint) %>%
    dplyr::mutate(width = error_bar_width * dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(fg))

  obs_limits <- obsdat %>%
    dplyr::group_by(fg) %>%
    dplyr::summarize(min_obs = min(bin_midpoint), max_obs = max(bin_midpoint))

  preddat <- preddat %>%
    dplyr::left_join(obs_limits) %>%
    dplyr::filter(prod_model %in% model_fit, fg %in% fg_names, year == year_to_plot) %>%
    dplyr::filter_at(vars(starts_with('q')), all_vars(. > min(y_limits))) %>%
    dplyr::filter(dbh >= min_obs & dbh <= max_obs) %>%
    dplyr::arrange(dplyr::desc(fg))

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = preddat, ggplot2::aes(x = dbh, ymin = q025, ymax = q975, group = fg, fill = fg), alpha = 0.4) +
    ggplot2::geom_line(data = preddat, ggplot2::aes(x = dbh, y = q50, group = fg, color = fg)) +
    ggplot2::geom_line(data = preddat[preddat$fg == "fg5",], ggplot2::aes(x = dbh, y = q50), color = "gray")+ # white circles get gray line
    ggplot2::geom_ribbon(data = preddat[preddat$fg == "fg5",],
                         ggplot2::aes(x = dbh, ymin = q025, ymax = q975),
                         fill = "gray", alpha = 0.4) +
    ggplot2::geom_point(data = obsdat, aes_string(x = 'bin_midpoint', y = average, group = 'fg', fill = 'fg'),
                        size = geom_size,color="black",shape=21, position = pos) +
    ggplot2::scale_x_log10(name = x_name, limits = x_limits, breaks = x_breaks) +
    ggplot2::scale_y_log10(name = y_name, limits = y_limits, breaks = y_breaks, labels = y_labels) +
    ggplot2::theme(axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   rect = ggplot2::element_rect(fill = "transparent"))+ # all rectangles
    ggplot2::scale_color_manual(values = fill_names0) +
    ggplot2::scale_fill_manual(values = fill_names) + theme_plant()

}
