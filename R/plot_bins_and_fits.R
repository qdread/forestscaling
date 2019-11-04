# Plot single model fit with multiple functional groups for density
plot_dens <- function(year_to_plot = 1995,
                      fg_names = c('fg1','fg2','fg3','fg4','fg5','all'),
                      model_fit = 'pareto',
                      x_limits,
                      x_breaks = c(1, 3, 10, 30, 100,300),
                      y_limits,
                      y_breaks,
                      y_labels,
                      fill_names = c("black","#BFE046", "#267038", "#27408b", "#87Cefa", "gray87"),
                      fill_names0 = c("black","#BFE046", "#267038", "#27408b", "#87Cefa", "gray"),
                      x_name = 'Diameter (cm)',
                      y_name = expression(paste('Density (n ha'^-1,'cm'^-1,')')),
                      geom_size = 4,
                      obsdat = obs_dens,
                      preddat = pred_dens
) {

  require(dplyr)
  require(ggplot2)

  obsdat <- obsdat %>%
    filter(fg %in% fg_names, year == year_to_plot, bin_count > 10) %>%
    filter(bin_value > 0)

  # Get minimum and maximum observed bin value for each group to be plotted
  # Delete points on the predicted line that are outside of this range
  obs_limits <- obsdat %>%
    group_by(fg) %>%
    summarize(min_obs = min(bin_midpoint), max_obs = max(bin_midpoint))

  preddat <- preddat %>%
    left_join(obs_limits) %>%
    filter(dens_model %in% model_fit, fg %in% fg_names, year == year_to_plot) %>%
    filter_at(vars(starts_with('q')), all_vars(. > min(y_limits))) %>%
    filter(dbh >= min_obs & dbh <= max_obs)

  ggplot() +
    geom_ribbon(data = preddat, aes(x = dbh, ymin = q025, ymax = q975, group = fg, fill = fg), alpha = 0.4) +
    geom_abline(intercept=4, slope = -2, color ="gray72",linetype="dashed", size=.75)+
    geom_line(data = preddat, aes(x = dbh, y = q50, group = fg, color = fg)) +
    geom_line(data = preddat[preddat$fg == "fg5",], aes(x = dbh, y = q50), color = "gray")+ # white circles get gray line
    #geom_ribbon(data = preddat[preddat$fg == "fg5",], aes(x = dbh, ymin = q025, ymax = q975), fill = "gray", alpha = 0.4) +
    geom_point(data = obsdat, aes(x = bin_midpoint, y = bin_value, group = fg, fill=fg), size = geom_size, shape=21,color="black") +
    scale_x_log10(name = x_name, limits = x_limits, breaks = x_breaks) +
    scale_y_log10(name = y_name, limits = y_limits, breaks = y_breaks,labels = y_labels) +
    scale_color_manual(values = fill_names0) +theme_plant +
    scale_fill_manual(values = fill_names)


}

# Plot single model fit with multiple functional groups for individual production
plot_prod <- function(year_to_plot = 1995,
                      fg_names = c('fg1','fg2','fg3','fg4','fg5','all'),
                      model_fit = 'powerlaw',
                      x_limits,
                      x_breaks = c(1, 3, 10, 30, 100,300),
                      y_limits,
                      y_labels,
                      y_breaks,
                      fill_names = c("#BFE046", "#267038", "#27408b", "#87Cefa", "gray87"),
                      fill_names0 = c("#BFE046", "#267038", "#27408b", "#87Cefa", "gray"),
                      x_name = 'Diameter (cm)',
                      y_name = expression(paste('Growth (kg yr'^-1,')')),
                      average = 'mean',
                      error_quantiles = c('ci_min', 'ci_max'),
                      error_bar_width = 0.03,
                      dodge_width = 0.03,
                      dodge_errorbar = TRUE,
                      geom_size = 4,
                      obsdat = obs_indivprod,
                      preddat = fitted_indivprod,
                      plot_abline = TRUE
) {

  require(dplyr)
  require(ggplot2)

  pos <- if (dodge_errorbar) position_dodge(width = dodge_width) else 'identity'

  obsdat <- obsdat %>%
    filter(fg %in% fg_names, year == year_to_plot, !is.na(mean), mean_n_individuals > 10) %>%
    group_by(bin_midpoint) %>% mutate(width = error_bar_width * n()) %>% ungroup

  obs_limits <- obsdat %>%
    group_by(fg) %>%
    summarize(min_obs = min(bin_midpoint), max_obs = max(bin_midpoint))

  preddat <- preddat %>%
    left_join(obs_limits) %>%
    filter(prod_model %in% model_fit, fg %in% fg_names, year == year_to_plot) %>%
    filter_at(vars(starts_with('q')), all_vars(. > min(y_limits))) %>%
    filter(dbh >= min_obs & dbh <= max_obs)

  p <- ggplot() +
    geom_ribbon(data = preddat, aes(x = dbh, ymin = q025, ymax = q975, group = fg, fill = fg), alpha = 0.4) +
    geom_line(data = preddat, aes(x = dbh, y = q50, group = fg, color = fg)) +
    #geom_errorbar(data = obsdat, aes_string(x = 'bin_midpoint', ymin = error_quantiles[1], ymax = error_quantiles[2], group = 'fg', color = 'fg', width = 'width'), position = pos) +
    geom_line(data = preddat[preddat$fg == "fg5",], aes(x = dbh, y = q50), color = "gray")+ # white circles get gray line
    #geom_ribbon(data = preddat[preddat$fg == "fg5",], aes(x = dbh, ymin = q025, ymax = q975), fill = "gray", alpha = 0.4) +
    geom_point(data = obsdat, aes_string(x = 'bin_midpoint', y = average, group = 'fg', fill = 'fg'),
               size = geom_size,color="black",shape=21, position = pos) +
    scale_x_log10()+#name = x_name, limits = x_limits, breaks = x_breaks) +
    scale_y_log10(name = y_name, limits = y_limits, breaks = y_breaks, labels = y_labels) +
    theme_no_x +theme(rect = element_rect(fill = "transparent"))+ # all rectangles
    scale_color_manual(values = fill_names0) +
    scale_fill_manual(values = fill_names) + theme_plant

  if (plot_abline) {
    p <- p + geom_abline(intercept= -1.6, slope = 2, color ="gray72",linetype="dashed", size=.75)
  }
  p
}

# Plot single model fit with multiple functional groups for total production
# Specify both density and production type

plot_totalprod <- function(year_to_plot = 1995,
                           fg_names = c('fg1','fg2','fg3','fg4','fg5','all'),
                           model_fit_density = 'pareto',
                           model_fit_production = 'powerlaw',
                           x_limits,
                           x_breaks = c(1, 3, 10, 30, 100,300),
                           y_limits = c(0.03,100),
                           y_breaks = c(0.01,0.1, 1, 10,100, 1000),
                           y_labels,
                           fill_names = c("black","#BFE046", "#267038", "#27408b", "#87Cefa", "gray87"),
                           fill_names0 = c("black","#BFE046", "#267038", "#27408b", "#87Cefa", "gray"),
                           x_name = 'Diameter (cm)',
                           y_name = expression(paste('Production (kg ha'^-1,' cm'^-1,' yr'^-1,')')),
                           geom_size = 4,
                           obsdat = obs_totalprod,
                           preddat = fitted_totalprod,
                           plot_abline = TRUE
) {

  require(dplyr)
  require(ggplot2)
  obsdat <- obsdat %>%
    filter(fg %in% fg_names, year == year_to_plot, bin_count > 10) %>%
    filter(bin_value > 0)

  obs_limits <- obsdat %>%
    group_by(fg) %>%
    summarize(min_obs = min(bin_midpoint), max_obs = max(bin_midpoint))

  preddat <- preddat %>%
    left_join(obs_limits) %>%
    filter(dens_model %in% model_fit_density, prod_model %in% model_fit_production, fg %in% fg_names, year == year_to_plot) %>%
    filter_at(vars(starts_with('q')), all_vars(. > min(y_limits))) %>%
    filter(dbh >= min_obs & dbh <= max_obs)

  p <- ggplot() +
    geom_ribbon(data = preddat, aes(x = dbh, ymin = q025, ymax = q975, group = fg, fill = fg), alpha = 0.4) +

    geom_line(data = preddat, aes(x = dbh, y = q50, group = fg, color = fg)) +
    #geom_line(data = preddat[preddat$fg == "fg5",], aes(x = dbh, y = q50), color = "gray")+ # white circles get gray line
    #geom_ribbon(data = preddat[preddat$fg == "fg5",], aes(x = dbh, ymin = q025, ymax = q975), fill = "gray", alpha = 0.4) +
    geom_point(data = obsdat, aes(x = bin_midpoint, y = bin_value,group = fg, fill=fg), size = geom_size, color = "black",shape=21) +
    scale_x_log10(name = x_name, limits = x_limits, breaks = x_breaks) +
    scale_y_log10(name = y_name, limits = y_limits, breaks = y_breaks, labels = y_labels, position = "right") +
    scale_color_manual(values = fill_names0) + theme_plant + theme(aspect.ratio = 1) +
    scale_fill_manual(values = fill_names)

  if (plot_abline) p <- p + geom_abline(intercept= 2, slope = 0, color ="gray72",linetype="dashed", size=.75)
  p

}


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
                                  hex_scale = hex_scale_log_colors,
                                  obsdat = raw_prod,
                                  preddat = fitted_indivprod,
                                  plot_abline = TRUE,
                                  plot_fits = FALSE
) {

  require(dplyr)
  require(ggplot2)
  #require(pracma)

  obsdat <- obsdat %>%
    filter(fg %in% fg_names, year == year_to_plot)

  obs_limits <- obsdat %>%
    group_by(fg) %>%
    summarize(min_obs = min(production), max_obs = max(production))

  # Add more identical rows of the f. groups with less data so that the fill scales appear the same on all plots.
  #multiples <- Reduce(Lcm, table(obsdat$fg)) / table(obsdat$fg)
  #obsdat <- obsdat[rep(1:nrow(obsdat), ceiling(multiples/(min(multiples)/3))[obsdat$fg]), ]


  preddat <- preddat %>%
    left_join(obs_limits) %>%
    filter(fg %in% fg_names, year == year_to_plot) %>%
    filter_at(vars(starts_with('q')), all_vars(. > min(y_limits))) %>%
    filter(dbh >= min_obs & dbh <= max_obs) %>%
    mutate(prod_model = factor(prod_model, labels = func_names))

  labels <- setNames(full_names, fg_names)

  p <- ggplot() +
    geom_hex(data = obsdat, aes(x = dbh_corr, y = production)) +
    facet_wrap(~ fg, ncol = 2, labeller = labeller(fg = labels)) +
    scale_x_log10(name = x_name, limits = x_limits, breaks = x_breaks) +
    scale_y_log10(name = y_name, limits = y_limits, breaks = y_breaks) +
    scale_linetype_manual(name = 'Growth fit', values = line_types) +
    hex_scale +
    theme_plant +
    coord_fixed(ratio = aspect_ratio) +
    theme(legend.position = c(0.7, 0.15), strip.background = element_blank(),
          strip.text = element_text(size=12),
          legend.key = element_rect(fill = NA))

  if (plot_abline) p <- p + geom_abline(slope = 2, intercept = -2.1, linetype = "dashed") + guides(linetype = 'none')
  if (plot_fits) p <- p + geom_line(data = preddat, aes(x = dbh, y = q50, group = prod_model, linetype = prod_model), size=0.25)

  return(p)

}
