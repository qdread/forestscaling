#' Forestlight plotting themes
#'
#' @return Returns a ggplot2 theme that can be added to an existing plot. Note that parentheses must be used.
#'
#' @examples
#' ggplot(data, aes(x, y)) + geom_line() + theme_plant()
#'
#' @export
theme_plant <- function() {
  theme(panel.grid = element_blank(),
                     aspect.ratio = .75,
                     axis.text = element_text(size = 19, color = "black"),
                     axis.ticks.length=unit(0.2,"cm"),
                     axis.title = element_text(size = 19),
                     axis.title.y = element_text(margin = margin(r = 10)),
                     axis.title.x = element_text(margin = margin(t = 10)),
                     axis.title.x.top = element_text(margin = margin(b = 5)),
                     plot.title = element_text(size = 19, face = "plain", hjust = 10),
                     panel.border = element_rect(color = "black", fill=NA,  size=1),
                     panel.background = element_blank(),
                     legend.position = "none",
                     rect = element_rect(fill = "transparent"),
                     text = element_text(family = 'Helvetica'))
}

#' @rdname theme_plant
#' @export
theme_plant_small <- function() {
  theme(panel.grid = element_blank(),
                           aspect.ratio = .75,
                           axis.text = element_text(size = 15, color = "black"),
                           axis.ticks.length=unit(0.2,"cm"),
                           axis.title = element_text(size = 15),
                           axis.title.y = element_text(margin = margin(r = 10)),
                           axis.title.x = element_text(margin = margin(t = 10)),
                           axis.title.x.top = element_text(margin = margin(b = 5)),
                           plot.title = element_text(size = 15, face = "plain", hjust = 10),
                           panel.border = element_rect(color = "black", fill=NA,  size=1),
                           panel.background = element_rect(fill = "transparent",colour = NA),
                           plot.background = element_rect(fill = "transparent",colour = NA),
                           legend.position = "none",
                           rect = element_rect(fill = "transparent"),
                           text = element_text(family = 'Helvetica'))
}

#' @rdname theme_plant
#' @export
theme_facet <- function() {
  theme(strip.background = element_rect(fill=NA),
                     panel.border = element_rect(color = "black", fill=NA,  size=.75),legend.position = 'none',
                     panel.background = element_blank(),
                     strip.text.x = element_blank(),
                     axis.text = element_text(size = 15, color = "black"),
                     axis.ticks.length=unit(0.2,"cm"),
                     axis.title = element_text(size = 15))
}

#' @rdname theme_plant
#' @export
theme_facet2 <- function() {
  theme(strip.background = element_rect(fill=NA),
                      panel.border = element_rect(color = "black", fill=NA,  size=.75),legend.position = 'none',
                      panel.background = element_blank(),
                      strip.text.x = element_blank(),
                      axis.text = element_text(size = 15, color = "black"),
                      axis.ticks.length=unit(0.2,"cm"),
                      axis.title = element_text(size = 15))
}

#' @rdname theme_plant
#' @export
theme_no_y <- function() {
  theme(axis.title.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank())
}

#' @rdname theme_plant
#' @export
theme_no_x <- function() {
  theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank())
}
