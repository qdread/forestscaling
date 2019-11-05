#' Forestlight plotting themes
#'
#' @return Returns a ggplot2 theme that can be added to an existing plot. Note that parentheses must be used.
#'
#' @examples
#' ggplot(data, aes(x, y)) + geom_line() + theme_plant()
#'
#' @export
theme_plant <- function() {
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 aspect.ratio = .75,
                 axis.text = ggplot2::element_text(size = 19, color = "black"),
                 axis.ticks.length=ggplot2::unit(0.2,"cm"),
                 axis.title = ggplot2::element_text(size = 19),
                 axis.title.y = ggplot2::element_text(margin = margin(r = 10)),
                 axis.title.x = ggplot2::element_text(margin = margin(t = 10)),
                 axis.title.x.top = ggplot2::element_text(margin = margin(b = 5)),
                 plot.title = ggplot2::element_text(size = 19, face = "plain", hjust = 10),
                 panel.border = ggplot2::element_rect(color = "black", fill=NA,  size=1),
                 panel.background = ggplot2::element_blank(),
                 legend.position = "none",
                 rect = ggplot2::element_rect(fill = "transparent"),
                 text = ggplot2::element_text(family = 'Helvetica'))
}

#' @rdname theme_plant
#' @export
theme_plant_small <- function() {
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 aspect.ratio = .75,
                 axis.text = ggplot2::element_text(size = 15, color = "black"),
                 axis.ticks.length=ggplot2::unit(0.2,"cm"),
                 axis.title = ggplot2::element_text(size = 15),
                 axis.title.y = ggplot2::element_text(margin = margin(r = 10)),
                 axis.title.x = ggplot2::element_text(margin = margin(t = 10)),
                 axis.title.x.top = ggplot2::element_text(margin = margin(b = 5)),
                 plot.title = ggplot2::element_text(size = 15, face = "plain", hjust = 10),
                 panel.border = ggplot2::element_rect(color = "black", fill=NA,  size=1),
                 panel.background = ggplot2::element_rect(fill = "transparent",colour = NA),
                 plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
                 legend.position = "none",
                 rect = ggplot2::element_rect(fill = "transparent"),
                 text = ggplot2::element_text(family = 'Helvetica'))
}

#' @rdname theme_plant
#' @export
theme_facet <- function() {
  ggplot2::theme(strip.background = ggplot2::element_rect(fill=NA),
                 panel.border = ggplot2::element_rect(color = "black", fill=NA,  size=.75),
                 legend.position = 'none',
                 panel.background = ggplot2::element_blank(),
                 strip.text.x = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(size = 15, color = "black"),
                 axis.ticks.length=ggplot2::unit(0.2,"cm"),
                 axis.title = ggplot2::element_text(size = 15))
}

#' @rdname theme_plant
#' @export
theme_facet2 <- function() {
  ggplot2::theme(strip.background = ggplot2::element_rect(fill=NA),
                 panel.border = ggplot2::element_rect(color = "black", fill=NA,  size=.75),
                 legend.position = 'none',
                 panel.background = ggplot2::element_blank(),
                 strip.text.x = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(size = 15, color = "black"),
                 axis.ticks.length=ggplot2::unit(0.2,"cm"),
                 axis.title = ggplot2::element_text(size = 15))
}

#' @rdname theme_plant
#' @export
theme_no_y <- function() {
  ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank())
}

#' @rdname theme_plant
#' @export
theme_no_x <- function() {
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank())
}
