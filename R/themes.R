# -*- coding: us-ascii-unix -*-

#' A \pkg{ggplot2} theme for maps.
#' @param base_size base font size.
#' @param base_family base font family.
#' @param ... passed to \code{\link{theme_strafica}}.
#' @export theme_map
theme_map = function(base_size=8, base_family="", ...) {
    theme_strafica(base_size, base_family, ...) %+replace%
    theme(axis.line=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_blank(),
          axis.ticks=ggplot2::element_blank(),
          axis.title.x=ggplot2::element_blank(),
          axis.title.y=ggplot2::element_blank(),
          legend.background=ggplot2::element_rect(colour=NA),
          panel.background=ggplot2::element_blank(),
          panel.border=ggplot2::element_blank(),
          panel.grid.major.x=ggplot2::element_blank(),
          panel.grid.major.y=ggplot2::element_blank(),
          panel.grid.minor.x=ggplot2::element_blank(),
          panel.grid.minor.y=ggplot2::element_blank(),
          panel.spacing=grid::unit(0, "lines"),
          strip.background=ggplot2::element_blank(),
          complete=TRUE)

}

#' A simple \pkg{ggplot2} theme.
#' @param base_size base font size.
#' @param base_family base font family.
#' @param ... passed to \code{\link{theme_strafica}}.
#' @export theme_plain
theme_plain = function(base_size=8, base_family="", ...) {
    theme = theme_strafica(base_size=base_size,
                           base_family=base_family,
                           panel.border=FALSE,
                           panel.spacing=0.5,
                           strip.background=FALSE,
                           ...)

    theme$axis.line$lineend = "square"
    return(theme)
}

#' A simple \pkg{ggplot2} theme with the Y-axis hidden.
#' @param base_size base font size.
#' @param base_family base font family.
#' @param ... passed to \code{\link{theme_strafica}}.
#' @export theme_plain_x
theme_plain_x = function(base_size=8, base_family="", ...)
    theme_plain(base_size=base_size,
                base_family=base_family,
                axis.x=TRUE,
                axis.y=FALSE,
                grid.x=FALSE,
                grid.y=TRUE,
                ...)

#' A simple \pkg{ggplot2} theme with the X-axis hidden.
#' @param base_size base font size.
#' @param base_family base font family.
#' @param ... passed to \code{\link{theme_strafica}}.
#' @export theme_plain_y
theme_plain_y = function(base_size=8, base_family="", ...)
    theme_plain(base_size=base_size,
                base_family=base_family,
                axis.x=FALSE,
                axis.y=TRUE,
                grid.x=TRUE,
                grid.y=FALSE,
                ...)

#' A generic \pkg{ggplot2} theme.
#' @param base_size base font size.
#' @param base_family base font family.
#' @param axis.x \code{TRUE} to show the X-axis.
#' @param axis.y \code{TRUE} to show the Y-axis.
#' @param grid.colour colour of grid lines.
#' @param grid.x \code{TRUE} to show major vertical grid lines.
#' @param grid.y \code{TRUE} to show major horizontal grid lines.
#' @param grid.minor.x \code{TRUE} to show minor vertical grid lines.
#' @param grid.minor.y \code{TRUE} to show minor horizontal grid lines.
#' @param legend.title \code{TRUE} to show legend title.
#' @param line.colour colour of axis etc. lines.
#' @param panel.border \code{TRUE} to show panel border.
#' @param panel.spacing amount of space between panels.
#' @param strip.background \code{TRUE} to show a box behind panel titles.
#' @param strip.colour colour of panel title background.
#' @export theme_strafica
theme_strafica = function(base_size=8,
                          base_family="",
                          axis.x=TRUE,
                          axis.y=TRUE,
                          grid.colour="grey83",
                          grid.x=FALSE,
                          grid.y=TRUE,
                          grid.minor.x=FALSE,
                          grid.minor.y=FALSE,
                          legend.title=FALSE,
                          line.colour="grey25",
                          panel.border=TRUE,
                          panel.spacing=0,
                          strip.background=TRUE,
                          strip.colour="grey87") {

    theme_grey(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_line(colour=line.colour, size=0.3),
          axis.line.x=(if (axis.x) NULL else ggplot2::element_blank()),
          axis.line.y=(if (axis.y) NULL else ggplot2::element_blank()),
          axis.text=ggplot2::element_text(),
          axis.ticks=ggplot2::element_line(colour=line.colour, size=0.3),
          axis.ticks.x=(if (axis.x) NULL else ggplot2::element_blank()),
          axis.ticks.y=(if (axis.y) NULL else ggplot2::element_blank()),
          axis.title.x=ggplot2::element_text(margin=margin(5,0,4,0)),
          axis.title.y=ggplot2::element_text(angle=90, margin=margin(0,5,0,4)),
          legend.key.size=grid::unit(1, "lines"),
          legend.key=ggplot2::element_rect(fill=NA, colour="white", size=0.3),
          legend.text=ggplot2::element_text(),
          legend.title=(if (legend.title) ggplot2::element_text(hjust=0) else ggplot2::element_blank()),
          panel.background=ggplot2::element_rect(colour=NA),
          panel.border=(if (panel.border) {
              ggplot2::element_rect(colour=line.colour, fill=NA, size=0.3)
          } else ggplot2::element_blank()),
          panel.grid.major=ggplot2::element_line(colour=grid.colour, size=0.3),
          panel.grid.major.x=(if (grid.x) NULL else ggplot2::element_blank()),
          panel.grid.major.y=(if (grid.y) NULL else ggplot2::element_blank()),
          panel.grid.minor=ggplot2::element_line(colour=grid.colour, size=0.175),
          panel.grid.minor.x=(if (grid.minor.x) NULL else ggplot2::element_blank()),
          panel.grid.minor.y=(if (grid.minor.y) NULL else ggplot2::element_blank()),
          panel.spacing=grid::unit(panel.spacing, "lines"),
          plot.title=ggplot2::element_text(margin=margin(10,0,8,0)),
          strip.background=(if (strip.background) {
              ggplot2::element_rect(fill=strip.colour, colour=line.colour, size=0.3)
          } else ggplot2::element_blank()),
          strip.text=ggplot2::element_text(),
          complete=TRUE)

}
