# System
library(dplyr)
library(magrittr)
library(gdpm)

# Function ####################################################################
# Specific GDPM ----------------------------------------------------------------
#' Makes a spatio-temporal choropleth map of a disease
#'
#' @param disease name of an epidemiological data frame (e.g. \code{ili}
#' @param ye an year to indicate the temporal information
#' @param month month to precise the temporal information
#' @param x a value for the x coordinate of the top-left part of the legend
#' @param y a value for the y coordinate of the top-left part of the legend
#' @param sel a character value which can be "incidence" (by default) or
#' "mortality" used to select the data expressed in the map
#' @param col a vector of colors to use for the heatmap.
#' @param style a character value isued from the \code{classint} package, and
#' used to select a method for the different way of calculating the intervals
#' @param col_na the color with which to represent the missing values
#' @param h legend parameter expressing the height of one rectangle
#' @param w legend parameter expressing the width of the rectangle
#' @param tl legend parameter expressing the length of the tick
#' @param s legend parameter expressing the space between the text and the
#' tick
#' @param adj legend parameter expressing the alignment of the text, 1 (to the
#' right) by default
#' @param ... if need to imput more text parameter
#'
#'
#' @examples
#' # A heatmap of the ILI data:
#' gdpm_choropleth("ili", 1980, x = 93, y = 18)
#' # with some data transformations in order to reflect better the contrasts:
#' gdpm_choropleth("ili", 1980, x = 93, y = 18, n = 6, col = heat.colors(6),
#'      style = "jenks")
#' # using some other color palettes, for examples the ones fromt the
#' # RColorBrewer package:
#' library(RColorBrewer)
#' # to see the available color palettes:
#' display.brewer.all()
#' gdpm_choropleth("ili", 1980, x = 93, y = 18, n = 6, col = "YlOrBr",
#'      style = "jenks")
#' # changing the color of the missing values:
#' gdpm_choropleth("ili", 1980, x = 93, y = 18, n = 6, col = "YlOrBr",
#'      style = "jenks", col_na = "chartreuse")
#' @export
gdpm_choropleth <- function(disease, ye, month, x, y, sel = "incidence", n = 6,
  col = "YlOrBr", style = "quantile",  col_na = "grey",
  locate = FALSE, pos = "top-left", distrib = TRUE,
  h = 0.75, w = 0.75, tl = .2, s = .2, adj = 1, ...)
  {
  if (sel == "incidence"){off <- "mortality"}
  if (sel == "mortality"){off <- "incidence"}

  # prepare the table in the right format
  name <- paste0(sel,"_",disease)
  df <- getid_(disease, from = ye, to = ye) %>%
    select(-year, -contains(off)) %>%
    rename_("value" = name)
  df <- df[which(df$month == month),] %>%
    select(-month)
    #summarise(value = ifelse(mean(is.na(incidence)) == 1,
     # sum(incidence), sum(incidence, na.rm = TRUE))) %>%
    #ungroup
  # draw the choropleth map
  idcm(df, ye, x, y, locate = locate, pos = pos,
    n = n, col = col, style = style, col_na = col_na, distrib = distrib,
    legend = legend, h = h, w = w, tl = tl, s = s, adj = adj, ...)
}

# GENERIC ----------------------------------------------------------------------

# draw a choropleth map
idcm <- function(df, ye, x, y,
  distrib = FALSE,
  n = 6, col = "YlOrBr", style = "quantile",  col_na = "grey",
  legend, pos = "top-left",
  locate= FALSE, h = 0.75, w = 0.75, tl = .2, s = .2, adj = 1, ...) {

  # graph parameters
  par <- par(fig = c(0,1,0,1), mar = c(5.1, 4.1, 4.1, 2.1))

  # implement the incidence data in the shape file data
  require(gadmVN)
  provinces <- gadm(date = ye)
  provinces <- merge(provinces, df)

  # choose class interval and color
  require(RColorBrewer)
  require(classInt)
  if (length(grep("#", col[1])) >= 1) {
    pal = col %>% rev
  } else {
    pal = brewer.pal(n, col)
  }
  classint <- suppressWarnings(classIntervals(provinces$value, n = n,
    style = style, na.rm = TRUE))

  # plot the result
  classint_colors <- findColours(classint, pal) %>%
    replace(is.na(.), col_na)
  plot(provinces, col = classint_colors)

  # legend
  wrap_legend(x, y, legend = classint$brks, col = pal, locate = locate,
    pos = pos, h = h, w = w, tl = tl,
    s = s, adj = adj, ...)

  # if ask, plot the quantile distribution (bottom right)
  if (distrib == TRUE){
    plotdim <- par("plt")
    xleft <- plotdim[2] - (plotdim[2] - plotdim[1]) * 0.2
    xright <- plotdim[2]
    ybottom <- plotdim[3]
    ytop <- plotdim[4] - (plotdim[4] - plotdim[3]) * 0.3
    par(fig = c(xleft, xright, ybottom, ytop), new = TRUE, mar = c(0,0,0,0))

    quantile <- suppressWarnings(classIntervals(provinces$value, n = n ,
      style = style, na.rm = TRUE))
    plot(quantile, pal = pal, main = "")
  }

}


# add a legend to a plot
legend2 <- function(x, y, legend, col = c("red", "green", "blue"),
  h = 0.75, w = 0.75, tl = .2, s = .2, adj = 1, ...) {

  # size of the top character (width height)
  size_legend <- legend %>% as.character %>% tail(1) %>%
    strwidth()

  # define point of the legend
  xleft <- x + size_legend + tl + s
  xright <- xleft + w
  y <- y - (0:length(col)) * h
  col %<>% rev
  for(i in seq_along(col))
    rect(xleft, y[i + 1], xright, y[i], col = col[i], border = NA)
  rect(xleft, tail(y, 1), xright, y[1])
  segments(xleft, y, xleft - tl, y)
  text(x + size_legend, y, rev(legend), adj = adj, ...)

}

# Development -----------------------------------------------------------------

wrap_legend <- function(x, y, legend, col, pos = "top-left", locate = FALSE,
  h = 0.75, w = 0.75, tl = .2, s = .2, adj = 1, ...){

  if (missing(x) & missing(y) & locate == FALSE){
    usr <- par("usr")
    xr <- (usr[2] - usr[1])/27
    yr <- (usr[4] - usr[3])/27
    xlim <- c(usr[1] + xr, usr[2] - xr)
    ylim <- c(usr[3] + yr, usr[4] - yr)

    size_legend <- legend %>% as.character %>% tail(1) %>%
      strwidth()

    if (pos == "top-left"){
      x <- xlim[1] + size_legend
      y <- ylim[2]
    }
    if (pos == "top-right"){
      x <- xlim[2] - w - size_legend - tl -s
      y <- ylim[2]
    }
    if (pos == "bottom-left"){
      x <- xlim[1] + size_legend
      y <- ylim[1] + ((length(legend)-1)* h )
    }
    if (pos == "bottom-right"){
      x <- xlim[2] - w - size_legend -tl - s
      y <- ylim[1] + ((length(legend)-1)* h )
    }

  }

  if (missing(x) & missing(y) & locate == TRUE){
    coordinates <- locator(1)
    x = coordinates$x
    y = coordinates$y
  }

  legend2(x, y, legend = legend, col = col , h = h, w = w, tl = tl, s = s,
    adj = adj, ...)
}

# test -------------------------------------------------------------------------

plot(1:10, 1:10)
points(1, 9, col = "blue")
points(2, 9, col ="pink" )
#legend2(1,9, letters[1:3], col = heat.colors(2))
legend2(1,9, rep("patate", 3), col = heat.colors(2))
wrap_legend(legend = c("zero", "yellow", "red"), col = heat.colors(2))
wrap_legend(locate = TRUE, legend = letters[1:3], col = heat.colors(2))
wrap_legend(legend = letters[1:3], col = heat.colors(2))
wrap_legend(legend = letters[1:3], col = heat.colors(2), pos = "top-right")

# read the data
gdpm_choropleth("ili", 1980, "January")
gdpm_choropleth("ili", 1980, "April", n = 6, col = "YlOrBr", style = "jenks")

# ... correspond to the legend text parameters
gdpm_choropleth("ili", 1980, "April", n = 6, col = "YlOrBr", style = "jenks",
  cex = 0.5)

# test colors
gdpm_choropleth("ili", 1980, "April", n = 6, col = heat.colors(6),
  style = "jenks")

# test styles
gdpm_choropleth("ili", 1980, "April", n = 6, col = heat.colors(6),
  style = "quantile")

# test locator
gdpm_choropleth("ili", 1980, "April", n = 6, locate = TRUE,
  col = heat.colors(6), style = "jenks")

# test distribution FALSE
gdpm_choropleth("ili", 1980, "April", n = 6, distrib = FALSE,
  col = heat.colors(6), style = "jenks")

# test position
# top left
gdpm_choropleth("ili", 2004, "April", n = 6, col = heat.colors(6),
  style = "jenks")
gdpm_choropleth("ili", 2004, "April", n = 6, pos = "top-left",
  col = heat.colors(6), style = "jenks")
# top right
gdpm_choropleth("ili", 2004, "April", n = 6, pos = "top-right",
  col = heat.colors(6), style = "jenks", distrib = FALSE)
# bottom left
gdpm_choropleth("ili", 2004, "April", n = 6, pos = "bottom-left",
  col = heat.colors(6), style = "jenks")
# bottom right
gdpm_choropleth("ili", 2004, "April", n = 6, pos = "bottom-right",
  col = heat.colors(6), style = "jenks", distrib = FALSE)

# test when x & y are inputed
gdpm_choropleth("ili", 1980, "April", x = 110, y = 22, n = 6, col = "YlOrBr",
  style = "jenks", col_na = "chartreuse", adj = 1)
points(110,22, col = "red")

# test color NA
gdpm_choropleth("ili", 1980,  "April", n = 6, col = "YlOrBr",
     style = "jenks", col_na = "chartreuse", adj = 1)

# test sel parameters
gdpm_choropleth("ili", 2004, "April", n = 6, sel = "incidence",
  col = heat.colors(6), style = "jenks")
gdpm_choropleth("ili", 2004, "January", n = 3, sel = "mortality",
  col = heat.colors(3), style = "jenks")



