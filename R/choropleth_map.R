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
gdpm_choropleth <- function(disease, ye, x, y, sel = "incidence", n = 6,
  col = "YlOrBr", style = "quantile",  col_na = "grey",
  h = 0.75, w = 0.75, tl = .2, s = .4, adj = 1, ...)
  {
  # prepare the table in the right format
  name <- paste0(sel,"_",disease)
  df <- getid_(disease, from = ye, to = ye) %>%
    select(-year, -contains("mortality")) %>%
    group_by(province) %>%
    rename_("incidence" = name) %>%
    summarise(value = ifelse(mean(is.na(incidence)) == 1,
      sum(incidence), sum(incidence, na.rm = TRUE))) %>%
    ungroup
  # draw the choropleth map
  idcm(df, ye, x, y, n = n, col = col, style = style, col_na = col_na,
    legend = legend,  h = h, w = w, tl = tl, s = s, adj = adj, ...)
}

# GENERIC ----------------------------------------------------------------------

# draw a choropleth map
idcm <- function(df, ye, x, y,
  n = 6, col = "YlOrBr", style = "quantile",  col_na = "grey",
  legend,h = 0.75, w = 0.75, tl = .2, s = .4, adj = 1, ...) {

  # implement the incidence data in the shape file data
  require(gadmVN)
  provinces <- gadm(date = ye)
  provinces <- merge(provinces, df)

  # choose class interval and color
  require(RColorBrewer)
  require(classInt)
  if (length(grep("#", col[1])) >= 1) {
    pal = col
  } else {
    pal = rev(brewer.pal(n, col))
  }
  classint <- suppressWarnings(classIntervals(provinces$value, n = n,
    style = style, na.rm = TRUE))

  # plot the result
  classint_colors <- findColours(classint, pal) %>%
    replace(is.na(.), col_na)
  plot(provinces, col = classint_colors)

  # legend
  wrap_legend(x, y, legend = classint$brks, col = pal, h = h, w = w, tl = tl,
    s = s, adj = adj, ...)
}


# add a legend to a plot
legend2 <- function(x, y, legend, col = c("red", "green", "blue"),
  h = 0.75, w = 0.75, tl = .2, s = .4, adj = 1, ...) {

  # calculate size of one character
  usr <- par("usr")  # figure dimension in coordinates unity
  usrr <- c(diff(usr[1:2]) - 0.08 * diff(usr[1:2]),
    diff(usr[3:4]) - 0.08 * diff(usr[3:4]))
  pin <- par("pin")
  pinr <- pin - 0.08 * pin
  cin <- par("cin")
  cinr <- cin - 0.08 * cin
  crl <- (cinr /  pinr) * usrr

  # size of the top character (width height)
  size_legend <- legend %>% as.character %>% tail(1) %>%
   nchar()
  size_legend <-  size_legend * crl

  # define point of the legend
  xleft <- x + size_legend[1] + tl + s
  xright <- xleft + w
  y <- y - (0:length(col)) * h
  for(i in seq_along(col))
    rect(xleft, y[i + 1], xright, y[i], col = col[i], border = NA)
  rect(xleft, tail(y, 1), xright, y[1])
  segments(xleft, y, xleft - tl, y)
  text(x + size_legend[1], y, rev(legend), adj = adj, ...)

}

# Development -----------------------------------------------------------------

wrap_legend <- function(x, y, legend, col, h = 0.75, w = 0.75, tl = .2, s = .4,
  adj = 1, ...){

  if (missing(x) & missing(y)){
    usr <- par("usr")
    xr <- (usr[2] - usr[1]) * 0.08
    yr <- (usr[4] - usr[3]) * 0.08
    xlim <- c(usr[1] + xr, usr[2] - xr)
    ylim <- c(usr[3] + yr, usr[4] - yr)
    x <- xlim[1]
    y <- ylim[2]
  }

  legend2(x, y, legend = legend, col = col , h = h, w = w, tl = tl, s = s,
    adj = adj, ...)
}

# test -------------------------------------------------------------------------

plot(1:10, 1:10)
points(1, 9, col = "blue")
legend2(1,9, letters[1:3], col = heat.colors(4))
#gdpm_choropleth("ili", 1980, x = 98.85, y = 18, n = 6, col = "YlOrBr",
#   style = "jenks", col_na = "chartreuse", adj = 1)

#points(98.85 + 0.75, 18 , col = "red")
#legend2(98.85,18,c("ab","cd","ef","gh"))


# read the data
gdpm_choropleth("ili", 1980, n = 6, col = "YlOrBr", style = "jenks")
gdpm_choropleth("ili", 2004, n = 6, col = heat.colors(6), style = "jenks")

gdpm_choropleth("ili", 1980, x = 110, y = 22, n = 6, col = "YlOrBr",
  style = "jenks", col_na = "chartreuse", adj = 1)





