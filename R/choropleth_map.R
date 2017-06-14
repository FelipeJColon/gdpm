#' Makes a spatio-temporal choropleth map of a disease
#'
#' @param df a data frame containing the epidemiological data (e.g. \code{ili})
#'  and at least the colums "year", "month", "province" and "incidence" and/or
#'  "mortality"
#' @param ye an year to indicate the temporal information
#' @param mo month to precise the temporal information
#' @param x a value for the x coordinate of the top-left point of the legend
#' @param y a value for the y coordinate of the top-left point of the legend
#' @param sel_value a character value which can be "incidence" (by default) or
#' "mortality" used to select the data expressed in the map
#' @param n a numeric indicating the number of intervals to represent the data
#' (by default, \code{n = 6})
#' @param col a vector of colors to use for the map (by default,
#' \code{col = heat.colors(6)}.The colors from the package RColorBrewer can also
#'  be used.
#' @param style a character value issued from the \code{classint} package, and
#' used to select a method for the different way of calculating the intervals
#' (by default \code{style = "quantile"})
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = "grey"})
#' @param fixedBreaks issued from the \code{classint} package. By default
#' \code{NULL} but if a vector value is inputed, it will be used to specifen the
#'  breaks
#' @param locate if TRUE, call the function \code{locator} to indicate the
#' top-left point of the legend
#' @param pos by default \code{top-left}, but can be \code{top-right,
#' bottom-left or bottom-right} can be used to indicate the position of the data
#' if \code{x, y} are not indicated
#' @param distrib if TRUE, print on the map, the distribution of the values by
#' intervals
#' @param n_round integer indicating the number of significant digits to be used
#' in the legend, by default (\code{0})
#' @param h legend parameter expressing the height of one rectangle
#' in the legend
#' @param w legend parameter expressing the width of the legend
#' @param tl legend parameter expressing the length of the tick
#' @param s legend parameter expressing the space between the text and the
#' tick
#' @param ... if need to imput more text parameters for the legend
#'
#' @examples
#' library(gdpm)
#' ili <- getid_("ili", from = 2004, to = 2004)
#' # A choroplet map of the ILI data:
#' gdpm_choropleth(ili, 2004, "April")
#'
#' # with some data transformations in order to reflect better the contrasts:
#' gdpm_choropleth(ili, 2004, "April", x = 93, y = 18, n = 6,
#' col = heat.colors(6), style = "jenks")
#'
#' # using some other color palettes, for examples the ones from the
#' # RColorBrewer package or the one in the color Palettes of R :
#' library(RColorBrewer)
#' # to see the available color palettes:
#' display.brewer.all()
#' gdpm_choropleth(ili, 2004, "April", x = 93, y = 18, n = 6,
#' col = heat.colors(6), style = "jenks")
#' gdpm_choropleth(ili, 2004, "April", x = 93, y = 18, n = 6,
#'  col = "YlOrBr", style = "jenks")
#'
#' # changing the color of the missing values:
#' ili80 <- getid(ili, from = 1980, to = 1980)
#' gdpm_choropleth(ili80, 1980, "April", col_na = "chartreuse")
#'
#' # changing the legend text parameters
#' gdpm_choropleth(ili80, 1980, "April", n = 6, col = heat.colors(6),
#' style = "jenks", cex = 0.5)
#' # Print the numeric legend with 2 decimals
#' gdpm_choropleth(ili80, 1980, "April", n = 6, n_round = 2)
#'
# # Doesn't print the distribution of the value by intervals
#' gdpm_choropleth(ili80, 1980, "April", n = 6, distrib = FALSE,
#'   col = heat.colors(6), style = "jenks")
#'
#' # By default, the legend is on the top left of the figure, but the position
#' # can be easily change by using the parameters pos
#'  # top left
#' gdpm_choropleth(ili, 2004, "April", n = 6, col = heat.colors(6),
#'   style = "jenks")
#' gdpm_choropleth(ili, 2004, "April", n = 6, pos = "top-left",
#'   col = heat.colors(6), style = "jenks")
#' # top right
#' gdpm_choropleth(ili, 2004, "April", n = 6, pos = "top-right",
#'   col = heat.colors(6), style = "jenks", distrib = FALSE)
#' # bottom left
#' gdpm_choropleth(ili, 2004, "April", n = 6, pos = "bottom-left",
#'   col = heat.colors(6), style = "jenks")
#' # bottom right
#' gdpm_choropleth(ili, 2004, "April", n = 6, pos = "bottom-right",
#'   col = heat.colors(6), style = "jenks", distrib = FALSE)
#'
#' # By default, the function print the map of the incidence value for one
#' # month of one year, but the mortality can also be printed.
#' gdpm_choropleth(ili, 2004, "April", n = 6, sel = "incidence",
#'   col = heat.colors(6), style = "jenks")
#' gdpm_choropleth(ili, 2004, "January", n = 3, sel = "mortality",
#'   col = heat.colors(3), style = "jenks")
#'
#' # The intervals used to expressed the value can be input directly as
#' # parameters in the function via the parameters fixedBreaks.
#' # The given breaks should be of length n+1.
#' gdpm_choropleth(ili, 2004, "April", n = 3, col = heat.colors(3),
#'   fixedBreaks = c(0, 5000, 10000, 15000))
#'
#' # Using the locator to choose where to print the legend
#' \dontrun{
#'  gdpm_choropleth(ili80, 1980, "April", n = 6, locate = TRUE,
#'   col = heat.colors(6), style = "jenks")
#' gdpm_choropleth(ili80, 1980, "April", n = 6, locate = TRUE, x = 95, y = 34,
#'   col = heat.colors(6), style = "jenks")
#' }
#'
#' @export
gdpm_choropleth <- function(#disease,
                            df, ye, mo, x, y, sel_value = "incidence", n = 6,
  col = heat.colors(6), style = "quantile", col_na = "grey", fixedBreaks = NULL,
  locate = FALSE, pos = "top-left", distrib = TRUE, n_round = 0,
  h = 0.75, w = 0.75, tl = .2, s = .2, ...)
  {
  if (sel_value == "incidence"){off <- "mortality"}
  if (sel_value == "mortality"){off <- "incidence"}

  # prepare the table in the right format
  sel <- grep(sel_value, names(df))
  names(df)[sel] <- "value"
  df <- dplyr::filter(df, year == ye, month == mo) %>%
    dplyr::select(-year, -contains(off), -month)

  # draw the choropleth map
  idcm(df, ye, x, y, locate = locate, pos = pos, fixedBreaks = fixedBreaks,
    n = n, col = col, style = style, col_na = col_na, distrib = distrib,
    n_round = n_round, h = h, w = w, tl = tl, s = s, ...)
}

# GENERIC ----------------------------------------------------------------------

#' Draws a spatio-temporal choropleth map of Vietnam
#'
#' @param df a data frame containing two columns : one containing the province
#' name and another containing the value to represent
#' @param ye an year in a numeric format (to select the map corresponding to
#' the data)
#' @param x a value for the x coordinate of the top-left part of the legend
#' @param y a value for the y coordinate of the top-left part of the legend
#' @param n a numeric indicating the number of intervals to represent the data
#' (by default, \code{n = 6})
#' @param col a vector of colors to use for the map (by default,
#' \code{col = heat.colors(6)}).The colors from the package RColorBrewer can
#' also be used.
#' @param style a character value issued from the \code{classint} package, and
#' used to select a method for the different way of calculating the intervals
#' (by default \code{style = "quantile"})
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = "grey"})
#' @param fixedBreaks issued from the \code{classint} package. By default
#' \code{NULL} but if a vector value is inputed, it will be used to specifen the
#'  breaks
#' @param locate if TRUE, call the function \code{locator} to indicate the
#' top-left point of the legend
#' @param pos by default \code{top-left}, but can be \code{top-right,
#' bottom-left or bottom-right} can be used to indicate the position of the data
#' if \code{x, y} are not indicated
#' @param distrib if TRUE, print on the map, the distribution of the values by
#' intervals
#' @param n_round integer indicating the number of significant digits to be used
#' @param h legend parameter expressing the height of one rectangle
#' in the legend
#' @param w legend parameter expressing the width of the legend
#' @param tl legend parameter expressing the length of the tick
#' @param s legend parameter expressing the space between the text and the
#' tick
#' @param ... if need to imput more text parameters for the legend
#'
#' @keywords internal
#' @noRd
idcm <- function(df, ye, x, y,
  n = 6, col = heat.colors(6), style = "quantile",  col_na = "grey",
  fixedBreaks = NULL, locate = FALSE, pos = "top-left", distrib = TRUE,
  n_round = 0, h = 0.75, w = 0.75, tl = .2, s = .2, ...) {

  # graph parameters
  ofig <- par("fig")
  omar <- par("mar")
  par <- par(fig = ofig, mar = omar)
  on.exit(par(fig = ofig, mar = omar))

  # implement the incidence data in the shape file data
  provinces <- gadmVN::gadm(date = ye)
  provinces <- sp::merge(provinces, df)

  # choose class interval and color
  if (length(grep("#", col[1])) >= 1) {
    pal = col %>% rev
  } else {
    pal = RColorBrewer::brewer.pal(n, col)
  }
   if(length(fixedBreaks) != 0){
     classint <- suppressWarnings(classIntervals(provinces$value, n = n,
       style = "fixed", fixedBreaks = fixedBreaks, na.rm = TRUE))
   } else {
     classint <- suppressWarnings(classIntervals(provinces$value, n = n,
       style = style, na.rm = TRUE))
   }

  # plot the result
  classint_colors <- findColours(classint, pal) %>%
    replace(is.na(.), col_na)
  #return(provinces)
  plot(provinces, col = classint_colors)

  # legend
  legend2(x, y, legend = classint$brks, col = pal, locate = locate,
    pos = pos, n_round = n_round, col_na = col_na, h = h, w = w, tl = tl,
    s = s, ...)

  # if ask, plot the quantile distribution (bottom right)
  if (distrib == TRUE){
    plotdim <- par("plt")
    xleft <- plotdim[2] - (plotdim[2] - plotdim[1]) * 0.2
    xright <- plotdim[2]
    ybottom <- plotdim[3]
    ytop <- plotdim[4] - (plotdim[4] - plotdim[3]) * 0.3
    par(fig = c(xleft, xright, ybottom, ytop), new = TRUE, mar = c(0,0,0,0))

    if(length(fixedBreaks) != 0){
      quantile <- suppressWarnings(classIntervals(provinces$value, n = n,
        style = "fixed", fixedBreaks = fixedBreaks, na.rm = TRUE))
    } else {
      quantile <- suppressWarnings(classIntervals(provinces$value, n = n ,
        style = style, na.rm = TRUE))
    }
    plot(quantile, pal = pal, main = "")
  }

}


#' Draws a legend
#'
#' @param x a value for the x coordinate of the top-left part of the legend
#' @param y a value for the y coordinate of the top-left part of the legend
#' @param legend a character vector
#' @param col a vector of colors
#' @param n_round integer indicating the number of significant digits to be used
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = NULL})
#' @param h legend parameter expressing the height of one rectangle
#' in the legend
#' @param w legend parameter expressing the width of the legend
#' @param tl legend parameter expressing the length of the tick
#' @param s legend parameter expressing the space between the text and the
#' tick
#' @param ... if need to imput more text parameters for the legend
#'
#' @keywords internal
#' @noRd
square_legend <- function(x, y, legend, col, n_round = 0, col_na = NULL,
  h = 0.75, w = 0.75, tl = .2, s = .2, ...) {

  # size of the top character (width height)
  size_legend <- legend %>% as.character %>% tail(1) %>%
    strwidth()

  # define point of the legend
  xleft <- x + size_legend + tl + s
  xright <- xleft + w


  if(length(col_na) != 0) {
    # define the y for rectangle legend
    col %<>% rev
    y1 <- y - (0:length(col)) * h
    # built the legend rectangles
    for(i in seq_along(col))
      rect(xleft, y1[i + 1], xright, y1[i], col = col[i], border = NA)
    rect(xleft, tail(y1, 1), xright, y1[1])
    rect(xleft, tail(y1, 1) - h , xright, tail(y1, 1) - h - h, col = col_na)
    segments(xleft, y1, xleft - tl, y1)
    # define the y for the text
    ntcol <- length(col) + 2
    y2 <- y - (0: ntcol) * h
    y2[length(y2)] <- y2[length(y2)] + 0.5 * h
    if (is.numeric(legend) == TRUE){
      text(x + size_legend, y2, c(format(round(rev(legend),n_round),
                                      nsmall = n_round), "", "NA"), adj = 1, ...)
    } else {
      text(x + size_legend, y2, c(rev(legend), "", "NA"), adj = 1, ...)
    }
  } else {
    y <- y - (0:length(col)) * h
    col %<>% rev
    for(i in seq_along(col))
      rect(xleft, y[i + 1], xright, y[i], col = col[i], border = NA)
    rect(xleft, tail(y, 1), xright, y[1])
    segments(xleft, y, xleft - tl, y)
    if (is.numeric(legend) == TRUE){
      text(x + size_legend, y, format(round(rev(legend),n_round),
                                      nsmall = n_round), adj = 1, ...)
    } else {
      text(x + size_legend, y, rev(legend), adj = 1, ...)
    }
  }
}

#' Draws a legend
#'
#' @param x a value for the x coordinate of the top-left part of the legend
#' @param y a value for the y coordinate of the top-left part of the legend
#' @param legend a character vector
#' @param col a vector of colors
#' @param locate if TRUE, call the function \code{locator} to indicate the
#' top-left point of the legend
#' @param pos by default \code{top-left}, but can be \code{top-right,
#' bottom-left or bottom-right} can be used to indicate the position of the data
#' if \code{x, y} are not indicated
#' @param n_round integer indicating the number of significant digits to be used
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = NULL})
#' @param h legend parameter expressing the height of one rectangle
#' in the legend
#' @param w legend parameter expressing the width of the legend
#' @param tl legend parameter expressing the length of the tick
#' @param s legend parameter expressing the space between the text and the
#' tick
#' @param ... if need to imput more text parameters for the legend
#'
#' @keywords internal
#' @noRd
legend2 <- function(x, y, legend, col, locate = FALSE, pos = "top-left",
  n_round = 0, col_na = NULL, h = 0.75, w = 0.75, tl = .2, s = .2, ...){

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

  if (locate == TRUE){
    coordinates <- locator(1)
    x = coordinates$x
    y = coordinates$y
  }

  square_legend(x, y, legend = legend, col = col , n_round = n_round,
                col_na = col_na, h = h, w = w, tl = tl, s = s, ...)
}


