#' Makes a spatio-temporal choropleth map of a disease
#'
#' The map selected is automatically the one corresponding to the first
#' year of the dataset inputed.
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
#' bottom-left or bottom-right}, used to indicate the position of the data
#' if \code{x, y} are not indicated
#' @param distrib if TRUE, print on the map, the distribution of the values by
#' intervals
#' @param n_round integer indicating the number of significant digits to be used
#' in the legend, by default (\code{0})
#' @param pos_brks if TRUE, the breaks values will all be positive, the first
#' break will be superior or equal to zero, by default (\code{TRUE}). If false,
#' allows negative value for breaks
#' @param postext define the side of the legend text, by default \code{left}
#' but can be \code{right}
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
#' gdpm_choropleth(ili, 2004, "April", col = heat.colors(3),
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
gdpm_choropleth <- function(df, ye, mo, sel_value = "incidence", n = 6,
  col = heat.colors(6), style = "quantile", col_na = "grey", fixedBreaks = NULL,
  distrib = TRUE, n_round = 0, pos_brks =TRUE)
  {
  if (sel_value == "incidence"){off <- "mortality"}
  if (sel_value == "mortality"){off <- "incidence"}

  # selection of the Vietnam map with the province accordingly to the year range
  # of the dataset
  if (min(df$year) < 1992 & max(df$year) > 2007){
    map <- gadmVN::gadm(date = min(df$year), merge_hanoi = TRUE)
    map[which(map$province == "Ha Son Binh"),] <- "Ha Noi"
  } else if (min(df$year) > 1992 & max(df$year) > 2007){
    map <- gadmVN::gadm(date = min(df$year), merge_hanoi = TRUE)
    #map[which(map$province == "Ha Tay"),] <- "Ha Noi"
  } else {
    map <- gadmVN::gadm(date = min(df$year))
  }

  # prepare the table in the right format
  #sel <- grep(sel_value, names(df))
  #names(df)[sel] <- "value"
  df <- dplyr::filter(df, year == ye, month == mo) %>%
    dplyr::select(-year, -contains(off), -month)

  # draw the choropleth map
  idcm(df = df, sel = "incidence_cholera", map = map,
       fixedBreaks = fixedBreaks, n = n, col = col, style = style,
       col_na = col_na, distrib = distrib, n_round = n_round,
       pos_brks = pos_brks)
}

# GENERIC ----------------------------------------------------------------------

#' Draws a spatio-temporal choropleth map
#'
#' @details The \code{n} parameter can be overuled by the number of breaks
#' obtain by certain style. By default, the number of intervall will be decide
#' by the \code{n} parameter but for certain \code{style} like "pretty", this
#' value can be overruled. Please for more information, look at the
#' documentation of the classint package.
#'
#' @param df a data frame containing at least the variable: \code{province}
#' @param sel the quoted name of the column whose values will be represented.
#' @param map an object of class "SpatialPolygonsDataFrame" containing at least
#' the varible \code{province}
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
#' @param fixedBreaks By default \code{NULL} but if a vector of value is
#' inputed, it will be used to specifen the breaks.
#' @param distrib if TRUE, print on the map, the distribution of the values by
#' intervals. Only available when the represented variable has at least two
#' differents value and if the breaks are not specified via fixedBreaks.
#' @param n_round integer indicating the number of significant digits to be used
#' @param pos_brks if TRUE, the breaks values will all be positive, the first
#' break will be superior or equal to zero, by default (\code{TRUE}). If false,
#' allows negative value for breaks
#' @param show_legend logical value saying whether the names of the
#' provinces and the value breaks should be returned as an output of the
#' function call or not. By default \code{FALSE}.
#'
#' @return A numeric with attributes corresponding of the breaks value and the
#' attributes \code{colors} corresponding to the color associated with the
#' breaks value. It can be returned invisibly or not depending on the parameter
#' \code{show_legend}.
#'
#' @keywords internal
#' @noRd
idcm <- function(df, sel, map,
  n = 6, col = heat.colors(6), style = "quantile",  col_na = "grey",
  pos_brks = TRUE, fixedBreaks = NULL, distrib = TRUE, n_round = 0,
  show_legend = FALSE) {

  # graph parameters
  ofig <- par("fig")
  omar <- par("mar")
  par <- par(mar = c(2,2,2,2))
  on.exit(par(fig = ofig, mar = omar))

  # implement the data in the shape file data
  df %<>% rename_("value" = sel)
  provinces <- sp::merge(map, df)

  # draw a choropleth map when all the data contain one single data and no fixed
  # breaks
  if(length(na.omit(unique(provinces$value))) <= 1 &
     length(fixedBreaks) == 0)
    {
    distrib <- NULL
    choropleth_v1(provinces, col = col, col_na = col_na, n_round = n_round,
                  show_legend = show_legend)

  }
  # draw a choropleth with multiple values and no fixed breaks
  else if (length(na.omit(unique(provinces$value))) > 1 &
           length(fixedBreaks) == 0)
  {
    choropleth_vm(provinces, col = col, col_na = col_na, n = n, style = style,
                  distrib = distrib, pos_brks = pos_brks, n_round = n_round,
                  show_legend = show_legend)
  }
  # draw a choropleth map with a fixed breaks
  else if (length(fixedBreaks) > 0){
    choropleth_fix(provinces, col = col, col_na = col_na,
                   fixedBreaks = fixedBreaks, show_legend = show_legend)
  }

}

#' Draws a spatio-temporal choropleth map with one unique value
#'
#' This function draws a choropleth map when all the provinces or regions have
#' the same value.
#'
#' @param df an object of class "SpatialPolygonsDataFrame" containing also
#' the value to represent
#' @param col a vector of colors to use for the map (by default,
#' \code{col = heat.colors(6)}).The colors from the package RColorBrewer can
#' also be used.
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = "grey"})
#' @param n_round integer indicating the number of significant digits to be used
#' @param show_legend logical value saying whether the names of the
#' provinces and the value breaks should be returned as an output of the
#' function call or not. By default \code{FALSE}.
#'
#' @return A numeric with attributes corresponding of the breaks value and the
#' attributes \code{colors} corresponding to the color associated with the
#' breaks value. It can be returned invisibly or not depending on the parameter
#' \code{show_legend}.
#'
#' @keywords internal
#' @noRd
choropleth_v1 <- function (df, col = heat.colors(1), col_na = "grey",
                           n_round = 0, show_legend = FALSE){

  # define the color and the class intervals
  if (length(grep("#", col[1])) >= 1) {
    pal <-  col
    pal <- pal[1]
  } else {
    pal <- RColorBrewer::brewer.pal(3, col)[1]
  }
  pal2 <-  rep(pal, length(df$value))
  pal2[is.na(df$value)] <- col_na
  classint <- na.omit(unique(df$value))

  # draw a choropleth map
  plot(df, col = pal2)

  # print a legend
  legend <- rep(classint,2) %>% round(n_round)
  attr(legend, "colors") <- pal
  if (show_legend) return(legend) else invisible(legend)
}

#' Draws a spatio-temporal choropleth map with multiple value
#'
#' This function draws a choropleth map with at least two differents value. Use
#' the package \code{classInt} to specify the style and the number of breaks.
#'
#' @param df an object of class "SpatialPolygonsDataFrame" containing also
#' the value to represent
#' @param col a vector of colors to use for the map (by default,
#' \code{col = heat.colors(6)}).The colors from the package RColorBrewer can
#' also be used.
#' @param n a numeric indicating the number of intervals to represent the data
#' (by default, \code{n = 6})
#' @param style a character value issued from the \code{classint} package, and
#' used to select a method for the different way of calculating the intervals
#' (by default \code{style = "quantile"})
#' @param distrib if TRUE, print on the map, the distribution of the values by
#' intervals
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = "grey"})
#' @param pos_brks if TRUE, the breaks values will all be positive, the first
#' break will be superior or equal to zero, by default (\code{TRUE}). If false,
#' allows negative value for breaks
#' @param n_round integer indicating the number of significant digits to be used
#' @param show_legend logical value saying whether the names of the
#' provinces and the value breaks should be returned as an output of the
#' function call or not. By default \code{FALSE}.
#'
#' @return A numeric with attributes corresponding of the breaks value and the
#' attributes \code{colors} corresponding to the color associated with the
#' breaks value. It can be returned invisibly or not depending on the parameter
#' \code{show_legend}.
#'
#' @keywords internal
#' @noRd
choropleth_vm <- function (df, col = heat.colors(6), n = 6,
                           style = "quantile", distrib = TRUE, col_na = "grey",
                           pos_brks = TRUE, n_round = 0,
                           show_legend = FALSE){

  # choose class interval and colors
    classint <- suppressWarnings(classIntervals(df$value, n = n,
                                                style = style))
  if (length(grep("#", col[1])) >= 1) {
    pal <-  col[1:(length(classint$brks) - 1)]
  } else if (length(classint$brks) <= 3 & length(grep("#", col[1])) == 0) {
    pal <-  RColorBrewer::brewer.pal(length(classint$brks), col)
    pal <- pal[1:(length(classint$brks) - 1)]
  } else {
    pal <-  RColorBrewer::brewer.pal(length(classint$brks) - 1, col)
  }

  # replace the value of brks below zero by zero
  if (pos_brks == TRUE){
    classint$brks[classint$brks < 0] <- 0
  }

  # plot the result
  classint_colors <- findColours(classint, pal) %>%
    replace(is.na(.), col_na)

  # if ask, plot the quantile distribution (bottom right)
  if (distrib == TRUE){

    # graph parameter
    omar <- par("mar")
    m <- layout(matrix(c(2, 2, 2, 1), 2, 2, byrow = TRUE), heights=c(1, 2),
           widths = c(3, 1))
    # plots
    plot(classint, pal = pal, main = "")
    par(mar = c(2,2,2,2), bg = "transparent")
    plot(df, col = classint_colors)
    par(mar = omar, bg = "white")

  } else {
    # graph parameters
    ofig <- par("fig")
    ousr <- par("usr")
    par(fig = ofig, usr = ousr)
    # plot
    plot(df, col = classint_colors)
  }

  # Print the legend if needed :
  legend <- classint$brks %>% round(n_round)
  attr(legend, "colors") <- pal
  if (show_legend) return(legend) else invisible(legend)

}

#' Draws a spatio-temporal choropleth map with fixed breaks
#'
#' @param df a data frame containing two columns : one containing the province
#' name and another containing the value to represent
#' @param col a vector of colors to use for the map (by default,
#' \code{col = heat.colors(6)}).The colors from the package RColorBrewer can
#' also be used.
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = "grey"})
#' @param fixedBreaks issued from the \code{classint} package. By default
#' \code{NULL} but if a vector value is inputed, it will be used to specifen the
#'  breaks
#' @param show_legend logical value saying whether the names of the
#' provinces and the value breaks should be returned as an output of the
#' function call or not. By default \code{FALSE}.
#'
#' @return A numeric with attributes corresponding of the breaks value and the
#' attributes \code{colors} corresponding to the color associated with the
#' breaks value. It can be returned invisibly or not depending on the parameter
#' \code{show_legend}.
#'
#' @keywords internal
#' @noRd
choropleth_fix <- function (df, col = heat.colors(6), col_na = "grey",
                            fixedBreaks = NULL, show_legend = FALSE){

  # choose class interval and colors
  if (length(grep("#", col[1])) >= 1) {
    pal <-  col[1:(length(fixedBreaks) - 1)]
  } else if (length(fixedBreaks) <= 3 & length(grep("#", col[1])) == 0) {
    pal <-  RColorBrewer::brewer.pal(length(fixedBreaks) -1, col)
    pal <- pal[1:(length(fixedBreaks) - 1)]
  } else {
    pal = RColorBrewer::brewer.pal(length(fixedBreaks) - 1, col)
  }
  pal2 <- colorRampPalette(pal)
  df$col <- pal2(length(fixedBreaks) - 1)[cut(df$value, breaks = fixedBreaks,
                                              include.lowest = TRUE)]
  df$col <- replace(df$col, is.na(df$col), col_na)

  #return(provinces)
  plot(df, col = df$col)

  # print the breaks for a legend
  legend <- fixedBreaks
  attr(legend, "colors") <- pal
  if (show_legend) return(legend) else invisible(legend)
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
#' @param postext define the side of the legend text, by default \code{left}
#' but can be \code{right}
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
                          postext = "left",
                          h = 0.75, w = 0.75, tl = .2, s = .2, ...) {

  # size of the top character (width height)
  size_legend <- legend %>% as.character %>% tail(1) %>%
    strwidth()

  # define point of the legend
  if (postext == "left") {
    xleft <- x + size_legend + tl + s
  }
  if (postext == "right") {
    xleft <- x
  }
  xright <- xleft + w

   # define the y for rectangle legend
  col %<>% rev
  y1 <- y - (0:length(col)) * h

  # built the legend rectangles
  for(i in seq_along(col))
    rect(xleft, y1[i + 1], xright, y1[i], col = col[i], border = NA)
  rect(xleft, tail(y1, 1), xright, y1[1])

  # If want NA, add a rectangle of the color NA
  if(length(col_na) != 0) {
    rect(xleft, tail(y1, 1) - h , xright, tail(y1, 1) - h - h, col = col_na)
  }

  # Define if segments should be on the left or the right
  if (postext == "left") {
    segments(xleft, y1, xleft - tl, y1)
  }
  if (postext == "right"){
    segments(xright, y1, xright + tl, y1)
  }

  # legend text
  if (length(col_na) > 0){
    # define the y  with NA for the text
    ntcol <- length(col) + 2
    y2 <- y - (0: ntcol) * h
    y2[length(y2)] <- y2[length(y2)] + 0.5 * h

    # legend with NA on the left or right side
    if (postext == "left") {
      text(x + size_legend, y2,
           c(format(round(rev(legend),n_round), nsmall = n_round), "", "NA"),
           adj = 1, ...)
    }
    if (postext == "right") {
      text(xright + tl + s , y2,
           c(format(round(rev(legend),n_round), nsmall = n_round), "", "NA"),
           adj = 0, ...)
    }

  } else {
     y2 <- y - (0: length(col)) * h
    if (postext == "left") {
       text(x + size_legend, y2,
           format(round(rev(legend),n_round), nsmall = n_round),
           adj = 1, ...)
      }
    if (postext == "right") {
      text(xright + tl + s , y2,
           format(round(rev(legend),n_round), nsmall = n_round),
           adj = 0, ...)
    }
  }
}

#' Draws a legend
#'
#' Draws a scale legend
#'
#' @param x a value for the x coordinate of the top-left part of the legend
#' @param y a value for the y coordinate of the top-left part of the legend
#' @param legend a character or expression vector to appear in the legend.
#' @param col a vector of colors appearing in the legend
#' @param locate if TRUE, call the function \code{locator} to indicate the
#' top-left point of the legend
#' @param pos by default \code{top-left}, but can be \code{"top-right"},
#' \code{"bottom-left"} or \code{"bottom-right"} can be used to indicate the
#' position of the data if \code{x, y} are not indicated
#' @param n_round integer indicating the number of significant digits to be used
#' , by default \code{0}.
#' @param col_na the color with which to represent the missing values
#' (by default \code{col_na = NULL}). If specified, a NA value will be add to
#' the lefend with the color corresponding.
#' @param postext define the side of the legend text, by default \code{left}
#' but can be \code{right}
#' @param h legend parameter expressing the height of one rectangle
#' in the legend
#' @param w legend parameter expressing the width of the legend
#' @param tl legend parameter expressing the length of the tick
#' @param s legend parameter expressing the space between the text and the
#' tick
#' @param ... if need to imput more text parameters for the legend
#'
#' @details The number of rectangle in the legend is calculate with the number
#' of object in the vector \code{legend} - 1.\cr
#' \cr If arguments \code{x,y}, the location may also be specified by
#' setting the parameter \code{pos} to a keyword form the list: \code{top-left},
#'  \code{"top-right"}, \code{"bottom-left"} or \code{"bottom-right"}. This
#'  places the legend on the inside of the plot frame at the giver location.
#' Note that a call to the function \code{locator(1)} can be used via setting
#' the parameter \code{locate} to TRUE in place of the \code{x} and \code{y}
#' arguments.
#'
#' @keywords internal
#' @noRd
legend2 <- function(x, y, legend, col, locate = FALSE, pos = "top-left",
                    n_round = 0, col_na = NULL, postext = "left", h = 0.75,
                    w = 0.75, tl = .2, s = .2, ...){

  omar <- par("mar")
  par(mar = c(2,2,2,1))
  on.exit(par(mar = omar))

  # size of the top character (width height)
  size_legend <- legend %>% as.character %>% tail(1) %>%
    strwidth()

  if (missing(x) & missing(y) & locate == FALSE){
    usr <- par("usr")
    xr <- (usr[2] - usr[1])/27
    yr <- (usr[4] - usr[3])/27
    xlim <- c(usr[1] + xr, usr[2] - xr)
    ylim <- c(usr[3] + yr, usr[4] - yr)

    if (pos == "top-left"){
      x <- xlim[1]
      y <- ylim[2]
    }
    if (pos == "top-right"){
      x <- xlim[2] - w - size_legend - tl -s
      y <- ylim[2]
    }
    if (pos == "bottom-left"){
      x <- xlim[1]
      y <- ylim[1] + ((length(legend)-1)* h + 2 * h)
    }
    if (pos == "bottom-right"){
      x <- xlim[2] - w - size_legend -tl - s
      y <- ylim[1] + ((length(legend)-1)* h + 2 * h)
    }

  }

  if (locate == TRUE){
    coordinates <- locator(1)
    x = coordinates$x
    y = coordinates$y
  }

  square_legend(x, y, legend = legend, col = col, n_round = n_round,
                col_na = col_na, postext = postext, h = h, w = w, tl = tl,
                s = s, ...)
}


