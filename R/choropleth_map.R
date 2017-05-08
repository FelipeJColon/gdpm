# System
library(dplyr)
library(magrittr)
library(gdpm)

# Function ####################################################################
# Specific GDPM ----------------------------------------------------------------

# select the incidence data for one specific year
gdpm_chloropleth <- function(disease, ye, sel = "incidence", n = 6,
  col = "YlOrBr", style = "quantile",  col_na = "grey", ...){
  name <- paste0(sel,"_",disease)
  df <- getid_(disease, from = ye, to = ye) %>%
    select(-year, -contains("mortality")) %>%
    group_by(province) %>%
    rename_("incidence" = name) %>%
    summarise(value = ifelse(mean(is.na(incidence)) == 1,
      sum(incidence), sum(incidence, na.rm = TRUE))) %>%
    ungroup
  idcm(df, ye, n = n, col = col, style = style, col_na = col_na, ...)
}

# GENERIC ----------------------------------------------------------------------

# draw a chloropleth map
idcm <- function(df, ye,
  n = 6, col = "YlOrBr", style = "quantile",  col_na = "grey", ...) {

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
  wrap_legend(legend = classint$brks, col = pal, ...)
  #usr <- par("usr")
  #legend2(usr[1], usr[4]-0.5, legend = classint$brks, col = pal, adj = 0, ...)

}

legend2 <- function(x, y, legend, col = c("red", "green", "blue"),
  h = 1, w = 1, tl = .2, s = .4, ...) {
  xleft <- x
  xright <- x + w
  y <- y - (0:length(col)) * h
  for(i in seq_along(col))
    rect(xleft, y[i + 1], xright, y[i], col = col[i], border = NA)
  rect(xleft, tail(y, 1), xright, y[1])
  segments(xright, y, xright + tl, y)
  text(xright + tl + s, y, rev(legend), ...)
}


# read the data
gdpm_chloropleth("ili", 1980, n = 6, col = "YlOrBr", style = "jenks",
  col_na = "chartreuse", adj = 0)
gdpm_chloropleth("ili", 2004, n = 6, col = heat.colors(6), style = "jenks")

# Development -----------------------------------------------------------------
locator(1)

wrap_legend <- function(x, y, legend, col, h, w, tl, s, ...){
  if (missing(x) | missing(y)){
    usr <- par("usr")
    xr <- (usr[2] - usr[1]) / 27
    yr <- (usr[4] - usr[3]) / 27
    xlim <- c(usr[1] + xr, usr[2] - xr)
    ylim <- c(usr[3] + yr, usr[4] - yr)
    x <- xlim[1]
    y <- ylim[2]
  }

  if (missing(h) | missing(w) | missing(tl) | missing(s)){
    h = diff(ylim)/25
    w = diff(xlim)/20
    tl = (diff(ylim)/50)
    s = diff(ylim)/50
  }

  legend2(x, y, legend = legend, col = col , h = h, w = w, tl = tl, s = s, ...)
}
# because by default, the coordinate range is
# extended by 4% so : 27 = (100 + 2*4) / 4










