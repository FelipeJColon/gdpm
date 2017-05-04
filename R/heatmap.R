#' Makes a spatio-temporal heatmap of a disease
#'
#' @param \code{df} an data frame outputed from the \code{getid} function or
#' with the same structure as an output from this funtion.
#' @param \code{f} a transforming function. By default the identity function.
#' @param \code{col} a vector of colors to use for the heatmap.
#' @param \code{col_na} the color with which to represent the missing values.
#' @param \code{x} a vector of values between 0 and 1. In proportion of the
#' figure's range, these numbers express the location of the right end of the
#' heatmap, and the beginning and end of the color scale that stands on the
#' right of the heatmap.
#' @param \code{show_names} logical value saying whether the names of the
#' provinces should be returned as an output of the function call or not.
#' By default FALSE.
#'
#' @examples
#' # A heatmap of the ILI data:
#' ili <- getid(ili, from = 2004)
#' heatmap(ili)
#' # with some data transformations in order to reflect better the contrasts:
#' heatmap(ili, f = sqrt)
#' heatmap(ili, f = function(x) x^.3)
#' # using some other color palettes, for examples the ones fromt the
#' # RColorBrewer package:
#' library(RColorBrewer)
#' # to see the available color palettes:
#' display.brewer.all()
#' heatmap(ili, f = function(x) x^.3, col = brewer.pal(11, "RdYlGn"))
#' heatmap(ili, f = function(x) x^.3, col = brewer.pal(11, "RdYlBu"))
#' heatmap(ili, f = function(x) x^.3, col = brewer.pal(11, "PRGn"))
#' heatmap(ili, f = function(x) x^.3, col = brewer.pal(9, "YlOrRd"))
#' heatmap(ili, f = function(x) x^.3, col = brewer.pal(9, "YlOrBr"))
#' # changing the color of the missing values:
#' rubella <- getid(rubella)
#' heatmap(rubella, f = sqrt, col = brewer.pal(9, "YlOrRd"))
#' heatmap(rubella, f = sqrt, col = brewer.pal(9, "YlOrRd"), col_na = "blue")
#' # to order the provinces by latitude:
#' library(gadmVN)
#' provinces <- gadm()
#' coord <- coordinates(provinces)
#' order <- rownames(coord[order(coord[, 2]), ])
#' order <- data.frame(province = order, order = seq_along(order))
#' rubella <- left_join(rubella, order)
#' rubella %<>% arrange(order)
#' heatmap(rubella, f = sqrt, col = brewer.pal(9, "YlOrRd"), col_na = "blue")
heatmap <- function(df, f = function(x) x, col = heat.colors(12),
  col_na = "grey", x = c(.85, .87, .92), show_names = FALSE) {
  require(dplyr)  # for "mutate", "arrange"

# Data preparation:
  df %<>%
    mutate(time = as.Date(paste0(year, "-", as.numeric(month), "-", 15))) %>%
    arrange(time)
  time_vec <- unique(df$time)
  provinces_names <- unique(df$province)
  values <- sapply(provinces_names,
                   function(x) filter(df, province == x) %>%
      select(contains("incidence")))
                     #subset(df, province == x, select = incidence))
  values <- f(as.matrix(as.data.frame(values)))

# Options and graphical parameters:
  opar <- par()
  owar <- getOption("warn")
  on.exit({par(opar); options(warn = owar)})
  plt <- par("plt")
  options(warn = -1)

# The heatmap:
  plt[2] <- x[1]
  par(plt = plt)
  image(time_vec, seq_len(ncol(values)), values, ann = FALSE)
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = col_na)
  image(time_vec, seq_len(ncol(values)), values, col = col, add = TRUE)
  box(bty = "o")

# The legend:
  plt[1:2] <- x[2:3]
  par(plt = plt, new = TRUE)
  scale <- seq(min(values, na.rm = TRUE), max(values, na.rm = TRUE), le = 512)
  image(1, scale, matrix(scale, 1), col = col, ann = FALSE, axes = FALSE)
  axis(4)
  box(bty = "o")

# The provinces names:
  if (show_names) return(provinces_names) else invisible(provinces_names)
}


