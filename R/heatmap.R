#' Formatting the gdpm data to create a heatmap
#'
#' @param df an data frame outputed from the \code{getid} function or
#' with the same structure as an output from this funtion, with at least the
#' variable \code{month}, \code{year}, \code{province} and the variable to
#' select for representing in a heatmap.
#' @param sel the quoted name of the colums whose values will be used for the
#' heatmap, or a part of the name UNIQUE of this column.
#'
#' @examples
#' # A heatmap of the ILI data:
#' library(gdpm)
#' ili <- getid(ili, from = 2004)
#' ili <- hm_data(ili, "incidence")
#'
#' @export
hm_data <- function(df,sel){
  df %>%
    mutate(time = as.Date(paste0(year, "-", as.numeric(month), "-", 15))) %>%
    select(matches("province"), matches("time"), contains(sel)) %>%
    arrange(time)
}


# GENERIC ----------------------------------------------------------------------

#' Makes a spatio-temporal heatmap of a disease
#'
#' @param df A  data frame. Should contains three variables: \code{province},
#' \code{time}, and one colums of 'numeric' class containing the value to
#' represent.
#' @param f a transforming function. By default the identity function.
#' @param col a vector of colors to use for the heatmap.
#' @param col_na the color with which to represent the missing values.
#' @param x a vector of values between 0 and 1. In proportion of the
#' figure's range, these numbers express the location of the right end of the
#' heatmap, and the beginning and end of the color scale that stands on the
#' right of the heatmap.
#' @param show_legend logical value saying whether the names of the
#' provinces and the value breaks should be returned as an output of the
#' function call or not. By default FALSE.
#'
#' @examples
#' library(gdpm)
#' # A heatmap of the ILI data:
#' ili <- getid(ili, from = 2004)
#' sthm(ili)
#' # with some data transformations in order to reflect better the contrasts:
#' sthm(ili, f = sqrt)
#' sthm(ili, f = function(x) x^.3)
#' # using some other color palettes, for examples the ones fromt the
#' # RColorBrewer package:
#' library(RColorBrewer)
#' # to see the available color palettes:
#' display.brewer.all()
#' sthm(ili, f = function(x) x^.3, col = brewer.pal(11, "RdYlGn"))
#' sthm(ili, f = function(x) x^.3, col = brewer.pal(11, "RdYlBu"))
#' sthm(ili, f = function(x) x^.3, col = brewer.pal(11, "PRGn"))
#' sthm(ili, f = function(x) x^.3, col = brewer.pal(9, "YlOrRd"))
#' sthm(ili, f = function(x) x^.3, col = brewer.pal(9, "YlOrBr"))
#' # changing the color of the missing values:
#' rubella <- getid(rubella)
#' sthm(rubella, f = sqrt, col = brewer.pal(9, "YlOrRd"))
#' sthm(rubella, f = sqrt, col = brewer.pal(9, "YlOrRd"), col_na = "blue")
#' # to order the provinces by latitude:
#' library(gadmVN)
#' library(dplyr)
#' provinces <- gadm()
#' coord <- coordinates(provinces)
#' row.names(coord) <- unique(provinces@data$province)
#' order <- rownames(coord[order(coord[, 2]), ])
#' order <- data.frame(province = order, order = seq_along(order))
#' rubella <- left_join(rubella, order, by = "province")
#' rubella <- arrange(rubella, order)
#' sthm(rubella, f = sqrt, col = brewer.pal(9, "YlOrRd"), col_na = "blue")
#' @export
sthm <- function(df,
  f = function(x) x, col = heat.colors(12),
  col_na = "grey", x = c(.85, .87, .92), show_legend = FALSE)
  {

  warn_old <- unlist(options("warn"))
  options(warn = -1)
  on.exit(options(warn = warn_old))

# Data preparation:
  time_vec <- unique(df$time)
  provinces_names <- unique(df$province)
  values <- sapply(provinces_names,
                   function(x) filter(df, province == x) %>%
      select_if(is.numeric))
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
  labels <- levels(cut(seq(min(values, na.rm = TRUE), max(values, na.rm = TRUE),
                         le = 512), length(col)))
  legend <- c(0, as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labels)))

# Print the legend if needed :
  if (show_legend) return(list(legend = legend, prov = provinces_names)) else
    invisible(legend)
}





