#' Formatting the gdpm data to create a heatmap
#'
#' @param df an data frame outputed from the \code{getid} function or
#' with the same structure as an output from this funtion, with at least the
#' variable \code{month}, \code{year}, \code{province} and the variable to
#' select for representing in a heatmap.
#' @param sel the quoted name of the colums whose values will be used for the
#' heatmap, or a part of the name UNIQUE of this column.
#'
#' @return A data frame containing three columns: \code{time} in a Date format,
#'  \code{province} in a character format and the variable to
#' select in a numeric format to represent in a heatmap.
#'
#' @examples
#' # A heatmap of the ILI data:
#' library(gdpm)
#' library(magrittr)
#' library(marc)
#'
#' ili <- getid(ili, from = 2004) %>% hm_data("incidence")
#' a <- sthm(ili)
#' legend2(.925, 1, legend =  a, col = col, postext = "right", n_round = 2,
#'         h = 1/length(col), w = 0.04, tl = 0.01, s = 0.005)
#'
#' @export
hm_data <- function(df,sel){
  df %>%
    mutate(time = as.Date(paste0(year, "-", as.numeric(month), "-", 15))) %>%
    select(matches("province"), matches("time"), contains(sel)) %>%
    arrange(time)
}
