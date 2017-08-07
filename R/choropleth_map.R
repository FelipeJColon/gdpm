#'
#' @export
select_map <- function(range){
  if (range[1] <= 1992 & range[2] > 2007){
    map <- gadmVN::gadm(date = range[1], merge_hanoi = TRUE)
    map[which(map$province == "Ha Son Binh"),] <- "Ha Noi"
  } else if (range[1] > 1992 & range[2] > 2007){
    map <- gadmVN::gadm(date = range[1], merge_hanoi = TRUE)
    #map[which(map$province == "Ha Tay"),] <- "Ha Noi"
  } else {
    map <- gadmVN::gadm(date = range[1])
  }
  return(map)
}
