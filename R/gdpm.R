#' gdpm : Surveillance Data from the Vietnamese GDPM
#'
#' Monthly surveillance data aggregated by province by the Vietnamese General
#' Department of Preventive Medicine (GDPM)
#'
#' @docType package
#' @name gdpm
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom dplyr filter
#' @importFrom dplyr contains
#' @importFrom dplyr one_of
#' @importFrom dplyr matches
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr select_
#' @importFrom dplyr rename_
#' @importFrom dplyr bind_rows
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom stats setNames
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tidyr separate
NULL

## quiets concerns of R CMD check for the values that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "incidence", "year"
                                                        , "mortality", "month",
                                                        "name", "province",
                                                        "value"))



