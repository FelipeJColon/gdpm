#' Summary Data From Monthly Surveillance Data
#'
#' A dataset containing the description of all the data frames contained in the
#' package gdpm containing the monthly syndromic data of 30 diseases since 1980
#' in Vietnam.
#'
#' @usage data(diseases)
#'
#' @format A data frame of 30 rows and 5 variables:
#' \itemize{
#'    \item \code{file}: name of data frame
#'    \item \code{nobs}: number of observations
#'    \item \code{nvar}: number of variables
#'    \item \code{year_start}: first year of the dataset
#'    \item \code{year_end}: last year of the dataset
#' }
#'
#' @details The dataset contains, the names of each data frame, the time range
#' of the data. This table can be used as a resume of all the other data frame
#' contained in this package and may be useful for computing on the various
#' files. \cr
#' \cr \code{amoebiasis} : amoebic dysentery
#' \cr \code{dysenteria} : dysenteria syndrome coming from unknown sources
#' \cr \code{hfmd} : hand-foot-and-mouth disease
#' \cr \code{nntetanus} : neonatal tetanus
#' \cr \code{shigella} : dysenteria by Shigella
#' \cr \code{vhf} : viral hemorrhagic fever
#' \cr\cr
#' For the data of \code{rabies}, the package \code{gdpm} contains the
#' vaccination data (named \code{vaccinated_rabies} in the \code{getid}
#' function) and as for rabies, a case lead to the death, the incidence and
#' mortality columns are the same and only the incidence column is outputted
#' in the function \code{getid}.
#'
#'
#' @source Data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"diseases"
