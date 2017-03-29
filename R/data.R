#' Epidemiologic Summary Data From Monthly Surveillance Data
#'
#' A dataset containing the description of all the data frames contained in the
#' package.
#'
#' The dataset contains, the names of each data frame,the dimension of the data
#' frame, the first year and the last year of the data.
#' This table can be used as a resume of all the other data frame contain in
#' this package and may be useful for computing on the various files.
#'
#' @usage data(epidemiology_summary)
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
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"epidemiology_summary"


#' Adenovirus Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for
#' adenovirus from the Vietnamese GDPM, aggregated by month and province from
#' January 1980 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(adenovirus)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"adenovirus"

#' Amoebiasis Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for
#' amoebiasys from the Vietnamese GDPM, aggregated by month and province from
#' January 1980 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#' In 1990, the number and the name of the province are different for amoebiasis
#' and hepatitis compared to the other disease. In 1990, the number and name of
#' the province changed in Vietnam, but some hospital made only the change a
#' year after.
#'
#' @usage data(amoebiasis)
#'
#' @format A data frame of 23244 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"amoebiasis"

#' Anthrax Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for anthrax
#' from the Vietnamese GDPM, aggregated by month and province from January 1990
#' to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(anthrax)
#'
#' @format A data frame of 18492 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"anthrax"

#' Chickenpox Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for
#' chickenpox from the Vietnamese GDPM, aggregated by month and province from
#' January 1980 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(chickenpox)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"chickenpox"

#' Cholera Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for cholera
#' from the Vietnamese GDPM, aggregated by month and province from January 1980
#' to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(cholera)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"cholera"

#' Dengue Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for dengue
#' from the Vietnamese GDPM, aggregated by month and province from January 1980
#' to December 2010.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(dengue)
#'
#' @format A data frame of 19512 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"dengue"

#' Diarrhea Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for diarrhea
#' from the Vietnamese GDPM, aggregated by month and province from January 1980
#' to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(diarrhea)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"diarrhea"

#' Diphteria Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for diphteria
#' from the Vietnamese GDPM, aggregated by month and province from January 1980
#' to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(diphteria)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"diphteria"

#' Dysenteria Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for
#' dysenteria syndrome from the Vietnamese GDPM, aggregated by month and
#' province from January 1980 to December 2010.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(dysenteria)
#'
#' @format A data frame of 19512 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"dysenteria"

#' Encephalitis Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for
#' encephalitis from the Vietnamese GDPM, aggregated by month and province from
#' January 1980 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(encephalitis)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"encephalitis"

#' Enterovirus Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for
#' enterovirus from the Vietnamese GDPM, aggregated by month and province from
#' January 2011 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(entevirus)
#'
#' @format A data frame of 3780 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"enterovirus"

#' H5N1 Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for H5N1 from
#' the Vietnamese GDPM, aggregated by month and province from January 2004 to
#' December 2015 (exception of year 2006)
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(h5n1)
#'
#' @format A data frame of 8352 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"h5n1"

#' Hepatitis Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for hepatitis
#' from the Vietnamese GDPM, aggregated by month and province from January 1980
#' to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#' In 1990, the number and the name of the province are different for amoebiasis
#' and hepatitis compared to the other disease. In 1990, the number and name of
#' the province changed in Vietnam, but some hospital made only the change a
#' year after.
#'
#' @usage data(hepatitis)
#'
#' @format A data frame of 23244 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"hepatitis"

#' ILI Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for ILI
#' (Influenza-Like Illness) from the Vietnamese GDPM, aggregated by month and
#' province from January 1980 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(ili)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"ili"

#' Leptospiriosis Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for
#' leptospiriosis from the Vietnamese GDPM, aggregated by month and province
#' from January 1986 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(leptospiriosis)
#'
#' @format A data frame of 20412 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"leptospiriosis"

#' Malaria Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for malaria
#' from the Vietnamese GDPM, aggregated by month and province from January 2011
#' to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(malaria)
#'
#' @format A data frame of 3780 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"malaria"

#' Measles Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for measles
#' from the Vietnamese GDPM, aggregated by month and province from January 1980
#' to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(measles)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"measles"

#' Meningitis Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for
#' meningitis from the Vietnamese GDPM, aggregated by month and province from
#' January 1980 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(meningitis)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"meningitis"

#' Mumps Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for mumps
#' from the Vietnamese GDPM, aggregated by month and province from January 1980
#' to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(mumps)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"mumps"

#' Neonatal Tetanus Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for neonatal
#' tetanus from the Vietnamese GDPM, aggregated by month and province from
#' January 1990 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(nntetanus)
#'
#' @format A data frame of 18492 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"nntetanus"

#' Pertussis Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for pertussis
#' from the Vietnamese GDPM, aggregated by month and province from January 1980
#' to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(pertussis)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"pertussis"

#' Plague Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for plague
#' from the Vietnamese GDPM, aggregated by month and province from January 1980
#' to December 2015 (except year 1993)
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(plague)
#'
#' @format A data frame of 22656 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"plague"

#' Polio Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for polio
#' from the Vietnamese GDPM, aggregated by month and province from January 1980
#' to December 2015
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#' Until 1997 (include), the data contains at the same time the polio cases and
#' the suspected polio cases (Acute Flaccid Paralysis). The last case of polio
#' was in 1997, after this year the dataset contains only the case of suspected
#' polio (Acute Flaccid Paralysis).
#'
#' @usage data(polio)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"polio"

#' Rabies Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for rabies
#' from the Vietnamese GDPM, aggregated by month and province from January 1980
#' to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#' The dataset contains the number of people who go to the hospital to have an
#' injection of rabies vaccine becauses of an animal's bite.
#'
#' @usage data(rabies)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"rabies"

#' Rubella Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for rubella
#' from the Vietnamese GDPM, aggregated by month and province from January 2011
#' to December 2015 and for the year 2006
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(rubella)
#'
#' @format A data frame of 4548 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"rubella"

#' Shigella Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for
#' dysenteria by Shigella from the Vietnamese GDPM, aggregated by month and
#' province from January 1980 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(shigella)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"shigella"

#' Streptococcus suis Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for
#' Streptococcus suis from the Vietnamese GDPM, aggregated by month and province
#' from January 2011 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(ssuis)
#'
#' @format A data frame of 3780 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"ssuis"

#' Tetanus Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for tetanus
#' from the Vietnamese GDPM, aggregated by month and province from January 1980
#' to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(tetanus)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"tetanus"

#' Typhoid Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for typhoid
#' fever from the Vietnamese GDPM, aggregated by month and province from January
#' 1980 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(typhoid)
#'
#' @format A data frame of 23292 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"typhoid"

#' VHF Epidemiologic Data From Monthly Surveillance Data
#'
#' A dataset containing \code{incidence} and \code{mortality} data for viral
#' hemorrhagic fever from the Vietnamese GDPM, aggregated by month and province
#' from January 2011 to December 2015.
#'
#' The incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. Before 1991, \code{NA} corresponds to
#' "no case" or "no report". After 1991, \code{NA} corresponds to "no report"
#' and \code{0} to "no case".
#'
#' @usage data(vhf)
#'
#' @format A data frame of 3780 rows and 5 variables:
#' \itemize{
#'    \item \code{province}: province name
#'    \item \code{year}: year
#'    \item \code{month}: month
#'    \item \code{incidence}: number of new cases
#'    \item \code{mortality}: number of death cases
#' }
#'
#' @source data provide by the Vietnamese General Department of Preventive
#' Medicine (GDPM) and by the National Institute of Hygiene and Epidemiology
#' (NIHE), and supported by the Oxford University Clinical Research Unit (OUCRU)
#' and the French National Research Institute for Sustainable Development (IRD).
#'
"vhf"
