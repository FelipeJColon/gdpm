# System and Package needed
library(magrittr) # for the " %>% " pipe
library(dplyr) # for "bind_rows", "rename", "mutate", "select"
library(tidyr) # for "gather", "separate", "spread"

# Prerequisites  ---------------------------------------------------------------

# Disease table for translation
diseases <- matrix(c(
  "Ta"                            , "cholera",
  "Ta1"                           , "cholera",
  "T¶"                            , "cholera",
  "th.hµn"                        , "typhoid",     # typhoid fever
  "T.Han"                         , "typhoid",     # typhoid fever
  "Thuong han"                    , "typhoid",     # typhoid fever
  "Thhan"                         , "typhoid",     # typhoid fever
  "ThHan"                         , "typhoid",     # typhoid fever
  "LyTT"                          , "shigella",    # dysenteria by Shigella
  "Ly truc trung"                 , "shigella",    # dysenteria by Shigella
  "lþ TT"                         , "shigella",    # dysenteria by Shigella
  "Ly amip"                       , "amoebiasis",
  "lþ Amip"                       , "amoebiasis",
  "LyAmip"                        , "amoebiasis",
  "Tieu chay"                     , "diarrhea",
  "Tieuchay"                      , "diarrhea",
  "Tiªu ch¶y"                     , "diarrhea",
  "V.nao"                         , "encephalitis",
  "VNaoVR"                        , "encephalitis",
  "VNAO"                          , "encephalitis",
  "SXH"                           , "vhf",         # viral hemorrhagic fever
  "Sot ret"                       , "malaria",
  "sot ret"                       , "malaria",
  "V.Gan"                         , "hepatitis",
  "Vgan"                          , "hepatitis",
  "VGAN"                          , "hepatitis",
  "Viem gan vi rut"               , "hepatitis",
  "VGanVR"                        , "hepatitis",
  "Dai"                           , "rabies",
  "dai"                           , "rabies",
  "V.MN"                          , "meningitis",
  "VMN"                           , "meningitis",
  "VMnaoMC"                       , "meningitis",
  "T.Dau"                         , "chickenpox",
  "Thuydau"                       , "chickenpox",
  "B.Hau"                         , "diphteria",
  "Bachhau"                       , "diphteria",
  "bachhau"                       , "diphteria",
  "Ho ga"                         , "pertussis",
  "Hoga"                          , "pertussis",
  "UVSS"                          , "nntetanus",   # neonatal tetanus
  "UV khac"                       , "tetanus",     # other tetanus
  "UVkhac"                        , "tetanus",     # other tetanus
  "Liet MC"                       , "polio",
  "Bailiet"                       , "polio",
  "Soi"                           , "measles",
  "Quai bi"                       , "mumps",
  "quaibi"                        , "mumps",
  "Quaibi"                        , "mumps",
  "Rubella"                       , "rubella",
  "Cum"                           , "ili",
  "HCcum"                         , "ili",
  "Cum A H5N1"                    , "h5n1",
  "CumA_H5N1"                     , "h5n1",
  "CumA(H5N1)"                    , "h5n1",
  "CumH5N1"                       , "h5n1",
  "H5N1"                          , "h5n1",
  "APC"                           , "adenovirus",
  "AdenoVR"                       , "adenovirus",
  "Dich hach"                     , "plague",
  "dichhach"                      , "plague",
  "Dichhach"                      , "plague",
  "Than"                          , "anthrax",
  "Xoắc khuẩn vàng da"            , "leptospiriosis",
  "Leptospira"                    , "leptospiriosis",
  "Lepto"                         , "leptospiriosis",
  "TCM"                           , "enterovirus",
  "Lien cau lon"                  , "ssuis",        # Streptococcus suis
  "dengue"                        , "dengue",
  "SotDengue"                     , "dengue",
  "DENGUE"                        , "dengue",
  "Dengue"                        , "dengue",
  "HC Ly"                         , "dysenteria",     # Dysenteria syndrome
  "HCLy"                          , "dysenteria",     # Dysenteria syndrome
  "Hoi chung ly"                  , "dysenteria",     # Dysenteria syndrome
  "HCLY"                          , "dysenteria",     # Dysenteria syndrome
  "HCly"                          , "dysenteria",     # Dysenteria syndrome
  "Ta"                            , "cholera" ,
  "Thuong han"                    , "typhoid" ,   # typhoid fever
  "Ly truc trung"                 , "shigella" ,
  "Ly amip"                       , "amoebiasis",
  "Hoi chung ly"                  , "dysenteria",
  "Ia chay"                       , "diarrhea",
  "Hoi chung nao cap"             , "encephalitis",
  "Sot Dengue"                    , "dengue",
  "Viem gan vi rut"               , "hepatitis",
  "Benh nghi dai"                 , "rabies",
  "Hoi chung mang nao"            , "meningitis",
  "Thuy dau"                      , "chickenpox",
  "Bach hau"                      , "diphteria",
  "Ho ga"                         , "pertussis",
  "Uon van so sinh"               , "nntetanus",   # neonatal tetanus
  "Uon van khac"                  , "tetanus",
  "Bai liet"                      , "polio",
  "Soi"                           , "measles",
  "Quai bi"                       , "mumps",
  "Cum"                           , "ili",
  "A.P.C."                        , "adenovirus",
  "Dich hach"                     , "plague",
  "Than"                          , "anthrax",
  "Leptospira"                    , "leptospiriosis",
  "Thuong han va Pho thuong han"  , "typhoid",
  "Tieu chay"                     , "diarrhea",
  "Viem nao vi rut"               , "encephalitis",
  "So tiem phong dai"             , "rabies",
  "Dengue"                        , "dengue",
  "Sot xuat huyet"                , "dengue",
  "Viem mang nao mo cau"          , "meningitis",
  "Viem nao"                      , "encephalitis",
  "Viem gan"                      , "hepatitis",
  "Hoi chung cum"                 , "ili",
  "Cum A/H5N1"                    , "h5n1",
  "Liet mem cap"                  , "polio",
  "Rubella"                       , "rubella"),
  ncol=2, byrow=T)
diseases <- setNames(diseases[, 2], diseases[, 1])

# Colnames table for translation
names_col <- matrix(c(
  "C"                 , "C",
  "M"                 , "M",
  "ChÕt"              , "C",
  "M¾c"               , "M",
  "Tỉnh/thành"        , "province",
  "TØnh/T.phè"        , "province",
  "TØnh/thµnh"        , "province",
  "TØnh, thµnh"       , "province",
  "Tháng 1"           , "month 1",
  "Th¸ng 1"           , "month 1",
  "Th¸ng 2"           , "month 2",
  "Th¸ng 3"           , "month 3",
  "Th¸ng 4"           , "month 4",
  "Th¸ng 5"           , "month 5",
  "Th¸ng 6"           , "month 6",
  "Th¸ng 7"           , "month 7",
  "Th¸ng 8"           , "month 8",
  "Th¸ng 9"           , "month 9",
  "Th¸ng 10"          , "month 10",
  "Th¸ng 11"          , "month 11",
  "Th¸ng 12"          , "month 12",
  "Tháng 2"           , "month 2",
  "Tháng 3"           , "month 3",
  "Tháng 4"           , "month 4",
  "Tháng 5"           , "month 5",
  "Tháng 6"           , "month 6",
  "Tháng 7"           , "month 7",
  "Tháng 8"           , "month 8",
  "Tháng 9"           , "month 9",
  "Tháng 10"          , "month 10",
  "Tháng 11"          , "month 11",
  "Tháng 12"          , "month 12",
  "Dân số 2006"       , "popsize",
  "Dân sè 2003"       , "popsize",
  "D©n sè 2007"       , "popsize",
  "Dân số 2004"       , "popsize",
  "D©n sè 2002"       , "popsize",
  "D©n sè 2001"       , "popsize",
  "D©n sè 2000"       , "popsize",
  "D©n sè 1999"       , "popsize",
  "D©n sè 1997"       , "popsize",
  "D©n sè 1996"       , "popsize",
  "D©n sè 1998"       , "popsize"),
  ncol=2, byrow=T)
names_col <- setNames(names_col[, 2], names_col[, 1])

# Provinces tables for translation
province <- readRDS("data-raw/province.RDS")

# Functions --------------------------------------------------------------------

# Use for both type of excel files ---------------------------------------------

# Translates all the province names from Vietnamese to English.
translate <- function(df, col_names = "province", hash = province) {
  df[, col_names] <- hash[unlist(df[, col_names])]
  df
}

# Use only for the excel file "Nien giam 201.r.xls" (NG)------------------------

# For each table, the columns' names are in two lines with a lot of missing data
# (in the original file) :
# Provinces | Month 1 | NA | Month 2 | NA | ... | Month 12 | NA | NA
#    NA     |   M     |  C |   M     |  C | ... |   M      |  C | NA
# Legend: M: Incidence ; C: Mortality and the last column without names
# correspond to the size of the population for each province.
# This function translate and arrange the names of each column to obtain a
# data frame with columns' names in one line, without NA, in English.
make_colnames_NG <- function(df) {
  colnames(df) <- names_col[colnames(df)]
  sel <- which(df[1, ] == "C")
  names(df)[sel] <- names(df)[sel-1]
  names(df) <- apply(cbind(names(df), unlist(df[1, ])), 1, paste,
    collapse = " ") %>%
    gsub("NA NA", "popsize", .) %>%
    gsub(".NA", "", .)
  df <- df[-1, ] %>% filter(!is.na(`province`))
}

# In each excel file, each sheet is named by a disease and contain one table
# containing all the cases and number of deaths for one disease by month and by
# province. Extracts the table,names it and gather all diseases in one list of
# multiple data frames.
lapply2 <- function(X, ...) setNames(lapply(X, ...), X)

read_excel_disease_sheets_NG <- function(filename) {
  require(readxl) # for "excel_sheets", "read_excel"
  capture.output(lst_df <- excel_sheets(filename) %>%
      lapply2(function(disease_sheets)
        read_excel(filename, disease_sheets) %>%
          make_colnames_NG))
  return(lst_df)
}

# Extracts the year (contain in the name of a file) and transforms it in a new
# variable, to identify all the data per year.
read_year_NG <- function(filename) {
  year <- filename %>% gsub("[^0-9]", "", .) %>% as.integer
}

read_vect_year_NG <- function(vector) {
  year <- unlist(lapply(vector, read_year_NG))
}

# Read one table contains in one sheet of one excel file ("Nien giam 201.r")
# and transform it to the right format. Return a data frame with 6 variables:
# province, year, month, incidence, mortality and popsize.
read_one_table_NG <- function(df, filename) {
  df %<>% translate %>%
    mutate(year = read_year_NG(filename)) %>%
    gather(key, value, contains("month")) %>%
    separate(key, letters[1:3], " ") %>%
    select(-a) %>%
    spread(c, value) %>%
    rename(month = b, incidence = M, mortality = C) %>%
    mutate(month = factor(month.name[as.numeric(month)],
      month.name, ordered=TRUE),
      incidence  = as.integer(incidence),
      mortality  = as.integer(mortality),
      popsize    = as.numeric(popsize)) %>%
    (function(x) lapply(
      list(epidemiology = names(x)[-which(names(x) == "popsize")],
        demography = c("province", "year", "popsize")),
      function(y) unique(subset(x, select = y))
    ))
}

# Read a list of tables (corresponding to all the sheets in one file) and
# apply the function to transform them accordingly.
read_list_NG <- function(lst, filename) {
  lst_total <- lapply(seq_along(lst), function(x) {
    read_one_table_NG(lst[[x]], filename)
  })
}

# Read the "Nien giam 201.r.xls" file : each file contains the incidence and
# mortality data for different diseases, by province and by month for one year,
# and return a list containing multiples lists (ordered by diseases) containing
# two data frame:
# 1- "demography" : contains only the province, year and size of the population
# 2- "epidemiology" : contains the incidence and the mortality data of one
# disease per month, year and province
read_one_excel_NG <- function(filename) {
  lst_total <- filename %>%
    read_excel_disease_sheets_NG %>%
    read_list_NG(filename) %>%
    setNames(diseases[excel_sheets(filename)])
}

# Read multiple "Nien giam 201.r.xls" files, gather each demographic and
# epidemiological data frames together to create one list :
# list | 2011(list) | plague (list) | epidemiology data frame
#                                   | demographic data frame
#                   | encephalitis(list) | epidemiology data frame
#                                        | demographic data frame
#                   |... (other diseases)
#
#     | 2012(list) | plaque (list) | epidemiology data frame
#                                  | demographic data frame
#                  | encephalitis(list) | epidemiology data frame
#                                       | demographic data frame
#                  |... (other diseases)
#     |... (other years)
#
read_multiple_files_NG <- function(vector) {
  lst <- lapply (vector, read_one_excel_NG) %>%
    setNames(read_vect_year_NG(vector))
}

# Gather together all the table for one disease in one table containing all the
# epidemiological data.
group_year_NG <- function(lst, disease) {
  lapply(names(lst), function(x) {
    lst[[x]][[disease]]$epidemiology
  }) %>% bind_rows()
}

# On a complex list, select the epidemiological data by year and return a
# list containing one epidemiological data frame for each disease. Each table
# contains 5 variables : month, year, mortality (number of death), incidence
# (number of cases) and province.
make_epidemiology_NG <- function(lst){
  epidemiology <- lapply(names(lst[[1]]), function(y) {
    lst <- group_year_NG(lst, y)
  }) %>% setNames(names(lst[[1]]))
}

# On a complex list, selects only one disease (because all the diseases
# has the same demographic data) to create a demographic data frame with three
# variables: year, province and popsize.
make_demography_NG <- function(lst){
  demography <- lapply(seq_along(lst), function(x) {
    lst[[x]]$cholera$demography
  }) %>% bind_rows()
}

# Read multiple "Nien giam 201.r.xls" files and create a list with two
# lists:
# The first one contains the epidemiological data and the second contains
# the demographic data.
make_demographic_epidemiology_NG <- function(vector) {
  total_lst <- read_multiple_files_NG(vector)
  epidemiology <- make_epidemiology_NG(total_lst)
  demography <- make_demography_NG(total_lst)
  total <- list(epidemiology, list(demography)) %>%
    setNames(c("epidemiology", "demography"))
}

# Use only for the excel file "YB 1980-2010.xls" -------------------------------

# Fonction specific to the Dack Lak province. On the original file, the name of
# the province is always written "Dak Lak". But, from 1980 until 2004, the name
# of this province should be written "Dack Lak".
dack_lak_function <- function(df) {
  df[which(df$year < 2004 & df$province == "Dak Lak"), ] <-
    filter(df, province == "Dak Lak", year < 2004) %>%
    mutate(province = "Dack Lack")
  df
}

# Read the "YB 1980-2010.xls" file : contains the incidence and mortality of
# different disease by province and by month from 1980 until 2010.
# Return a dataframe with all the mortality and incidence data for all the
# diseases by year, month and province.
read_YB_file <- function (filename)  {
  read_excel(filename) %>%
    filter(!is.na(SLNO)) %>%
    select(
      -matches("SLNO|Checked"),
      -contains("SUM"),
      -contains("pht")) %>%
    select(
      year             = YR,
      disease          = DISEASE,
      province         = PROVINCE,
      popsize          = as.numeric(POP),
      month_incidence_ = starts_with("CASE"),
      month_mortality_ = starts_with("DEATH")) %>%
    translate %>%
    translate("disease", diseases) %>%
    gather(key, value, contains("month")) %>%
    separate(key, letters[1:3], "_") %>%
    select(-a) %>%
    spread(b, value) %>%
    rename(month = c) %>%
    mutate(month = factor(month.name[as.numeric(month)],
      month.name, ordered=TRUE),
      incidence  = as.integer(incidence),
      mortality  = as.integer(mortality),
      popsize    = as.numeric(popsize)) %>%
    dack_lak_function
}

# On an YB data frame, select the epidemiological data by disease and return a
# list containing one epidemiological data frame for each disease. Each table
# contains 5 variables : month, year, mortality (number of death), incidence
# (number of cases) and province.
make_epidemiology_YB <- function(df){
  disease <- unique(df$disease)
  epidemiology <- lapply(disease, function(x) {
    filter(df, disease == x) %>%
      select(-one_of(c("disease", "popsize"))) %>%
      mutate(
        incidence = as.integer(incidence),
        mortality = as.integer(mortality))
  }) %>% setNames(unique(df$disease))
}

# On an YB data frame, selects only one disease (because all the diseases
# has the same demographic data) to create a demographic data frame with three
# variables: year, province and popsize.
make_demography_YB <- function(df){
  df %>%
    filter(disease == "dengue") %>%
    select(-one_of(c("month", "mortality", "incidence", "disease")))
}

# Read the "YB 1980-2010.xls" excel file, apply the function "read_YB_file" and
# transforms the data frame in a list containing two lists :
# The first one contains the epidemiological data and the second contains
# the demographic data.
make_demographic_epidemiology_YB <- function(filename) {
  df_yb <- read_YB_file(filename)
  epidemiology <- make_epidemiology_YB(df_yb)
  demography <- make_demography_YB(df_yb)
  total <- list(epidemiology, list(demography)) %>%
    setNames(c("epidemiology", "demography"))
}

# Use for both type of excel files ---------------------------------------------

# Gather all the epidemiology data together by diseases.
make_total_epidemiology <- function(lst1, lst2){
  diseases <- c(names(lst1$epidemiology), (names(lst2$epidemiology))) %>%
    unique
  epidemiology <- lapply(diseases, function(x){
    bind_rows(lst1$epidemiology[x], lst2$epidemiology[x])
  }) %>% setNames(diseases)
}

# Read all the excel file (because of the difference of format, the two kind as
# their proper functions) and apply the functions to gather  the demographic
# and epidemiologic data together and join them by disease (for the
make_demography_epidemiology_total <- function(vector, filename) {
  total_YB <- make_demographic_epidemiology_YB(filename)
  total_NG <- make_demographic_epidemiology_NG(vector)
  epidemiology <- make_total_epidemiology(total_NG, total_YB)
  demography <-  bind_rows(total_NG$demography, total_YB$demography) %>%
    filter(duplicated(.) == FALSE)
  total <- list(epidemiology, list(demography)) %>%
    setNames(c("epidemiology", "demography"))
}

# Read all the file and returns a summary table, descripting all the data frame
# contain in the package. Return a data frame with 5 variables:
# The names of the data frame (file), the dimension of the data frame
# (nvar & nobs), the first year of the data (year_start) and the last year of
# the data (year_end).
# This table can be use as a resume of all the other data frame contain in this
# package and may be useful for computing on the various files.
make_summary_table <- function(lst) {
  lapply(seq_along(lst),function(x){
    file <- names(lst[x])
    nobs <- dim(lst[[x]])[1]
    nvar <- dim(lst[[x]])[2]
    year <- unique(lst[[x]]$year)
    year_start <- min(year)
    year_end <- max(year)
    data.frame(file, nobs, nvar, year_start, year_end)
  }) %>% bind_rows
}

# Read data-raw and made data --------------------------------------------------

filename <- "data-raw/YB 1980-2010.xls"
vector <- c(
  "data-raw/Nien giam 2011r.xls", "data-raw/Nien giam 2012r.xls",
  "data-raw/Nien giam 2013r.xls", "data-raw/Nien giam 2014r.xls",
  "data-raw/Nien giam 2015r.xls")

total <- make_demography_epidemiology_total(vector, filename)
epidemiology_summary <- make_summary_table(total$epidemiology)
list2env(total$epidemiology, environment())

devtools::use_data(epidemiology_summary, overwrite = TRUE)
list_data <- grep(paste(names(total$epidemiology), collapse = "|"),
  ls(), value = T)
for(I in seq_along(list_data)){
  save(list = list_data[I],
       file = paste("data/", list_data[I], ".rda", sep = ""),
      envir = .GlobalEnv)
}

