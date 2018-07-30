# System and Package needed
library(magrittr) # for the " %>% " pipe
library(dplyr) # for "bind_rows", "rename", "mutate", "select"
library(tidyr) # for "gather", "separate", "spread"
library(dictionary) # for the province dictionary

# Prerequisites  ---------------------------------------------------------------

# Disease table for translation
diseases <- matrix(c(
  "Ta"                            , "cholera",
  "Ta1"                           , "cholera",
  "T¶"                            , "cholera",
  "th.hµn"                        , "typhoid",        # typhoid fever
  "T.Han"                         , "typhoid",        # typhoid fever
  "Thuong han"                    , "typhoid",        # typhoid fever
  "Thhan"                         , "typhoid",        # typhoid fever
  "ThHan"                         , "typhoid",        # typhoid fever
  "LyTT"                          , "shigella",       # dysenteria by Shigella
  "Ly truc trung"                 , "shigella",       # dysenteria by Shigella
  "lþ TT"                         , "shigella",       # dysenteria by Shigella
  "Ly amip"                       , "amoebiasis",
  "lþ Amip"                       , "amoebiasis",
  "LyAmip"                        , "amoebiasis",
  "Tieu chay"                     , "diarrhea",
  "Tieuchay"                      , "diarrhea",
  "Tiªu ch¶y"                     , "diarrhea",
  "V.nao"                         , "encephalitis",
  "VNaoVR"                        , "encephalitis",
  "VNAO"                          , "encephalitis",
  "SXH"                           , "dengue",            # viral hemorrhagic fever
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
  "UVSS"                          , "nntetanus",      # neonatal tetanus
  "UV khac"                       , "tetanus",        # other tetanus
  "UVkhac"                        , "tetanus",        # other tetanus
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
  "TCM"                           , "hfmd",           # hand-foot-and-mouth disease
  "Lien cau lon"                  , "ssuis",          # streptococcus suis
  "dengue"                        , "dengue",
  "SotDengue"                     , "dengue",
  "DENGUE"                        , "dengue",
  "Dengue"                        , "dengue",
  "HC Ly"                         , "dysenteria",     # dysenteria syndrome
  "HCLy"                          , "dysenteria",     # dysenteria syndrome
  "Hoi chung ly"                  , "dysenteria",     # dysenteria syndrome
  "HCLY"                          , "dysenteria",     # dysenteria syndrome
  "HCly"                          , "dysenteria",     # dysenteria syndrome
  "Ta"                            , "cholera" ,
  "Thuong han"                    , "typhoid" ,       # typhoid fever
  "Ly truc trung"                 , "shigella" ,      # dysenteria by Shigella
  "Ly amip"                       , "amoebiasis",
  "Hoi chung ly"                  , "dysenteria",     # dysenteria syndrome
  "Ia chay"                       , "diarrhea",
  "Hoi chung nao cap"             , "encephalitis",
  "Sot Dengue"                    , "dengue",
  "Viem gan vi rut"               , "hepatitis",
  "Benh nghi dai"                 , "rabies",
  "Hoi chung mang nao"            , "meningitis",
  "Thuy dau"                      , "chickenpox",
  "Bach hau"                      , "diphteria",
  "Ho ga"                         , "pertussis",
  "Uon van so sinh"               , "nntetanus",      # neonatal tetanus
  "Uon van khac"                  , "tetanus",        # other tetanus
  "Bai liet"                      , "polio",
  "Soi"                           , "measles",
  "Quai bi"                       , "mumps",
  "Cum"                           , "ili",
  "A.P.C."                        , "adenovirus",
  "Dich hach"                     , "plague",
  "Than"                          , "anthrax",
  "Leptospira"                    , "leptospiriosis",
  "Thuong han va Pho thuong han"  , "typhoid",        # typhoid fever
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
province <- dictionary::provinces

# Lists of splits events in the province history in Vietnam (necessary for the
# merging function)
splits <- readRDS(file = "data-raw/splits.RDS")
ah_splits <- readRDS(file = "data-raw/ah_splits.RDS")

# Functions --------------------------------------------------------------------

# Use for both type of excel files ---------------------------------------------

# Translates from Vietnamese to English (by default: province's name).
translate <- function(df, col_names = "province", hash = province) {
  df[, col_names] <- df[, col_names] %>%
    unlist %>%  as.vector %>%
    sub("^\\d+\\.", "", .) %>% trimws
  df[, col_names] <-   hash[
    stringi::stri_escape_unicode(unlist(df[, col_names]))]
  df
}

# Use only for the excel file "Nien giam 201.r.xls" (NG)------------------------

# For each table, the columns' names are in two lines with a lot of missing data
# (see schema):
# Provinces | Month 1 | NA | Month 2 | NA | ... | Month 12 | NA | NA
#    NA     |   M     |  C |   M     |  C | ... |   M      |  C | NA
# Legend: M: Incidence ; C: Mortality and the last column corresponds to the
# size of the population for each province.
# This function translates and arranges the names of each column to obtain a
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
# containing all the cases and number of deaths by month and by province.
# Extracts the table, names it, and gather all diseases in one list of
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

# Extracts the year (contains in the file name) and transforms it in a new
# variable, to identify all the data per year.
read_year_NG <- function(filename) {
  year <- filename %>% gsub("[^0-9]", "", .) %>% as.integer
}

read_vect_year_NG <- function(vector) {
  year <- unlist(lapply(vector, read_year_NG))
}

# Reads one table contain in one sheet of an excel file ("Nien giam 201.r")
# and returns a list containing two data frames:
# 1- "demography": contains the size of the population by year and provinces.
# 2- "epidemiology": contains the incidence and the mortality data of one
# disease per month, year and province.
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

# Applies the function "read_one_table_NG" to all the sheets contained in one
# "Nien giam 201.r.xls" file and returns a list containing two data frames by
# disease.
read_list_NG <- function(lst, filename) {
  lst_total <- lapply(seq_along(lst), function(x) {
    read_one_table_NG(lst[[x]], filename)
  })
}

# Reads one "Nien giam 201.r.xls" file, applies the functions accordingly to
# return a list:
# List | plague (list) | epidemiology data frame
#                      | demographic data frame
#      | encephalitis (list) | epidemiology data frame
#                            | demographic data frame
#      |... (other diseases)
read_one_excel_NG <- function(filename) {
  lst_total <- filename %>%
    read_excel_disease_sheets_NG %>%
    read_list_NG(filename) %>%
    setNames(diseases[excel_sheets(filename)])
}

# Reads multiple "Nien giam 201.r.xls" files, to create one list:
# list | 2011 (list) | plague (list) | epidemiology data frame
#                                    | demographic data frame
#                    | encephalitis (list) | epidemiology data frame
#                                          | demographic data frame
#                    |... (other diseases)
#
#      | 2012 (list)  | plaque (list) | epidemiology data frame
#                                     | demographic data frame
#                     | encephalitis (list) | epidemiology data frame
#                                           | demographic data frame
#                     |... (other diseases)
#      |... (other years)
read_multiple_files_NG <- function(vector) {
  lst <- lapply (vector, read_one_excel_NG) %>%
    setNames(read_vect_year_NG(vector))
}

# Gathers together all the different epidemiological data frames (one by year)
# for one disease and gather them all together in one data frame.
group_year_NG <- function(lst, disease) {
  lapply(names(lst), function(x) {
    lst[[x]][[disease]]$epidemiology
  }) %>% bind_rows()
}

# On a complex list, selects the epidemiological data by year and return a
# list containing one epidemiological data frame by disease. Each table
# contains five variables: month, year, mortality (number of deaths), incidence
# (number of cases) and province.
make_epidemiology_NG <- function(lst){
  epidemiology <- lapply(names(lst[[1]]), function(y) {
    lst <- group_year_NG(lst, y)
  }) %>% setNames(names(lst[[1]]))
}

# On a complex list, selects only one disease (because all the diseases
# have the same demographic data) to create a demographic data frame with three
# variables: year, province and popsize.
make_demography_NG <- function(lst){
  demography <- lapply(seq_along(lst), function(x) {
    lst[[x]]$cholera$demography
  }) %>% bind_rows()
}

# Reads multiple "Nien giam 201.r.xls" files and creates a list containing two
# lists: the first one, contains all the epidemiological data by diseases and
# the second, contains all the demographic data.
make_demographic_epidemiology_NG <- function(vector) {
  total_lst <- read_multiple_files_NG(vector)
  epidemiology <- make_epidemiology_NG(total_lst)
  demography <- make_demography_NG(total_lst)
  total <- list(epidemiology, list(demography)) %>%
    setNames(c("epidemiology", "demography"))
}

# Use only for the excel file "YB 1980-2010.xls" -------------------------------

# Function specific to the Dack Lak province. On the original file, the name of
# the province is always written "Dak Lak". But, from 1980 until 2004, the name
# of this province should be written "Dack Lak".
dack_lak_function <- function(df) {
  df[which(df$year < 2004 & df$province == "Dak Lak"), ] <-
    filter(df, province == "Dak Lak", year < 2004) %>%
    mutate(province = "Dack Lak")
  df
}

# Reads the "YB 1980-2010.xls" file: contains the incidence and mortality of
# different disease by province and by month from 1980 until 2010. Selects
# the important data and converts them into the same format as the one used for
# the "Nien giam 201.r.xls" excel files. Returns a data frame with six variable:
#  year, month, province, incidence, mortality and popsize.
read_YB_file <- function (filename)  {
  require(readxl) # for "excel_sheets", "read_excel"
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
      popsize          = POP,
      month_incidence_ = starts_with("CASE"),
      month_mortality_ = starts_with("DEATH")) %>%
    mutate(popsize = as.numeric(popsize)) %>%
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

# Selects the epidemiological data by disease and returns a list containing one
# epidemiological data frame for each disease. Each table contains five
# variables: month, year, mortality (number of deaths), incidence (number of
# cases) and province.
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

# Selects only one disease (because all the diseases have the same demographic
# data) to create a demographic data frame with three variables: year, province
# and popsize.
make_demography_YB <- function(df){
  df %>%
    filter(disease == "dengue") %>%
    select(-one_of(c("month", "mortality", "incidence", "disease")))
}

# Reads the "YB 1980-2010.xls" excel file, applies the function "read_YB_file"
# and transforms the data frame in a list containing two lists:
# The first, contains all the epidemiological data and the second, contains
# the demographic data.
make_demographic_epidemiology_YB <- function(filename) {
  df_yb <- read_YB_file(filename)
  epidemiology <- make_epidemiology_YB(df_yb)
  demography <- make_demography_YB(df_yb)
  total <- list(epidemiology, list(demography)) %>%
    setNames(c("epidemiology", "demography"))
}

# Use for both type of excel files ---------------------------------------------

# Gathers all the epidemiology data together by diseases.
make_total_epidemiology <- function(lst1, lst2){
  diseases <- c(names(lst1$epidemiology), (names(lst2$epidemiology))) %>%
    unique
  epidemiology <- lapply(diseases, function(x){
    bind_rows(lst1$epidemiology[x], lst2$epidemiology[x])
  }) %>% setNames(diseases)
}

# Reads all the excel file (because of the difference of format, the two kinds
# have their proper functions) and applies the functions to gather the
# demographic and epidemiologic data together.
make_demography_epidemiology_total <- function(vector, filename) {
  total_YB <- make_demographic_epidemiology_YB(filename)
  total_NG <- make_demographic_epidemiology_NG(vector)
  epidemiology <- make_total_epidemiology(total_NG, total_YB)
  demography <-  bind_rows(total_NG$demography, total_YB$demography) %>%
    filter(duplicated(.) == FALSE)
  total <- list(epidemiology, list(demography)) %>%
    setNames(c("epidemiology", "demography"))
}

# Describes all the data frames contain in the package. Return a data frame with
# five variables: the names of the data frame (file), the dimension of the data
# frame (nvar & nobs), the first year of the data (year_start) and the last year
# of the data (year_end).
# This table can be used as a resume of all the other data frame contain in this
# package and may be useful for computing on the various files.
make_summary_table <- function(lst) {
  lapply(seq_along(lst),function(x){
    disease <- names(lst[x])
    year <- unique(lst[[x]]$year)
    from <- min(year) %>% paste0("-01-01")  %>% as.Date()
    to <- max(year) %>% paste0("-12-31")  %>% as.Date()
    data.frame(disease, from, to)
  }) %>% bind_rows %>% arrange(disease)
}

# Read data-raw and made data --------------------------------------------------

filename <- "data-raw/YB 1980-2010.xlsx"
vector <- c(
  "data-raw/Nien giam 2011r.xls", "data-raw/Nien giam 2012r.xls",
  "data-raw/Nien giam 2013r.xls", "data-raw/Nien giam 2014r.xls",
  "data-raw/Nien giam 2015r.xls")

total <- make_demography_epidemiology_total(vector, filename)
diseases <- make_summary_table(total$epidemiology)
list2env(total$epidemiology, environment())

devtools::use_data(diseases, overwrite = TRUE, internal = FALSE)
devtools::use_data(cholera, typhoid, shigella, amoebiasis, diarrhea,
  hfmd, malaria, hepatitis, rabies, meningitis, chickenpox, encephalitis,
  diphteria, pertussis, nntetanus, tetanus, polio, measles, mumps, rubella, ili,
  h5n1, adenovirus, plague, anthrax, leptospiriosis, ssuis, dengue,
  dysenteria, splits, ah_splits, internal = TRUE, overwrite = TRUE)

