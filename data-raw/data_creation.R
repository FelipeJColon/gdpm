##System and Package needed
library(magrittr) # for the " %>% " pipe
library(dplyr) # for "bind_rows", "rename", "mutate", "select"
library(tidyr) # for "gather", "separate", "spread"

########################## PREREQUISITE ##############

## disease table for translation ####
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
  "HCly"                          , "dysenteria",
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
diseases <- setNames(diseases[,2],diseases[,1])

## colnames table for translation ####
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
names_col <- setNames(names_col[,2],names_col[,1])

## provinces tables for translation ####
province <- readRDS("data-raw/province.RDS")

########################### FUNCTIONS ################

## For each table, the columns' names are in two lines with a lot of missing data (in the original file).
## They, first, need to be translated and then, they need to be copied and pasted to fill the empty "colnames".
## The function returns the data frame with "clean" columns' names in one line, without NA, in English
## and all the tables will also have exactly the same "colnames" to permit linking them afterwards.
make_colnames <- function(df) {
  colnames(df) <- names_col[colnames(df)]
  sel <- which(df[1,]=="C")
  names(df)[sel] <- names(df)[sel-1]
  names(df) <- apply(cbind(names(df),unlist(df[1,])),1,paste,collapse=" ") %>%
    gsub("NA NA","popsize",.) %>% gsub(".NA","",.)
  df <- df[-1,] %>% filter(!is.na(`province`))
}

## In each excel file, each sheet is named by a disease and contain one table containing all
## the cases and deaths for one disease by month and by province. Extracts the table and names it
## and gather all diseases in a list of multiple data frames.
lapply2 <- function(X, ...) setNames(lapply(X, ...),X)

read_excel_disease_sheets <- function(filename) {
  require(readxl) # for "excel_sheets", "read_excel"
  capture.output(tab <- excel_sheets(filename) %>%
      lapply2(function(disease_sheets)
        read_excel(filename,disease_sheets) %>% make_colnames))
  return(tab)
}

## Translates all the province names from Vietnamese to English.
translate_prov <- function(df,col_names="province",hash=province) {
  df[,col_names] <- hash[unlist(df[,col_names])]
  df
}

## Extracts the year (contain in the name of a file) and transforms it in a new variable, to
## identify all the data per year.
read_year <- function(filename){
  year <- filename %>% gsub("[^0-9]","",.) %>% as.integer
}

read_vect_year <- function(vector){
  year <-unlist( lapply(vector, read_year))
}

### Read the table contain in one sheet of one excel file ("Nien..")
## and transform it to the right format. Return a data frame with 6 variables:
## province, year, month, incidence, mortality and popsize.
read_one_table <- function(df,filename){
  df %<>% translate_prov %>%
    mutate(year=read_year(filename)) %>%
    gather(key,value,contains("month")) %>%
    separate(key,letters[1:3]," ") %>%
    select(-a) %>%
    spread(c,value) %>%
    rename(month=b,incidence=M,mortality=C) %>%
    mutate(month=factor(month.name[as.numeric(month)],month.name,ordered=TRUE),
      incidence=as.integer(incidence),
      mortality=as.integer(mortality),
      popsize=as.numeric(popsize)) %>%
    (function(x)lapply(
     list(epidemiology=names(x)[-which(names(x)=="popsize")],
      demography=c("province","year","popsize")),
    function(y)unique(subset(x,select=y))
    ))
}

## Read a list of tables (corresponding to all the sheets in one file) and
## apply the function to transform them accordingly.
read_list <- function(lst,filename){
  tmp <- lapply(c(1:length(lst)), function(x) read_one_table(lst[[x]],filename))
}

### Read the "Nien giam 201.r.xls" file : each file contains
## the incidence and mortality data for different diseases, by province and by month for one year,
## and return a list containing multiples lists (ordered by diseases) containing two data frame:
## 1- "demography" : contains only the province, year and size of the population
## 2- "epidemiology" : contains the incidence and the mortality data of one disease per month, year and province
read_one_excel <- function(filename){
  tmp <- filename %>%
    read_excel_disease_sheets %>%
    read_list (filename)%>%
    setNames(diseases[excel_sheets(filename)])
}

## Read multiple "Nien giam 201.r.xls" files, gather each demographic and epidemiological data frames
## together to create one list containing multiple lists ordered by year, containing
## one list by disease, containing one data frame
## with the demographic data and one data frame with the epidemiological data.
read_multiple_files <- function(vector){
  lst <- lapply (vector,read_one_excel)
  names(lst) <- read_vect_year(vector)
  lst
}


## Gather together all the table for one disease
## in one table containing all the epidemiological data.
group_year <- function(lst,disease){
  lapply(1:length(lst),function(x){
    lst[[x]][disease][[1]]$epidemiology
  }) %>% bind_rows()
}

## Gather together all the table for each disease by year and return a list
## containing mutliple epidemiological data frame (each one correspond to one disease).
## Each table contains 5 variables : month, year, mortality (number of death), incidence (number of cases)
## and province.
make_epidemiology <- function(vector){
  tmp <- read_multiple_files(vector)
  epidemio <- lapply(names(tmp[[1]]),function(y){
      lst <- group_year(tmp,y)
    })
  names(epidemio) <- names(tmp[[1]])
  epidemio
}

## Read all the files and select only one disease (because all the diseases has the
## same demographic data), and regroup all the year together in one file.
## Return a demographic data frame with three variables: year, province and popsize.
make_demography <- function(vector){
  tmp <- read_multiple_files(vector)
  lapply(1:length(tmp),function(x){
    tmp[[x]]$cholera$demography
  }) %>% bind_rows()
}

### Special function for the province Dack Lak, in the original file Dack Lak is written Dak Lak.
## But, until 2004, the province should be written "Dack Lak".
dack_lak_function <- function(df){
  df[which(df$year<2004 & df$province=="Dak Lak"),] <-
    filter(df,province=="Dak Lak",year<2004) %>%
    mutate(province="Dack Lack")
  df
}


### Read the "YB 1980-2010.xls" file : contains the incidence and mortality of different
## disease by province and by month from 1980 until 2010.
## Return a dataframe with all the mortality and incidence data for all the diseases by year, month and province.
read_YB_file <- function (filename)  {
  tmp <- read_excel(filename)%>%
    filter(!is.na(SLNO)) %>%
    select(-matches("SLNO|Checked"),
      -contains("SUM"),
      -contains("pht")) %>%
    select(year=YR,disease=DISEASE,province=PROVINCE,
      popsize=as.numeric(POP),
      month_incidence_=starts_with("CASE"),
      month_mortality_=starts_with("DEATH"))%>%
    translate_prov %>%
    translate_prov("disease",diseases) %>%
    gather(key,value,contains("month")) %>%
    separate(key,letters[1:3],"_") %>%
    select(-a) %>%
    spread(b,value)  %>%
    rename(month=c) %>%
    mutate(month=factor(month.name[as.numeric(month)],month.name,ordered=TRUE),
      incidence=as.integer(incidence),
      mortality=as.integer(mortality),
      popsize=as.numeric(popsize)) %>%
    dack_lak_function

}

### Read the "YB 1980-2010.xls" excel file, apply the function "read_YB_file" and transforms
### the data frame in a list containing only one data frame for each disease.
make_epidemiology_YB <- function(filename){
  tmp <- read_YB_file(filename)
  lst <- lapply(c(1:length(unique(tmp$disease))),function(x){
    filter(tmp,disease==unique(tmp$disease)[x]) %>% select(-one_of(c("disease","popsize"))) %>%
      mutate(incidence=as.integer(incidence),
        mortality=as.integer(mortality))
  })
  names(lst) <- unique(tmp$disease)
  lst
}

## Read the YB excel file and selects only one disease (because all the diseases has the
## same demographic data) to create a demographic data frame with three variables: year, province and popsize.
make_demography_YB <- function(filename){
  tmp <- read_YB_file(filename) %>%
    filter(disease=="dengue") %>%
    select(-one_of(c("month","mortality","incidence","disease")))
}

## Read all the excel file (because of the difference of format, the two kind as their proper functions)
## and apply the functions to select only the demographic or epidemiologic data and join them by disease
## (for the epidemiologic data).
## Return a data frame containing three variables : year, province and popsize
make_total_demography <- function(vector,filename){
  demography <- make_demography(vector)
  demo <- make_demography_YB(filename)
  bind_rows(demo,demography)
}

## Return a list containing one data frame by disease. Each data frame contains 5 variables :
## year, month, province, incidence and mortality
make_total_epidemiology <- function(vector,filename){
  epidemio <- make_epidemiology_YB(filename)
  epidemiology <- make_epidemiology(vector)
  diseases <- unique(c(names(epidemio),names(epidemiology)))
  lst <- lapply(diseases,function(x) bind_rows(epidemio[x],epidemiology[x]))
  names(lst) <- diseases
  lst
}


## Read all the file and returns a summary table, descripting all the data frame contain in the
## package. Return a data frame with 5 variables:
## The names of the data frame (file), the dimension of the data frame (nvar & nobs), the first year of the data
## (year_start) and the last year of the data (year_end)
## This table can be use as a resume of all the other data frame contain in this package and may
## be useful for computing on the various files.
made_summary_table <- function(vector,filename){
  total_list <- make_total_epidemiology(vector,filename)
  lapply(c(1:length(total_list)),function(x){
    file <- names(total_list[x])
    nobs <- dim(total_list[[x]])[1]
    nvar <- dim(total_list[[x]])[2]
    year <- unique(total_list[[x]]$year)
    year_start <- min(year)
    year_end <- max(year)
    data.frame(file,nobs,nvar,year_start,year_end)
  }) %>% bind_rows
}


##### DATA
filename <- "data-raw/YB 1980-2010.xls"
vector <- c("data-raw/Nien giam 2011r.xls","data-raw/Nien giam 2012r.xls","data-raw/Nien giam 2013r.xls",
  "data-raw/Nien giam 2014r.xls","data-raw/Nien giam 2015r.xls")

demography <- make_total_demography(vector,filename) %>% filter(duplicated(.)==FALSE)
epidemiology_summary <- made_summary_table(vector,filename)
devtools::use_data(epidemiology_summary,overwrite=TRUE)
epidemiology <- make_total_epidemiology(vector,filename)
list2env(epidemiology,environment())



list_data <- grep(paste(names(epidemiology),collapse="|"),ls(),value=T)
for(I in 1:length(list_data)){
  save(list=list_data[I],
    file=paste("data/",list_data[I],".rda",sep=""),
    envir = .GlobalEnv)
}

