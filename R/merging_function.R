#' Filters dataframe by a time range
#'
#' Filters an epidemiological data frame to keep only the data corresponding to
#' a certain time range (between \code{to} and \code{from} (exclude)).
#'
#' @param df An epidemiological data frame (e.g. \code{ili} or \code{dengue}).
#' Should contain at least the variables \code{province}, \code{year},
#' \code{month} and the variables \code{incidence} and \code{mortality}.
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @return A data frame with the same variables as \code{df}.
#' @keywords internal
#' @noRd
select_date <- function(df, from, to) {
  df %<>% mutate(date = as.Date(paste(year, as.numeric(month),
    01, sep = "-"))) %>%
    filter(date >= from, date <= to) %>%
    select(-date)
}

################################################################################
#' Filters and order splits events by a time range
#'
#' Filters a list of splits event to keep only the event corresponding to a
#' certain time range (between \code{to} and \code{from} (exclude)) and order
#' them from the older to the more recent
#'
#' @param splits_lst A list containing a list of event, each code with a slot
#' \code{combined} and a slot \code{elements}. The first one contains the name
#' of the merged provinces and the second one contains a vector of the names of
#' the provinces to merge.
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @return A list with the same variables as \code{splits_lst}.
#' @keywords internal
#' @noRd
select_events <- function(splits_lst, from, to) {
  sel0 <- purrr::map(splits_lst, 3) %>% unlist %>%
    sort(decreasing = F) %>% names()
  splits_lst <- splits_lst[sel0]
  sel <- purrr::map(splits_lst, 3) > as.Date(from) &
    purrr::map(splits_lst, 3) <= as.Date(to)
  lst <- splits_lst[sel]
}

################################################################################
#' Vectorises provinces by event
#'
#' Selects the name of the provinces concerned by one or multiple splits events
#' and returns a list with a vector for each event and containing all the
#' province names concerned by this event.
#'
#' @param splits_lst A list containing a list of event, each code with a slot
#' \code{combined} and a slot \code{elements}. The first one contains the name
#' of the merged provinces and the second one contains a vector of the names of
#' the provinces to merge.
#' @return A list of vector, each vector corresponds to one event and contains
#' the province names concerned by this event.
#' @keywords internal
#' @noRd
province_splits <- function(lst_split) {
  provinces <- lapply(names(lst_split), function(x) {
    combined <- purrr::map(lst_split[x], 1) %>% unlist() %>% as.vector()
    elements <- purrr::map(lst_split[x], 2) %>% unlist() %>% as.vector()
    province <- c(combined, elements)
  }) %>% setNames(names(lst_split))
  provinces
}

################################################################################
#' Prepares dataframe
#'
#' Applies for one event on a data frame previously filter to keep only the data
#' of the \code{province} linked to the event.
#' Prepares a data frame who joins all the \code{incidence} and \code{mortality}
#' data together by \code{year} and \code{month} without keeping the
#' \code{province} information.
#'
#' @param df An epidemiological data frame (e.g. \code{ili} or \code{dengue}).
#' Should contain at least the variables \code{province}, \code{year},
#' \code{month} and the variables \code{incidence} and \code{mortality}.
#' @return A data frame with the variables:\code{year}, \code{month},
#' \code{incidence} and \code{mortality}
#' @keywords internal
#' @noRd
prepare_data <- function(df) {
  split(df, as.factor(unique(df$province))) %>%
    purrr::map(select, -contains("province")) %>%
    purrr::reduce(full_join, by = c("year", "month"))
}

################################################################################
#' Does one merging event (only on incidence or mortality data)
#'
#' Applies for one event to a data frame previously filter to keep only the data
#' of the \code{province} linked to the event. Does one merging event only for
#' the \code{incidence} or \code{mortality} data
#'
#' @param df An epidemiological data frame (e.g. \code{ili} or \code{dengue}).
#' Should contain at least the variables \code{year}, \code{month} and the
#' variables \code{incidence} or \code{mortality}.
#' @param x A character object indicating \code{"incidence"} or
#' \code{"mortality"}
#' @return A data frame with the same variables as \code{df}
#' @keywords internal
#' @noRd
gather_sum <- function(df, x) {
  df %<>%  select(contains(x), one_of(c("month", "year"))) %>%
    gather(name, value, contains(x)) %>%
    select(-matches("name")) %>%
    group_by(year, month) %>%
    summarise(value = ifelse(mean(is.na(value)) == 1,
      sum(value), sum(value, na.rm = TRUE))) %>%
    rename_(.dots = setNames(list("value"), x))
}

################################################################################
#' Does one merging event
#'
#' Apply, for one event, to a data frame previously filter to keep only the data
#' of the \code{province} linked to the event. Does one merging event for the
#' \code{incidence} and the \code{mortality} data
#'
#' @param df An epidemiological data frame (e.g. \code{ili} or \code{dengue}).
#' Should contain at least the variables \code{year}, \code{month} and the
#' variables \code{incidence} or \code{mortality}.
#' @return A data frame with the same variables as \code{df}
#' @keywords internal
#' @noRd
sum_incidence_mortality <- function(df) {
  lapply(c("incidence","mortality"), function(x) {
    gather_sum(df, x)
  }) %>%  purrr::invoke(full_join, ., by = c("year", "month"))
}

################################################################################
#' Merging Ha Noi / Ha Son Binh event
#'
#' Applies only if the time range contains the split and the combine event of
#' Ha Noi & Ha Son Binh, does an additional merging on Hanoi and Ha Son Dinh.
#'
#' @param df An epidemiological data frame (e.g. \code{ili} or \code{dengue}).
#' Should contain at least the variables \code{year}, \code{month},
#' \code{incidence},\code{mortality} and \code{province} containing
#' \code{"Ha Noi"} and \code{"Ha Son Binh"}
#' @return A data frame with the same variables as \code{df}
#' @keywords internal
#' @noRd
hanoi_function <- function(df) {
  tab <- split(df, df$province %in% c("Ha Noi", "Ha Son Binh"))
  tab$`TRUE` %<>%
    prepare_data %>%
    sum_incidence_mortality %>%
    mutate(province = "Ha Noi")
  bind_rows(tab$`TRUE`, tab$`FALSE`)
}

################################################################################
#' Merges provinces
#'
#' Merges epidemiological data accordingly to a time range and by the provinces
#' concerned by a split/combined event and return a data frame for the time
#' range imputed.
#'
#' @details For two diseases: \code{"hepatitis"} and \code{"amoebiasis"}, the
#' story of splits/combined province is different. The merges events in 1990
#' take place in 1991. An other list (\code{ah_splits}) of event have been
#' created for these two diseases.
#'
#' @param splits_lst A list containing a list of event, each code with a slot
#' \code{combined} and a slot \code{elements}. The first one contains the name
#' of the merged provinces and the second one contains a vector of the names of
#' the provinces to merge.
#' @param df An epidemiological data frame (e.g. \code{ili} or \code{dengue}).
#' Should contain atcleast the variables \code{province}, \code{year},
#' \code{month} and the variables \code{incidence} and \code{mortality}.
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @return A object of the same class as \code{df} in which all the provinces
#' that needed to be merged (according to the time range) are merged.
#' @keywords internal
#' @noRd
merge_time_serie <- function(splits_lst, df, from, to) {
  df %<>% select_date(from, to)
  lst_events <- select_events(splits_lst, from, to)
  if (length(lst_events) > 0) {
    for (i in rev(seq_along(lst_events))) {
      province_lst <- province_splits(lst_events[i])
      tmp <- split(df, df$province %in% province_lst[[1]])
      tmp$`TRUE` %<>%
        prepare_data %>%
        sum_incidence_mortality %>%
        mutate(province = names(province_lst[1]))
      df <- bind_rows(tmp$`TRUE`, tmp$`FALSE`)
    }
  } else { df }
  if (from <= splits$`Ha Son Binh`$date & to >= splits$`Ha Noi`$date) {
    df %<>% hanoi_function()
  }
  return(df)
}

################################################################################
#' Merges Provinces in Infectious Disease (for one disease)
#'
#' @description Merges epidemiological data accordingly to a time range and
#' merges the provinces concern by a split/combined event if necessary and
#' returns a data frame for the time range imputed.
#'
#' @details A dataset called \code{diseases} contains the description of all the
#' epidemiological data frames available in this package. The dataframe is
#' ordered by the names of each disease and the time range of the data. This
#' table can be used as a resume. \cr\cr In each epidemiological data frames The
#' incidence corresponds to the number of new cases and the mortality
#' corresponds to the number of deaths. \cr \cr Before 1991, \code{<NA>}
#' corresponds to "no case" or "no report". After 1991, \code{<NA>} corresponds
#' to "no report" and \code{0} to "no case".
#'
#' @param splits_lst A list containing a list of event, each code with a slot
#' \code{combined} and a slot \code{elements}. The first one contains the name
#' of the merged provinces and the second one contains a vector of the names of
#' the provinces to merge.
#' @param disease An epidemiological data frame (e.g. \code{ili} or
#' \code{dengue}). Should contain at least the variables \code{province},
#' \code{year}, \code{month} and the variables \code{incidence} and
#' \code{mortality}.
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @return A object of the same class as \code{df} in which all the provinces
#' that needed to be merged (according to the time range provided) are merged.
#' @keywords internal
#' @noRd
merge_province <- function(splits_list, disease, from, to) {
  # get from and to in the right format
  from %<>% paste0("-01-01") %>% as.Date
  to %<>% paste0("-12-31") %>% as.Date

  df <- suppressWarnings(merge_time_serie(splits_list, disease, from, to)) %>%
    ungroup %>%
    mutate(year = as.integer(year)) %>%
    arrange(province, year, month) %>%
    as.data.frame() %>%
    select(province, year, month, incidence, mortality)

}


################################################################################
#' Selects first and last year of a list
#'
#' @description Selects the first and the last year of multiple data frames
#' contain in a list.
#'
#' @param lst a list containing one or multiple data frame containing, each one
#' of them, at least one numeric column named identically.
#' @param sel a numeric (1 or 2), to select the first (1) or the last year (2).
#' @param col column named containing the value (by default = year)
#' @return A numeric
#' @keywords internal
#' @noRd
select_min <- function(lst, sel, col = "year"){
  year <- purrr::map(lst, select, contains(col)) %>%
    purrr::map(range)  %>%
    purrr::map(sel)  %>% unlist  %>% min
  year
}

#' @rdname select_min
#' #' @keywords internal
#' @noRd
select_max <- function(lst, sel, col = "year"){
  year <- purrr::map(lst, select, contains(col)) %>%
    purrr::map(range)  %>%
    purrr::map(sel)  %>% unlist  %>% max
  year
}

################################################################################
#' Merges multiples data frame
#'
#' @description Merges multiple epidemiological data accordingly to a time range
#' and merges the provinces concern by a split/combined event if necessary and
#' returns a data frame for the time range imputed with, for each disease, one
#' column \code{incidence_disease} and one column \code{mortality_disease}
#'
#' @param vect a vector containing the name of each disease.
#' @param from Initial date of the time range, can be an object of class
#' \code{Date}, \code{character} or \code{numeric}.
#' @param to Final date of the data, of the class \code{Date}, \code{character}
#' or \code{numeric}.
#' @return A dataframe in which all the provinces
#' that needed to be merged (according to the time range provided) are merged
#' and keep all the incidence and mortality data for each disease selected.
#' @keywords internal
#' @noRd
multiple_disease <- function(lst, splits_list, from, to){
  lapply(seq_along(lst), function(x){
    df <- suppressWarnings(merge_province(splits_list, lst[[x]], from, to))
    colnames(df) <- sub("incidence",
      paste0("incidence_", names(lst)[x]), colnames(df))
    colnames(df) <- sub("mortality",
      paste0("mortality_", names(lst)[x]), colnames(df))
    df %<>% as.data.frame
  }) %>% plyr::join_all(., by = c("province", "year", "month"),
    type = "full") %>%
    arrange(province, year, month)
}

################################################################################
#' Gets Infectious Diseases
#'
#' @description Merges epidemiological data accordingly to a time range and
#' merges the provinces concern by a split/combined event if necessary and
#' returns a data frame for the time range imputed.
#'
#' @details One dataset called \code{diseases} contains the description of all
#' the epidemiological data frames available in this package. The data frame is
#' ordered by the names of each disease and the time range of the data. This
#' table can be used as a resume.
#' \cr\cr The time range can be implemented in the function by three different
#'  ways : \code{from}, \code{to} and \code{shortest}.
#'  \cr\cr In each epidemiological data frame, the incidence corresponds to the
#' number of new cases and the mortality corresponds to the number of deaths.
#' \cr \cr Before 1991, \code{<NA>} corresponds to "no case" or "no report".
#' After 1991, \code{<NA>} corresponds to "no report" and \code{0} to "no case".
#' \cr \cr For two diseases: \code{hepatitis} and \code{amoebiasis}, the
#' story of splits/combined province is different. The merges events of 1990
#' take place in 1991. Another list of event has been created for these two
#' diseases and if one or the two of them are selected (together with other
#' diseases), this story will be applied to all the disease selected.
#'
#' @param disease An epidemiological data frame (e.g. \code{ili},
#' \code{dengue} ...). Should contains at least the variables \code{province},
#' \code{year}, \code{month} and the variables \code{incidence} and
#' \code{mortality}.
#' @param ... Other(s) epidemiological data frame(s).
#' @param from Initial date of the time range, can be an object of class
#' \code{Date}, \code{character} or \code{numeric}.
#' @param to Final date of the data, of the class \code{Date}, \code{character}
#' or \code{numeric}.
#' @param shortest if \code{FALSE}, the time range selected contains all the
#' data from all the diseases (by default).
#' If \code{TRUE}: starts when all the diseases have data and ends as soon
#'  as one disease as no more available data. (selects the overlaps year)
#' @return An object of the same class as \code{df} in which all the provinces
#' that needed to be merged (according to the time range provided) are merged.
#' @examples
#'
#' # Loads a resume table of all the epidemiological data frame contains in the
#' # package.
#' diseases
#'
#' # Returns a data frame in which all the provinces that needed to be merged
#' # (according to the time range) are merged. By default, the time range
#' # selected is the one containing the data for all the diseases.
#' getid_("dengue")
#' getid(hepatitis)
#' getid_("cholera", "malaria", "ili") # time range: 1980-01-01 to 2015-12-31
#' getid(dengue, chickenpox, anthrax) # time range: 1980-01-01 to 2015-12-31
#'
#' # If shortest = TRUE, the time range selected is the one
#' # containing overlaps data per year, for example the malaria data start
#' # in 2003, the years before will not be selected. And, it is the same for
#' # dengue, the data stop at 2010, so the years after will not be selected.
#' getid_("cholera", "malaria", "ili", shortest = TRUE)
#' # time range: 2003-01-01 to 2015-12-31
#' getid(dengue, chickenpox, anthrax, shortest = TRUE)
#' # time range: 1990-01-01 to 2010-12-31
#'
#' # For the time range, the year can also be entered as parameters:
#' # As numeric or character and by default, the first day of the year "from"
#' # and the last day of the year "to" will be selected as time range.
#' getid_("dengue", "ili", "cholera", from = 1990, to = 2004)
#' getid(chickenpox, anthrax, from = "1980" , to = "2009")
#' # Or directly as Date
#' getid_("dengue", "ili", "cholera", from = "1990-01-01", to = "2004-12-31")
#' getid(chickenpox, anthrax, from = "1980-01-01" , to = "2009-12-31")
#' @export
getid_ <- function(disease, ..., from, to, shortest = FALSE) {
  # create a character vector of all the diseases names
  vect <- c(disease, list(...)) %>%
    unlist %>% as.vector() %>%
    as.character()

  lst_disease <- mget(vect, inherits = TRUE)

  # define the value of the time range and test for all mistakes
  if(missing(from) & shortest == FALSE){from = select_min(lst_disease, 1)}
  if(missing(to) & shortest == FALSE){to = select_max(lst_disease, 2)}
  if(missing(from) & shortest == TRUE){from = select_max(lst_disease, 1)}
  if(missing(to) & shortest == TRUE){to = select_min(lst_disease, 2)}

  if(from > to |
      from > select_max(lst_disease, 2)){
    stop("The time range selected is out of bound or incorrect: ", from, "-",
      to, ". The widest time range for this selection is: ", select_min(
        lst_disease, 1), "-", select_max(lst_disease, 2), ". Maybe, try another"
," 'shortest' option or enter a different value for the parameters 'from'",
" and/or 'to'.", call. = FALSE)
  }

  # test for one disease in a list which can be out of range
  test <- purrr::map(lst_disease, select, contains("year"))  %>%
    purrr::map(range)
  if( mean(to < test  %>%
      purrr::map(1) %>%
      unlist %>%
      as.vector()) > 0){
    name_error <- names(which(purrr::map(test, 1) > to))
    sel <- grep(paste(name_error, collapse = "|"),
      names(lst_disease), invert = T, value = T)
    lst_disease <- lst_disease[sel]
  }

  # test which split history should be selected
  if (grep("hepatitis|amoebiasis", vect) %>% length >= 1){
    diseases <- multiple_disease(lst_disease, ah_splits, from, to)
  } else {
    diseases <- multiple_disease(lst_disease, splits, from, to)
  }

  # if one disease or two diseases are out of range
  if( mean(to < test  %>%
      purrr::map(1) %>%
      unlist %>%
      as.vector()) > 0){
    diseases <- suppressMessages(lapply(seq_along(name_error), function(x){
      diseases[,paste0("incidence","_",name_error[x])] <- NA
      diseases[,paste0("mortality","_",name_error[x])] <- NA
      diseases
    }) %>% plyr::join_all(.))
    warning(paste0('One or more diseases is/are out of the time range selected:
      ', paste(name_error, collapse = ", "), ', time range associated:',
paste(test[name_error], collapse = ", "), '. NAs
      were introducted.'))
  }

   # warnings message if error on the time range
  if( !missing (from) || !missing(to)){
    if(from < select_min(lst_disease, 1)) {
      warning(paste0('The argument "from" is out of the time range for this
        (these) disease(s): ', paste0(select_min(lst_disease, 1), "-",
          select_max(lst_disease, 2) ), '. The closest time range was selected:
        ', range(diseases$year) %>% paste(collapse = "-"),'.'))
    } else if (to > select_max(lst_disease, 2)) {
      warning(paste0('The argument "to" is out of the time range for this
        (these) disease(s): ', paste0(select_min(lst_disease, 1), "-",
          select_max(lst_disease, 2) ), '. The closest time range was selected:
        ', range(diseases$year) %>% paste(collapse = "-"),'.'))
    }
  }
  return(diseases)
}

#' @rdname getid_
#' @export
getid <- function(disease, ..., from, to, shortest = FALSE){
  disease <- deparse(substitute(disease))
  vect <- as.character(substitute(list(...))) %>%
    grep("list", ., invert = T, value = T)

  if (shortest == TRUE){
    diseases <- getid_(disease, vect, from = from, to = to, shortest = TRUE)
  } else {
    diseases <- getid_(disease, vect, from = from, to = to)
  }

  return(diseases)
}



