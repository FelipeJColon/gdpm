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
  sel <- purrr::map(splits_lst, 3) > as.Date(from) &
    purrr::map(splits_lst, 3) <= as.Date(to)
  lst <- splits_lst[sel] %>% purrr::sort_by(3)
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
    summarise(value = sum(value, na.rm=TRUE)) %>%
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
#' Gets Infectious Diseases
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
#' @param disease An epidemiological data frame (e.g. \code{ili} or
#' \code{dengue}). Should contain at least the variables \code{province},
#' \code{year}, \code{month} and the variables \code{incidence} and
#' \code{mortality}.
#' @param from Initial date of the time range, of the class \code{Date}, by
#' default: the 1st January of 2004.
#' @param to Final date of the data, of the class \code{Date}, by default :
#' the 31st December of 2015.
#' @return A object of the same class as \code{df} in which all the provinces
#' that needed to be merged (according to the time range provided) are merged.
#' @examples
#' library(gdpm)
#'
#' # Load a resume table of all the epidemiological data frame contains in the
#' # package.
#' diseases
#' # Return a data frame in which all the provinces that needed to be merged
#' # (according to the time range) are merged.
#' gdpm_("dengue", "1990-01-01", "2004-01-01")
#' gdpm(hepatitis, 1980 ,2009)
#'
#' # By default, the time range selected is the time range of the disease
#' selected.
#' gdpm_("cholera") # the time range will be from 1980 to 2015 (include)
#' gdpm(dengue) # the time range will be from 1980 to 2010 (include)
#'
#' # For the time range, just the year can be entered as parameters and by
#' # default, the first day of the year "from" and the last day of the year
#' # "to" will be selected as time range.
#' gdpm_("dengue", "1990", "2004")
#' gdpm(hepatitis, "1980" , "2009")
#' @export
gdpm_ <- function(disease,
  from = range(disease$year)[1],
  to = range(disease$year)[2]) {
  # get disease data frame
  disease <- get(disease)

  # get from and to in the right format
  from %<>% paste0("-01-01") %>% as.Date
  to %<>% paste0("-12-31") %>% as.Date

  # test which split history should be selected
  test <- filter(disease, year == 1990)$province %>% unique() %>% length()
  ifelse (test == 40,
    df <- suppressWarnings(merge_time_serie(ah_splits, disease, from, to)),
    df <- suppressWarnings(merge_time_serie(splits, disease, from, to)))

  # arrange and return the data frame with the good format for all the data
  df %<>%
    ungroup %>%
    mutate(year = as.integer(year)) %>%
    arrange(province, year, month) %>%
    as.data.frame() %>%
    select(province, year, month, incidence, mortality)

  # test time range
  if(
    from < range(disease$year)  %>%
      min  %>%
      paste0("-01-01")  %>%
      as.Date()){
    warning(paste0('The argument "from" is out of the time range for this
      disease: ', range(disease$year) %>% paste(collapse = "-") ,
      '. The closest time range was selected: ', range(df$year) %>% paste(
        collapse = "-"),'.'))
  } else if(
    to > range(disease$year)  %>%
      max  %>%
      paste0("-12-31")  %>%
      as.Date()){
    warning(paste0('The argument "to" is out of the time range for this
      disease: ', range(disease$year) %>% paste(collapse = "-"),
      '. The closest time range was selected: ', range(df$year) %>% paste(
        collapse = "-"),'.'))
  }
  return(df)
}

#' @rdname gdpm_
#' @export
gdpm <- function(disease, from = 2004, to = 2005) {
  disease <- deparse(substitute(disease)) %>%
    gdpm_(from, to)
  return(disease)
}


