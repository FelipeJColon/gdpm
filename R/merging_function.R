#' Filter dataframe by a time range
#'
#' Filter an epidemiological data frame to keep only the data corresponding to a
#' certain time range (between \code{to} and \code{from} (include)).
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
#' Filter and order splits events by a time range
#'
#' Filter a list of splits event to keep only the event corresponding to a
#' certain time range (between \code{to} and \code{from} (include)) and order
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
#' Vectorise provinces by event
#'
#' Select the name of the provinces concern by one or multiple splits events and
#' return a list with a vector for each event and containing all the province
#' concern by this event.
#'
#'
#' @param splits_lst A list containing a list of event, each code with a slot
#' \code{combined} and a slot \code{elements}. The first one contains the name
#' of the merged provinces and the second one contains a vector of the names of
#' the provinces to merge.
#' @return A list of vector, each vector correspond to one event and contains
#' the name of the provinces concern by this event.
#' @keywords internal
#' @noRd
province_splits <- function(lst_split){
  tmp <- lapply(names(lst_split),function(x){
    combined <- purrr::map(lst_split[x],1) %>% unlist() %>% as.vector()
    elements <- purrr::map(lst_split[x],2) %>% unlist() %>% as.vector()
    province <- c(combined,elements)
  }) %>% setNames(names(lst_split))
  tmp
}

################################################################################
#' Prepare dataframe
#'
#' Apply for one event to a data frame previously filter to keep only the data of the \code{province} linked
#' to the event. Prepare a data frame who joins all the \code{incidence} and \code{mortality} data together
#' by \code{year} and \code{month} without keeping the \code{province} information.
#'
#' @param df An epidemiological data frame (e.g. \code{ili} or \code{dengue}). Should contain at
#' least the variables \code{province}, \code{year}, \code{month} and the variables
#' \code{incidence} and \code{mortality}.
#' @return A data frame with the variables:\code{year}, \code{month}, \code{incidence} and \code{mortality}
#' @keywords internal
#' @noRd
prepare_data <- function(df){
  split(df,as.factor(unique(df$province))) %>%
    purrr::map(select,-contains("province")) %>%
    purrr::reduce(dplyr::full_join,by=c("year","month"))
}

################################################################################
#' Does one merging event (only on incidence or mortality data)
#'
#' Apply for one event to a data frame previously filter to keep only the data of the \code{province} linked
#' to the event. Does one merging event only for the \code{incidence} or \code{mortality} data
#'
#' @param df An epidemiological data frame (e.g. \code{ili} or \code{dengue}). Should contain at
#' least the variables \code{year}, \code{month} and the variables \code{incidence} or \code{mortality}.
#' @param x A  character object indicating \code{"incidence"} or \code{"mortality"}
#' @return A data frame with the same variables as \code{df}
#' @keywords internal
#' @noRd
gather_sum <- function(df,x){
  df %<>%  select(contains(x),one_of(c("month","year"))) %>%
    gather(name,value,contains(x)) %>%
    select(-matches("name")) %>%
    group_by(year,month) %>%
    summarise(value=sum(value,na.rm=TRUE)) %>%
    rename_(.dots=setNames(list("value"),x))
}

################################################################################
#' Does one merging event
#'
#' Apply, for one event, to a data frame previously filter to keep only the data of the \code{province} linked
#' to the event. Does one merging event for the \code{incidence} and the \code{mortality} data
#'
#' @param df An epidemiological data frame (e.g. \code{ili} or \code{dengue}). Should contain at
#' least the variables \code{year}, \code{month} and the variables \code{incidence} or \code{mortality}.
#' @return A data frame with the same variables as \code{df}
#' @keywords internal
#' @noRd
sum_incidence_mortality <- function(df){
  lapply(c("incidence","mortality"),function(x){
    gather_sum(df,x)
  }) %>%  purrr::invoke(full_join,.)
}

################################################################################
#' Merging Ha Noi / Ha Son Binh event
#'
#' Apply only if the time range contain the split and the combine event of Ha Noi & Ha Son Binh.
#' Does a additional merging on Hanoi and Ha Son Dinh.
#'
#' @param df An epidemiological data frame (e.g. \code{ili} or \code{dengue}). Should contain at
#' least the variables \code{year}, \code{month}, \code{incidence},\code{mortality} and \code{province}
#' containing \code{"Ha Noi"} and \code{"Ha Son Binh"}
#' @return A data frame with the same variables as \code{df}
#' @keywords internal
#' @noRd
hanoi_function <- function(df){
  tmp <- split(df,df$province %in% c("Ha Noi","Ha Son Binh"))
  tmp$`TRUE` %<>%
    prepare_data %>%
    sum_incidence_mortality %>%
    mutate(province="Ha Noi")
  bind_rows(tmp$`TRUE`,tmp$`FALSE`)
}

################################################################################
#' Merges provinces
#'
#' Merges epidemiological data accordingly to a time range and by the provinces concern by a split/combined
#' event and return a data frame for the time series imputed.
#'
#' @details For two diseases: \code{"hepatitis"} and \code{"amoebiasis"}, the story of splits/combined province
#' is different as the events in 1990 for all the others diseases, take place in 1991 for these two diseases.
#' An other list(\code{ah_splits}) of event have been created for these two diseases.
#'
#' @param splits_lst A list containing a list of event, each code with a slot \code{combined} and a slot
#' \code{elements}. The first one contains the name of the merged provinces and
#' the second one contains a vector of the names of the provinces to merge.
#' @param df An epidemiological data frame (e.g. \code{ili} or \code{dengue}). Should contain at
#' least the variables \code{province}, \code{year}, \code{month} and the variables
#' \code{incidence} and \code{mortality}.
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @return A object of the same class as \code{df} in which all the provinces
#' that needed to be merged (according to the time serie) are merged.
#' @keywords internal
#' @noRd
merge_time_serie <- function(splits_lst,df,from,to){

  ## select the df between to and from.
  df %<>% select_date(from,to)
  ## select event from the more recent to the older
  lst_events <- select_events(splits_lst,from,to)

  ## if exist some split events, apply functions by event (recent to older) to sum together the splits provinces
  ## and linked them to the combined province and return a dataframe with all the time series by province for
  ## the incidence and mortality data
  ## if not return only the dataframe filter by the time serie selected.
  if (length(lst_events)>0) {
    for (i in length(lst_events):1){
      ## extract name of the province in the event
      province_lst <- province_splits(lst_events[i])
      ### apply all the functions to sum together the incidence and mortality data for all the province concern
      ### by the event and return a data frame containing the information of the split event gather in one province
      ### (instead of two or three provinces)
      tmp <- split(df, df$province %in% province_lst[[1]])
      tmp$`TRUE` %<>%
        prepare_data %>%
        sum_incidence_mortality %>%
        mutate(province=names(province_lst[1]))
      df <- bind_rows(tmp$`TRUE`,tmp$`FALSE`)
    }
  } else {df}

  ## If the time serie contain the split and the combined element of Ha Noi, apply also a complementary
  ## function who merge the two events together.
  if (from <= splits$`Ha Son Binh`$date & to >= splits$`Ha Noi`$date) {
    df %<>% hanoi_function()
  }
  return(df)
}


################################################################################

#' Merges provinces
#'
#' Merges epidemiological data accordingly to a time range and merge the provinces concern by a split/combined
#' event if necessary and return a data frame for the time series imputed.
#'
#' @param df An epidemiological data frame (e.g. \code{ili} or \code{dengue}). Should contain at
#' least the variables \code{province}, \code{year}, \code{month} and the variables
#' \code{incidence} and \code{mortality}.
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @return A object of the same class as \code{df} in which all the provinces
#' that needed to be merged (according to the time serie) are merged.
#' @examples
#' ## For a time series strating on the first of January 1980, finishing on the first of January 2004
#' and merging the provinces accordingly:
#' data(dengue)
#' data(hepatitis)
#' data(amoebiasis)
#' range(dengue$year)
#' merging(dengue,"1990-01-01","2004-01-01")
#' range(hepatitis$year)
#' merging(hepatitis,"1980-01-01","2009-01-01")
#' range(amoebiasis$year)
#' merging(amoebiasis,"1980-01-01","2009-01-01")
#' @export
### Select the good splits list to apply the merging, in function of the data frame in input.
merging <- function(df,from,to){
  test <- filter(df,year==1990)$province %>% unique() %>% length()
  ifelse (test==40, # only hepatitis and amoebiaisis have 40 provinces in 1990
    df <- merge_time_serie(ah_splits,df,from,to),
    df <- merge_time_serie(splits,df,from,to))
  return(df)
}
