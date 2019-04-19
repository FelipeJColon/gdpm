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
  df <- transform(df, date = as.Date(paste(df$year, as.numeric(df$month),
                                        01, sep = "-")))
  df <- subset(df, c(date >= from & date <= to))
  df <- df[, -which(names(df) %in% "date")]
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
  sel0 <- lapply(splits_lst, "[[", "date")
  sel0 <- sort(unlist(sel0), decreasing = F)
  sel0 <- names(sel0)
  splits_lst <- splits_lst[sel0]
  sel <- lapply(splits_lst, "[[", "date")
  sel <- lapply(sel, function(x) x > as.Date(from) & x <= as.Date(to))
  splits_lst[unlist(sel)]
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
#' @importFrom stats setNames
#' @noRd
province_splits <- function(lst_split) {
  provinces <- lapply(names(lst_split), function(x) {
    combined <- unlist(lapply(lst_split[x], "[[", "combined"))
    combined <- as.vector(combined)
    elements <- unlist(lapply(lst_split[x], "[[", "elements"))
    elements <- as.vector(elements)
    c(combined, elements)
  })
  provinces <- setNames(provinces, names(lst_split))
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
  lst_df <- split(df, as.factor(df$province))
  lst_df <- lapply(lst_df, function(x) x[, -which(names(x) %in% "province")])
  df <- Reduce(rbind, lst_df)
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
  tmp <- df[, c(x, "month", "year")]
  colnames(tmp)[which(names(tmp) == x)] <- "value"
  tmp <- aggregate(value ~ ., data = tmp,
                   FUN = function(y) {
                     ifelse(mean(is.na(y)) == 1, sum(y), sum(y, na.rm = TRUE))
                     },
                   na.action = na.pass)
  colnames(tmp)[which(names(tmp) == "value")] <- x
  tmp
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
#' @return A data frame with the same
#'
#' variables as \code{df}
#' @importFrom stats na.pass aggregate
#' @keywords internal
#' @noRd
sum_incidence_mortality <- function(df) {
  dft <- lapply(c("incidence", "mortality"), function(x) {
    gather_sum(df, x)
  })
  df <- merge(dft[[1]], dft[[2]], by = c("year", "month"))
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
  tab$`TRUE` <- prepare_data(tab$`TRUE`)
  tab$`TRUE` <- sum_incidence_mortality(tab$`TRUE`)
  tab$`TRUE` <- transform(tab$`TRUE`, province = "Ha Noi")
  rbind(tab$`TRUE`, tab$`FALSE`)
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
  df <- select_date(df, from, to)
  lst_events <- select_events(splits_lst, from, to)
  if (length(lst_events) > 0) {
    for (i in rev(seq_along(lst_events))) {
      province_lst <- province_splits(lst_events[i])
      tmp <- split(df, df$province %in% province_lst[[1]])
      tmp$`TRUE` <- prepare_data(tmp$`TRUE`)
      tmp$`TRUE` <- sum_incidence_mortality(tmp$`TRUE`)
      tmp$`TRUE` <- transform(tmp$`TRUE`, province = names(province_lst[1]))
      df <- rbind(tmp$`TRUE`, tmp$`FALSE`)
    }
  } else {
    df
    }
  if (from <= splits$`Ha Son Binh`$date & to >= splits$`Ha Noi`$date) {
    df <- hanoi_function(df)
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
  from <- as.Date(paste0(from, "-01-01"))
  to <- as.Date(paste0(to, "-12-31"))

  df <- merge_time_serie(splits_list, disease, from, to)
  df <-  transform(df, year = as.integer(df$year))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <-  df[, c("province", "month", "year", "incidence", "mortality")]
  df$province <- as.character(df$province)
  df[order(df$province, df$year, df$month), ]
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
  year <- lapply(lst, "[[", col)
  year <- lapply(year, range)
  year <- lapply(year, "[[", sel)
  year <- min(unlist(year))
}

#' @rdname select_min
#' #' @keywords internal
#' @noRd
select_max <- function(lst, sel, col = "year"){
  year <- lapply(lst, "[[", col)
  year <- lapply(year, range)
  year <- lapply(year, "[[", sel)
  year <- max(unlist(year))
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
  dftot <- lapply(seq_along(lst), function(x){
    df <- suppressWarnings(merge_province(splits_list, lst[[x]], from, to))
    colnames(df) <- sub("incidence", paste0("incidence_", names(lst)[x]),
                        colnames(df))
    colnames(df) <- sub("mortality", paste0("mortality_", names(lst)[x]),
                        colnames(df))
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  })
  dftot <- Reduce(merge, dftot)
  dftot[order(dftot$province, dftot$year, dftot$month), ]
}

################################################################################
#' Gets Infectious Diseases
#'
#' @description Merges epidemiological data accordingly to a time range and
#' merges the provinces concerned by a split/combined event if necessary and
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
#' @param ... Name of one or multople disease (e.g. \code{ili},
#' \code{dengue} ...).
#' @param from Initial date of the time range, can be an object of class
#' \code{Date}, \code{character} or \code{numeric}.
#' @param to Final date of the data, of the class \code{Date}, \code{character}
#' or \code{numeric}.
#' @param shortest if \code{FALSE}, the time range selected contains all the
#' data from all the diseases (by default).
#' If \code{TRUE}: starts when all the diseases have data and ends as soon
#'  as one disease as no more available data. (selects the overlaps year)
#'
#' @return An object of the class \code{data frame} in which all the provinces
#' that needed to be merged (according to the time range provided) are merged
#' together and contains also the variables \code{year}, \code{month} and
#' \code{incidence} and \code{mortality} for each \code{disease} asked.
#' @examples
#'
#' # Loads the resume table of all the epidemiological data frame contains in
#' # the package.
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
getid_ <- function(..., from, to, shortest = FALSE) {
  # create a character vector of all the diseases names
  vect <- unlist(list(...))
  vect <- as.vector(vect)
  vect <- as.character(vect)
  lst_disease <- mget(vect, inherits = TRUE)

  # define the value of the time range and test for all mistakes
  if (missing(from) & shortest == FALSE) {
    from <-  select_min(lst_disease, 1)
    }
  if (missing(to) & shortest == FALSE) {
    to <-  select_max(lst_disease, 2)
    }
  if (missing(from) & shortest == TRUE) {
    from <-  select_max(lst_disease, 1)
    }
  if (missing(to) & shortest == TRUE) {
    to <-  select_min(lst_disease, 2)
    }

  if (from > to |
      from > select_max(lst_disease, 2)) {
    stop("The time range selected is out of bound or incorrect: ", from, "-",
      to, ". The widest time range for this selection is: ", select_min(
        lst_disease, 1), "-", select_max(lst_disease, 2), ". Maybe, try another"
, " 'shortest' option or enter a different value for the parameters 'from'",
" and/or 'to'.", call. = FALSE)
  }

  # test for one disease in a list which can be out of range
  test <- lapply(lst_disease, "[[", "year")
  test <- lapply(test, range)
  if (mean(as.vector(unlist(lapply(test, "[[", 1))) > to) > 0) {
    name_error <- names(which(lapply(test, "[[", 1) > to))
    sel <- grep(paste(name_error, collapse = "|"),
      names(lst_disease), invert = T, value = T)
    lst_disease <- lst_disease[sel]
  }

  # test which split history should be selected
  if (length(grep("hepatitis|amoebiasis", vect)) >= 1){
    diseases <- multiple_disease(lst_disease, ah_splits, from, to)
  } else {
    diseases <- multiple_disease(lst_disease, splits, from, to)
  }

  # if one disease or two diseases are out of range
  if (mean(as.vector(unlist(lapply(test, "[[", 1))) > to) > 0) {
    diseases <- lapply(seq_along(name_error), function(x) {
      diseases[, paste0("incidence", "_", name_error[x])] <- NA
      diseases[, paste0("mortality", "_", name_error[x])] <- NA
      diseases
    })
    diseases <- Reduce(merge, diseases)
    warning(paste0("One or more diseases is/are out of the time range selected:
      ", paste(name_error, collapse = ", "), ", time range associated:",
paste(test[name_error], collapse = ", "), ". NAs
      were introducted."))
  }

   # warnings message if error on the year time range
  if ( !missing (from) || !missing(to)) {
    if (as.numeric(substr(from, 1, 4)) < select_min(lst_disease, 1)) {
      warning(paste0('The argument "from" is out of the time range for this
        (these) disease(s): ', paste0(select_min(lst_disease, 1), "-",
          select_max(lst_disease, 2) ), ". The closest time range was selected:
        ", paste(range(diseases$year), collapse = "-"), "."))
    } else if (as.numeric(substr(to, 1, 4)) > select_max(lst_disease, 2)) {
      warning(paste0('The argument "to" is out of the time range for this
        (these) disease(s): ', paste0(select_min(lst_disease, 1), "-",
          select_max(lst_disease, 2) ), ". The closest time range was selected:
        ",  paste(range(diseases$year), collapse = "-"), "."))
    }
  }
  return(diseases)
}

#' @rdname getid_
#' @export
getid <- function(..., from, to, shortest = FALSE){
  vect <- as.character(substitute(list(...)))
  vect <- grep("list", vect, invert = T, value = T)
  if (shortest == TRUE){
    diseases <- getid_(vect, from = from, to = to, shortest = TRUE)
  } else {
    diseases <- getid_(vect, from = from, to = to)
  }
  return(diseases)
}
