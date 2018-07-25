library(magrittr) # for the " %>% " pipe

context("`getid` return a data frame for each disease available")

test_that("`getid` returns the right number of column for all diseases", {

  col_num <- gdpm::diseases$disease %>%
    paste(sep = ", ") %>%
    getid_() %>%
    ncol

  # the final table contains the incidence and the mortality for all the
  # diseases and also the province, year and month corresponding. So the number
  # of disease should be equal to the number of columns minus 3 (columns
  # province, year and month) divided by 2 (each diseases has 2 columns:
  # incidence and mortality)
  expect_equal((col_num - 3)/2, 29)

})

