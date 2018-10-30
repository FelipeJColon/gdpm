library(magrittr) # for the " %>% " pipe
library(dplyr) # for "bind_rows", "rename", "mutate", "select"
library(tidyr) # for "gather", "separate", "spread"
context("data provinces name")

test_that("correct names of provinces", {
  province_name_ye <- function(df, ye) {
    df %>%
      get %>%
      filter(year == ye) %>%
      select(province) %>%
      ungroup %>%
      unlist %>%
      unique %>%
      sort
  }

  expect_equal(
    province_name_ye("chickenpox", "1980"),
    dictionary::vn_province_year$`1979-1990`)

  expect_equal(
    province_name_ye("chickenpox", "1990"),
    dictionary::vn_province_year$`1990-1991`)

  expect_equal(
    province_name_ye("chickenpox", "1991"),
    dictionary::vn_province_year$`1991-1992`)

  expect_equal(
    province_name_ye("chickenpox", "1992"),
    dictionary::vn_province_year$`1992-1997`)

  expect_equal(
    province_name_ye("chickenpox", "1997"),
    dictionary::vn_province_year$`1997-2004`)

  expect_equal(
    province_name_ye("chickenpox", "2004"),
    dictionary::vn_province_year$`2004-2008`)

  expect_equal(
    province_name_ye("chickenpox", "2008"),
    dictionary::vn_province_year$`2008-2020`)
})
