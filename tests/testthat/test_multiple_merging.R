library(magrittr) # for the " %>% " pipe
library(dplyr) # for "bind_rows", "rename", "mutate", "select"
library(tidyr) # for "gather", "separate", "spread"
library(dictionary) # for "provinces_year"

context("`getid` return the good the time range and provinces when provide
  multiple diseases at the same time")

test_that("`getid` returns the correct time range", {

  expect_equal(
    range(getid(chickenpox, hepatitis, ili, dengue)$year),
    c(1980, 2015))

  expect_equal(
    range(getid_("chickenpox", "hepatitis", "ili", "dengue")$year),
    c(1980, 2015))

  expect_equal(
    range(getid(chickenpox, hepatitis, ili, dengue, shortest = TRUE)$year),
    c(1980, 2010))

  expect_equal(
    range(getid_("chickenpox", "hepatitis", "malaria", "dengue",
      shortest = TRUE)$year),
    c(2003, 2010))


  expect_equal(
    range(getid(chickenpox, shigella, ili, dengue,
      from = "1980", to = "2002")$year),
    c(1980, 2002))

  expect_equal(
    range(getid_("chickenpox", "hepatitis", "ili", "dengue",
      from = 2000)$year),
    c(2000, 2015))

  expect_equal(
    range(getid_("chickenpox", "hepatitis", "ili", "dengue",
      to = 2000)$year),
    c(1980, 2000))
})


test_that("`getid` returns the correct province name", {

  expect_equal(
    unique(getid(chickenpox, hepatitis, ili, dengue)$province) %>%
      c("Ha Son Binh") %>% sort,
    dictionary::province_year$`1979`)

  expect_equal(
    unique(getid_("chickenpox", "hepatitis", "ili", "dengue")$province) %>%
      c("Ha Son Binh") %>% sort,
    dictionary::province_year$`1979`)

  expect_equal(
    unique(getid(chickenpox, hepatitis, ili, dengue,
      shortest = TRUE)$province) %>%
        c("Ha Son Binh") %>% sort,
      dictionary::province_year$`1979`)

  expect_equal(
    unique(getid_("chickenpox", "hepatitis", "malaria", "dengue",
      shortest = TRUE)$province) %>%
      c("Ha Tay") %>% sort,
    dictionary::province_year$`1997`)

  expect_equal(
    unique(getid(chickenpox, ili, dengue, shigella,
      from = "1990", to = "2002")$province),
    dictionary::province_year$`1990`)

  expect_equal(
    unique(getid_("chickenpox", "hepatitis", "ili", "dengue",
      from = 1990, to = 2002)$province),
    dictionary::province_year$`1979`)

})



test_that("`getid` returns an error", {

  expect_warning(getid(chickenpox, ili, dengue, malaria, from = 1980,
    to = 2002))

  expect_error(getid(chickenpox, ili, dengue, malaria, from = 2028))

  expect_error(getid_("chickenpox","dengue","ili","vhf","malaria",
    shortest = TRUE))

  expect_error(getid_("vhf","mumps", from = "1990", to = "1980"))

})

