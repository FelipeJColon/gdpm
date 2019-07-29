library(dictionary) # for the "vn_admin1_year"

context("`getid` merges provinces accordingly to the time range")

test_that("`getid` returns the correct incidence data", {
  merging_incidence <- function(df, ye1, ye2, prov, x = "incidence") {
    df <- getid_(df, from = ye1, to = ye2)
    df <- df[which(df$province == prov), ]
    df <- df[order(df$year, df$month), ]
    df <- df[, grep(x, colnames(df), value = TRUE), drop = TRUE]
  }

  expect_equal(
    merging_incidence("chickenpox", "2000-01-01", "2000-12-01", "Cao Bang"),
    c(0, 21, 0, 0, 0, 7, 0, 0, 1, 0, 0, 0))

  expect_equal(
    merging_incidence("chickenpox", "1991-01-01", "1992-12-01", "Ha Nam Ninh"),
    c(75, 0, 36, 0, 57, 69, 122, 88, 98, 119, 65, 27, 21, 18, 62, 59, 74, 89,
      144, 67, 59, 37, 22, 24))

  expect_equal(
    merging_incidence("chickenpox", "1996-01-01", "1997-12-01", "Nam Ha"),
    c(17, 24, 57, 8, 0, 6, 7, 19, 17, 12, 39, 5, 67, 18, 35, 21, 12, 13, 31, 29,
      20, 6, 22, 10))

  expect_equal(
    merging_incidence("chickenpox", "1991-01-01", "1997-12-01", "Ha Nam Ninh"),
    c(75, 0, 36, 0, 57, 69, 122, 88, 98, 119, 65, 27, 21, 18, 62, 59, 74, 89,
      144, 67, 59, 37, 22, 24, 17, 11, 52, 9, 34, 78, 142, 141, 131, 39, 13, 23,
      82, 15, 69, 65, 54, 60, 83, 56, 21, 15, 15, 1, 7, 17, 45, 51, 31, 36, 69,
      38, 35, 27, 16, 21, 22, 38, 57, 12, 6, 6, 26, 27, 17, 12, 39, 6, 70, 18,
      39, 21, 18, 20, 33, 35, 25, 6, 23, 10))

  expect_equal(
    merging_incidence("chickenpox", "1989-01-01", "1990-12-01",
      "Binh Tri Thien"),
    c(8, 6, 4, 25, NA, NA, NA, 17, 30, 1, 3, NA, 10, 6, 3, 8, 4, 2, NA, NA, NA,
      NA, NA, 13))

  expect_equal(
    merging_incidence("chickenpox", "2007-01-01", "2008-12-01", "Ha Noi"),
    c(273, 1315, 1569, 674, 461, 347, 244, 227, 51, 52, 143, 192, 680, 653,
      1260, 988, 459, 406, 289, 84, 714, 106, 89, 85))

  expect_equal(
    merging_incidence("chickenpox", "1991-01-01", "2008-12-01", "Ha Noi"),
    c(164, 52, 107, 132, 162, 106, 58, 111, 56, 120, 30, 22, 57, 119, 134, 117,
      70, 53, 32, 69, 66, 23, 21, 44, 42, 34, 39, 102, 134, 107, 38, 97, 91, 46,
      33, 27, 67, 96, 130, 72, 46, 64, 44, 16, 19, 18, 51, 35, 20, 39, 38, 53,
      21, 46, 57, 49, 23, 29, 18, 40, 62, 168, 92, 87, 102, 59, 92, 103, 490,
      284, 58, 31, 36, 40, 61, 69, 22, 109, 14, 12, 28,  9,  5, 29, 58, 32, 30,
      33,  7, 68, 39, 28, 18, 24, 19, 17, 68, 98, 79, 93, 65, 39, 31, 28, 15,
      20, 18, 14, 14, 43,  3, 113, 16, 39, 24, 35, 19, 15, 24, 37, 102, 99, 205,
      91, 57, 54, 57, 40, 33, 14, 21, 39, 30, 43, 193, 23, 62, 59, 17, 17, 29,
      43, 46, 28, 93, 62, 11, 13, 74, 114, 61, 70, 34, 71, 31, 108, 204, 579,
      995, 844, 424, 411, 235, 158, 134, 79, 184, 140, 186, 79, 235, 406, 343,
      268, 167, 168, 261, 82, 97, 235, 146, 511, 644, 657, 304, 236, 127, 115,
      100, 81, 56, 109, 287, 1317, 1615, 758, 472, 393, 272, 242, 63, 63, 155,
      267, 773, 668, 1304, 1152, 545, 505, 307, 102, 725, 111, 99, 105))

  expect_equal(
    merging_incidence("hepatitis", "1990-01-01", "1991-12-01",
                      "Binh Tri Thien"),
    c(74, 30, 14, 19, 21, 18, 9, 30, 91, 25, 81, 19, 12, 6, 27, 77, 77, 7, 57,
      35, 38, 49, 99, 43))
})


test_that("`getid` returns the good number and names of provinces", {
  merging_province <- function(df, ye1, ye2) {
    df <- getid_(df, from = ye1,  to = ye2)
    vect <- df[, "province", drop = TRUE]
    sort(unique(vect))
  }

  expect_length(
    merging_province("chickenpox", "1980-01-01", "2015-12-31"), 39)

  expect_identical(
    sort(c("Ha Son Binh",
           merging_province("chickenpox", "1980-01-01", "2015-12-31"))),
    dictionary::vn_admin1_year$`1979-1990`)

  expect_length(
    merging_province("chickenpox", "1990-01-01", "2015-12-31"), 43)

  expect_identical(
    sort(c("Ha Son Binh",
           merging_province("chickenpox", "1990-01-01", "2015-12-31"))),
    dictionary::vn_admin1_year$`1990-1991`)

  expect_length(
    merging_province("chickenpox", "1991-01-01", "2015-12-31"), 44)

  expect_identical(
    sort(c("Ha Son Binh",
           merging_province("chickenpox", "1991-01-01", "2015-12-31"))),
    dictionary::vn_admin1_year$`1991-1992`)

  expect_length(
    merging_province("chickenpox", "1992-01-01", "2015-12-31"), 52)

  expect_identical(
    sort(c("Ha Tay",
           merging_province("chickenpox", "1992-01-01", "2015-12-31"))),
    dictionary::vn_admin1_year$`1992-1997`)

  expect_length(
    merging_province("chickenpox", "1997-01-01", "2003-12-31"), 61)

  expect_identical(
    merging_province("chickenpox", "1997-01-01", "2003-12-31"),
    dictionary::vn_admin1_year$`1997-2004`)

  expect_length(
    merging_province("chickenpox", "2004-01-01", "2007-12-31"), 64)

  expect_identical(
    merging_province("chickenpox", "2004-01-01", "2007-12-31"),
    dictionary::vn_admin1_year$`2004-2008`)

  expect_length(
    merging_province("chickenpox", "2008-01-01", "2015-12-31"), 63)

  expect_identical(
    merging_province("chickenpox", "2008", "2015"),
    dictionary::vn_admin1_year$`2008-2020`)

  expect_length(
    merging_province("hepatitis", "1980", "2015"), 39)

  expect_length(
    merging_province("hepatitis", "1990", "2015"), 39)

  expect_identical(
    sort(c("Ha Son Binh", merging_province("hepatitis", "1990", "2015"))),
    dictionary::vn_admin1_year$`1979-1990`)

  expect_length(
    merging_province("hepatitis", "1991", "2015"), 44)

  expect_identical(
    sort(c("Ha Son Binh", merging_province("hepatitis", "1991", "2015"))),
    dictionary::vn_admin1_year$`1991-1992`)

  expect_length(
    merging_province("hepatitis", "1992", "2015"), 52)
})


test_that("correct names of columns", {
  expect_identical(names(getid(rabies)),
                   c("province", "year", "month", "vaccinated_rabies",
                     "incidence_rabies"))

})
