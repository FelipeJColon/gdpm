library(magrittr) # for the " %>% " pipe
library(dplyr) # for "bind_rows", "rename", "mutate", "select"
library(tidyr) # for "gather", "separate", "spread"

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
    unique(getid(chickenpox, hepatitis, ili, dengue)$province),
    c("An Giang", "Ba Ria - Vung Tau", "Bac Thai", "Ben Tre",  "Binh Tri Thien",
      "Cao Bang", "Cuu Long", "Dack Lak", "Dong Nai", "Dong Thap",
      "Gia Lai - Kon Tum", "Ha Bac", "Ha Nam Ninh", "Ha Noi", "Ha Tuyen",
      "Hai Hung", "Hai Phong", "Hau Giang", "Ho Chi Minh", "Hoang Lien Son",
      "Kien Giang", "Lai Chau", "Lam Dong", "Lang Son", "Long An",  "Minh Hai",
      "Nghe Tinh", "Nghia Binh", "Phu Khanh", "Quang Nam - Da Nang",
      "Quang Ninh", "Son La", "Song Be", "Tay Ninh", "Thai Binh", "Thanh Hoa",
      "Thuan Hai", "Tien Giang", "Vinh Phu"))

  expect_equal(
    unique(getid_("chickenpox", "hepatitis", "ili", "dengue")$province),
    c("An Giang", "Ba Ria - Vung Tau", "Bac Thai", "Ben Tre",  "Binh Tri Thien",
      "Cao Bang", "Cuu Long", "Dack Lak", "Dong Nai", "Dong Thap",
      "Gia Lai - Kon Tum", "Ha Bac", "Ha Nam Ninh", "Ha Noi", "Ha Tuyen",
      "Hai Hung", "Hai Phong", "Hau Giang", "Ho Chi Minh", "Hoang Lien Son",
      "Kien Giang", "Lai Chau", "Lam Dong", "Lang Son", "Long An",  "Minh Hai",
      "Nghe Tinh", "Nghia Binh", "Phu Khanh", "Quang Nam - Da Nang",
      "Quang Ninh", "Son La", "Song Be", "Tay Ninh", "Thai Binh", "Thanh Hoa",
      "Thuan Hai", "Tien Giang", "Vinh Phu"))

  expect_equal(
    unique(getid(chickenpox, hepatitis, ili, dengue,
      shortest = TRUE)$province),
    c("An Giang", "Ba Ria - Vung Tau", "Bac Thai", "Ben Tre",  "Binh Tri Thien",
      "Cao Bang", "Cuu Long", "Dack Lak", "Dong Nai", "Dong Thap",
      "Gia Lai - Kon Tum", "Ha Bac", "Ha Nam Ninh", "Ha Noi", "Ha Tuyen",
      "Hai Hung", "Hai Phong", "Hau Giang", "Ho Chi Minh", "Hoang Lien Son",
      "Kien Giang", "Lai Chau", "Lam Dong", "Lang Son", "Long An",  "Minh Hai",
      "Nghe Tinh", "Nghia Binh", "Phu Khanh", "Quang Nam - Da Nang",
      "Quang Ninh", "Son La", "Song Be", "Tay Ninh", "Thai Binh", "Thanh Hoa",
      "Thuan Hai", "Tien Giang", "Vinh Phu"))

  expect_equal(
    unique(getid_("chickenpox", "hepatitis", "malaria", "dengue",
      shortest = TRUE)$province),
    c("An Giang", "Ba Ria - Vung Tau", "Bac Giang", "Bac Kan", "Bac Lieu",
      "Bac Ninh", "Ben Tre",  "Binh Dinh", "Binh Duong", "Binh Phuoc",
      "Binh Thuan", "Ca Mau", "Can Tho", "Cao Bang", "Da Nang", "Dack Lak",
      "Dong Nai", "Dong Thap", "Gia Lai", "Ha Giang", "Ha Nam", "Ha Noi",
      "Ha Tinh", "Hai Duong", "Hai Phong", "Ho Chi Minh", "Hoa Binh",
      "Hung Yen", "Khanh Hoa", "Kien Giang", "Kon Tum", "Lai Chau", "Lam Dong",
      "Lang Son", "Lao Cai", "Long An", "Nam Dinh", "Nghe An", "Ninh Binh",
      "Ninh Thuan", "Phu Tho", "Phu Yen", "Quang Binh", "Quang Nam",
      "Quang Ngai", "Quang Ninh", "Quang Tri", "Soc Trang", "Son La",
      "Tay Ninh", "Thai Binh", "Thai Nguyen", "Thanh Hoa", "Thua Thien - Hue",
      "Tien Giang", "Tra Vinh", "Tuyen Quang", "Vinh Long", "Vinh Phuc",
      "Yen Bai"))

  expect_equal(
    unique(getid(chickenpox, ili, dengue, shigella,
      from = "1990", to = "2002")$province),
    c("An Giang", "Ba Ria - Vung Tau", "Bac Thai", "Ben Tre",  "Binh Dinh",
      "Cao Bang", "Cuu Long", "Dack Lak", "Dong Nai", "Dong Thap",
      "Gia Lai - Kon Tum", "Ha Bac", "Ha Nam Ninh", "Ha Noi", "Ha Son Binh",
      "Ha Tuyen", "Hai Hung", "Hai Phong", "Hau Giang", "Ho Chi Minh",
      "Hoang Lien Son", "Khanh Hoa", "Kien Giang", "Lai Chau", "Lam Dong",
      "Lang Son", "Long An",  "Minh Hai", "Nghe Tinh", "Phu Yen", "Quang Binh",
      "Quang Nam - Da Nang", "Quang Ngai", "Quang Ninh", "Quang Tri", "Son La",
      "Song Be", "Tay Ninh", "Thai Binh", "Thanh Hoa", "Thua Thien - Hue",
      "Thuan Hai", "Tien Giang", "Vinh Phu"))

  expect_equal(
    unique(getid_("chickenpox", "hepatitis", "ili", "dengue",
      from = 1990, to = 2002)$province),
    c("An Giang", "Ba Ria - Vung Tau", "Bac Thai", "Ben Tre",  "Binh Tri Thien",
      "Cao Bang", "Cuu Long", "Dack Lak", "Dong Nai", "Dong Thap",
      "Gia Lai - Kon Tum", "Ha Bac", "Ha Nam Ninh", "Ha Noi", "Ha Son Binh",
      "Ha Tuyen", "Hai Hung", "Hai Phong", "Hau Giang", "Ho Chi Minh",
      "Hoang Lien Son", "Kien Giang", "Lai Chau", "Lam Dong", "Lang Son",
      "Long An",  "Minh Hai", "Nghe Tinh", "Nghia Binh", "Phu Khanh",
      "Quang Nam - Da Nang", "Quang Ninh", "Son La", "Song Be", "Tay Ninh",
      "Thai Binh", "Thanh Hoa", "Thuan Hai", "Tien Giang", "Vinh Phu"))

})



test_that("`getid` returns an error", {

  expect_warning(getid(chickenpox, ili, dengue, malaria, from = 1980,
    to = 2002))

  expect_error(getid(chickenpox, ili, dengue, malaria, from = 2028))

  expect_error(getid_("chickenpox","dengue","ili","vhf","malaria",
    shortest = TRUE))

  expect_error(getid_("vhf","mumps", from = "1990", to = "1980"))

})

