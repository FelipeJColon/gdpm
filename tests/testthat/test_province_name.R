library(magrittr) # for the " %>% " pipe
library(dplyr) # for "bind_rows", "rename", "mutate", "select"
library(tidyr) # for "gather", "separate", "spread"
context("data provinces name")

test_that("correct names of provinces", {
  province_name_ye <- function(df, ye) {
    df %>%
      filter(year == ye) %>%
      select(province) %>%
      ungroup %>%
      unlist %>%
      unique %>%
      sort
  }

  expect_equal(
    province_name_ye(chickenpox, "1980"),
    c("An Giang", "Ba Ria - Vung Tau", "Bac Thai", "Ben Tre",  "Binh Tri Thien",
      "Cao Bang", "Cuu Long", "Dack Lak", "Dong Nai", "Dong Thap",
      "Gia Lai - Kon Tum", "Ha Bac", "Ha Nam Ninh", "Ha Noi", "Ha Son Binh",
      "Ha Tuyen", "Hai Hung", "Hai Phong", "Hau Giang", "Ho Chi Minh",
      "Hoang Lien Son", "Kien Giang", "Lai Chau", "Lam Dong", "Lang Son",
      "Long An",  "Minh Hai", "Nghe Tinh", "Nghia Binh", "Phu Khanh",
      "Quang Nam - Da Nang", "Quang Ninh", "Son La", "Song Be", "Tay Ninh",
      "Thai Binh", "Thanh Hoa", "Thuan Hai", "Tien Giang", "Vinh Phu"))

  expect_equal(
    province_name_ye(chickenpox, "1990"),
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
    province_name_ye(chickenpox, "1991"),
    c("An Giang", "Ba Ria - Vung Tau", "Bac Thai", "Ben Tre",  "Binh Dinh",
      "Cao Bang", "Cuu Long", "Dack Lak", "Dong Nai", "Dong Thap",
      "Gia Lai - Kon Tum", "Ha Bac", "Ha Nam Ninh", "Ha Noi", "Ha Son Binh",
      "Ha Tinh", "Ha Tuyen", "Hai Hung", "Hai Phong", "Hau Giang",
      "Ho Chi Minh", "Hoang Lien Son", "Khanh Hoa", "Kien Giang", "Lai Chau",
      "Lam Dong", "Lang Son", "Long An",  "Minh Hai", "Nghe An", "Phu Yen",
      "Quang Binh", "Quang Nam - Da Nang", "Quang Ngai", "Quang Ninh",
      "Quang Tri", "Son La", "Song Be", "Tay Ninh", "Thai Binh", "Thanh Hoa",
      "Thua Thien - Hue", "Thuan Hai", "Tien Giang", "Vinh Phu"))

  expect_equal(
    province_name_ye(chickenpox, "1992"),
    c("An Giang", "Ba Ria - Vung Tau", "Bac Thai", "Ben Tre",  "Binh Dinh",
      "Binh Thuan", "Can Tho", "Cao Bang", "Dack Lak", "Dong Nai", "Dong Thap",
      "Gia Lai", "Ha Bac", "Ha Giang", "Ha Noi", "Ha Tay", "Ha Tinh",
      "Hai Hung", "Hai Phong", "Ho Chi Minh", "Hoa Binh", "Khanh Hoa",
      "Kien Giang", "Kon Tum", "Lai Chau", "Lam Dong", "Lang Son", "Lao Cai",
      "Long An",  "Minh Hai", "Nam Ha", "Nghe An", "Ninh Binh", "Ninh Thuan",
      "Phu Yen", "Quang Binh", "Quang Nam - Da Nang", "Quang Ngai",
      "Quang Ninh", "Quang Tri", "Soc Trang", "Son La", "Song Be", "Tay Ninh",
      "Thai Binh", "Thanh Hoa", "Thua Thien - Hue", "Tien Giang", "Tra Vinh",
      "Tuyen Quang", "Vinh Long", "Vinh Phu", "Yen Bai"))

  expect_equal(
    province_name_ye(chickenpox, "1997"),
    c("An Giang", "Ba Ria - Vung Tau", "Bac Giang", "Bac Kan", "Bac Lieu",
      "Bac Ninh", "Ben Tre",  "Binh Dinh", "Binh Duong", "Binh Phuoc",
      "Binh Thuan", "Ca Mau", "Can Tho", "Cao Bang", "Da Nang", "Dack Lak",
      "Dong Nai", "Dong Thap", "Gia Lai", "Ha Giang", "Ha Nam", "Ha Noi",
      "Ha Tay", "Ha Tinh", "Hai Duong", "Hai Phong", "Ho Chi Minh", "Hoa Binh",
      "Hung Yen", "Khanh Hoa", "Kien Giang", "Kon Tum", "Lai Chau", "Lam Dong",
      "Lang Son", "Lao Cai", "Long An", "Nam Dinh", "Nghe An", "Ninh Binh",
      "Ninh Thuan", "Phu Tho", "Phu Yen", "Quang Binh", "Quang Nam",
      "Quang Ngai", "Quang Ninh", "Quang Tri", "Soc Trang", "Son La",
      "Tay Ninh", "Thai Binh", "Thai Nguyen", "Thanh Hoa", "Thua Thien - Hue",
      "Tien Giang", "Tra Vinh", "Tuyen Quang", "Vinh Long", "Vinh Phuc",
      "Yen Bai"))

  expect_equal(
    province_name_ye(chickenpox, "2004"),
    c("An Giang", "Ba Ria - Vung Tau", "Bac Giang", "Bac Kan", "Bac Lieu",
      "Bac Ninh", "Ben Tre",  "Binh Dinh", "Binh Duong", "Binh Phuoc",
      "Binh Thuan", "Ca Mau", "Can Tho", "Cao Bang", "Da Nang", "Dak Lak",
      "Dak Nong", "Dien Bien", "Dong Nai", "Dong Thap", "Gia Lai", "Ha Giang",
      "Ha Nam", "Ha Noi", "Ha Tay", "Ha Tinh", "Hai Duong", "Hai Phong",
      "Hau Giang", "Ho Chi Minh", "Hoa Binh", "Hung Yen", "Khanh Hoa",
      "Kien Giang", "Kon Tum", "Lai Chau", "Lam Dong", "Lang Son", "Lao Cai",
      "Long An", "Nam Dinh", "Nghe An", "Ninh Binh", "Ninh Thuan", "Phu Tho",
      "Phu Yen", "Quang Binh", "Quang Nam", "Quang Ngai", "Quang Ninh",
      "Quang Tri", "Soc Trang", "Son La", "Tay Ninh", "Thai Binh",
      "Thai Nguyen", "Thanh Hoa", "Thua Thien - Hue", "Tien Giang", "Tra Vinh",
      "Tuyen Quang", "Vinh Long", "Vinh Phuc", "Yen Bai"))

  expect_equal(
    province_name_ye(chickenpox, "2008"),
    c("An Giang", "Ba Ria - Vung Tau", "Bac Giang", "Bac Kan", "Bac Lieu",
      "Bac Ninh", "Ben Tre",  "Binh Dinh", "Binh Duong", "Binh Phuoc",
      "Binh Thuan", "Ca Mau", "Can Tho", "Cao Bang", "Da Nang", "Dak Lak",
      "Dak Nong", "Dien Bien", "Dong Nai", "Dong Thap", "Gia Lai", "Ha Giang",
      "Ha Nam", "Ha Noi", "Ha Tinh", "Hai Duong", "Hai Phong", "Hau Giang",
      "Ho Chi Minh", "Hoa Binh", "Hung Yen", "Khanh Hoa", "Kien Giang",
      "Kon Tum", "Lai Chau", "Lam Dong", "Lang Son", "Lao Cai", "Long An",
      "Nam Dinh", "Nghe An", "Ninh Binh", "Ninh Thuan", "Phu Tho", "Phu Yen",
      "Quang Binh", "Quang Nam", "Quang Ngai", "Quang Ninh", "Quang Tri",
      "Soc Trang", "Son La", "Tay Ninh", "Thai Binh", "Thai Nguyen",
      "Thanh Hoa", "Thua Thien - Hue", "Tien Giang", "Tra Vinh", "Tuyen Quang",
      "Vinh Long", "Vinh Phuc", "Yen Bai"))
})
