
testthat::test_that("test calc_adultbmchange", {

  # Test 1:  when `alive = TRUE`
  with_my_funct <- calc_adultbmchange(alive = TRUE, BM_adult = 70, Egain_adult = 1000, Ereq_adult = 800, adult_mass_KG = 10)
  expected      <- 70 + ((1000 - 800) / 10)
  testthat::expect_equal(with_my_funct, expected)

  # Test 2:  when `alive = FALSE`
  with_my_funct <- calc_adultbmchange(alive = FALSE, BM_adult = 70, Egain_adult = 1000, Ereq_adult = 800, adult_mass_KG = 10)
  expected      <- 70

  testthat::expect_equal(with_my_funct, expected)

})
