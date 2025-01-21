testthat::test_that("test calc_pSurvival", {

  ilogit_b <- boot::inv.logit
  logit_b  <- boot::logit


  #Test 1: Bird did not survive (bmi == 0)
  testthat::expect_equal(
    calc_pSurvival(sp = "KI", bmi = 0, bm = 65, basesurv = 0.8, beta = 0.1),
    0
  )

  # Test 2: Bird's body mass index is NA
  testthat::expect_equal(
    calc_pSurvival(sp = "KI", bmi = NA, bm = 65, basesurv = 0.8, beta = 0.1),
    0
  )

  # Test 3:  for "KI" species
  with_my_funct <- calc_pSurvival(sp = "KI", bmi = 70, bm = 65, basesurv = 0.8, beta = 0.1)
  expected <- ilogit_b(logit_b(0.8) + ((70 - 65) * 0.1))
  testthat::expect_equal(with_my_funct, expected)

  # Test for species other than "KI"
  with_my_funct <- calc_pSurvival(sp = "GU", bmi = 70, bm = 65, basesurv = 0.8, beta = 0.1, sd = 50)
  expected <- ilogit_b(logit_b(0.8) + ((70 - 65) / 50) * 0.1)
  testthat::expect_equal(with_my_funct, expected)

})
