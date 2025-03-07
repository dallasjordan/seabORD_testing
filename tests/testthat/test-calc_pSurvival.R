testthat::test_that("test calc_pSurvival", {

  load("~/repositories/SeabORD/seabORD/local/test_values_v2.rdata")
  data <- all_together_now$calc_pSurvival

  ilogit_b <- boot::inv.logit
  logit_b  <- boot::logit


  #Test 1: Bird did not survive (bmi == 0)
  testthat::expect_equal(
    calc_pSurvival(sp = "KI", bmi = 0, bm = data$bm[1], basesurv = data$basesurv[1], beta = data$beta),
    0
  )

  # Test 2: Bird's body mass index is NA
  testthat::expect_equal(
    calc_pSurvival(sp = "KI", bmi = NA, bm = data$bm[1], basesurv = data$basesurv[1], beta = data$beta),
    0
  )

  # Test 3:  for "KI" species
  with_my_funct <- calc_pSurvival(sp = "KI", bmi = data$bmi[1], bm = data$bm[1], basesurv = data$basesurv[1], beta = data$beta)
  expected <- ilogit_b(logit_b(data$basesurv[1]) + ((data$bmi[1] - data$bm[1]) * data$beta))
  testthat::expect_equal(with_my_funct, expected)

  # Test for species other than "KI"
  with_my_funct <- calc_pSurvival(sp = "GU", bmi = data$bmi[1], bm = data$bm[1], basesurv = data$basesurv[1], beta = data$beta, sd = data$sd[1])
  expected <- ilogit_b(logit_b(data$basesurv[1]) + ((data$bmi[1] - data$bm[1]) / data$sd[1]) * data$beta)
  testthat::expect_equal(with_my_funct, expected)

})
