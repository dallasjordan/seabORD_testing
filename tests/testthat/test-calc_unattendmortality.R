testthat::test_that("calc_unattendmortality", {

  # Test 1: Alive, no unattended hours
  with_my_funct <- calc_unattendmortality(CoD = NULL, alive = 1, unattend_hrs = 0, max_hrs = 12)
  testthat::expect_equal(with_my_funct, NULL)

  # Test 2: Alive, unattended hours below critical threshold
  with_my_funct <- calc_unattendmortality(CoD = NULL, alive = 1, unattend_hrs = 6, max_hrs = 12)
  testthat::expect_equal(with_my_funct, NULL) # Low probability, unlikely to die

  # Test 3: Alive, unattended hours == to critical threshold
  with_my_funct <- calc_unattendmortality(CoD = NULL, alive = 1, unattend_hrs = 12, max_hrs = 12)
  testthat::expect_true(with_my_funct %in% c(NULL, "unattended")) # Depends on random draw

  # Test 4: Alive, unattended hours > critical threshold
  with_my_funct <- calc_unattendmortality(CoD = NULL, alive = 1, unattend_hrs = 18, max_hrs = 12)
  testthat::expect_equal(with_my_funct, "unattended") # High probability, chick likely dies

  # Test 5: Dead
  with_my_funct <- calc_unattendmortality(CoD = "starved", alive = 0, unattend_hrs = 18, max_hrs = 12)
  testthat::expect_equal(with_my_funct, "starved")  # CoD should not change

})
