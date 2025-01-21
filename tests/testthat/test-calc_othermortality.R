testthat::test_that("calc_othermortality", {

  # Test 1:  alive, no death occurs
  set.seed(123) # Set seed for reproducibility
  with_my_funct <- calc_othermortality(CoD = NULL, alive = 1, seasonlength = 30)
  testthat::expect_true(is.null(with_my_funct))

  # Test 2: dead, CoD remains unchanged
  with_my_funct <- calc_othermortality(CoD = "starved", alive = 0, seasonlength = 30)
  testthat::expect_equal(with_my_funct, "starved")

  # Test 3: Death occurs due to other causes
  set.seed(789) # Change seed for a different random outcome
  with_my_funct <- replicate(1000, calc_othermortality(CoD = NULL, alive = 1, seasonlength = 30))
  testthat::expect_true("other" %in% with_my_funct)

})
