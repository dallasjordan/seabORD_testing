testthat::test_that("multiplication works", {

  # Test 1: If puffin is already dead, CoD remains unchanged
  with_my_funct <- calc_puffinmortality(CoD = "starved", alive = 0, BM_condition = 0.5, BM_Chick_mortf = 0.3)
  testthat::expect_equal(with_my_funct, "starved")

  # Test 2: If puffin is alive and not "hungry" (BM_condition >= 0.7)
  with_my_funct <- calc_puffinmortality(CoD = NULL, alive = 1, BM_condition = 0.8, BM_Chick_mortf = 0.3)
  testthat::expect_equal(with_my_funct , NULL) # stays alive = no change in CoD

  # Test 3: puffin is alive, hungry, but survives (rbinom returns 0)
  set.seed(123)
  with_my_funct <- calc_puffinmortality(CoD = NULL, alive = 1, BM_condition = 0.5, BM_Chick_mortf = 0.3)
  testthat::expect_equal(with_my_funct , NULL) # stays alive = no change in CoD

  # Test 4: puffin is alive, hungry, but gets killed (rbinom returns 1)
  set.seed(456)
  with_my_funct <- calc_puffinmortality(CoD = NULL, alive = 1, BM_condition = 0.4, BM_Chick_mortf = 0.3)
  testthat::expect_equal(with_my_funct , "killed")

})
