
testthat::test_that("test calc_adultdee", {

  # Test 1:  when `alive = TRUE`
  with_my_funct <-  calc_adultdee( alive = TRUE,
                                   colony_h = 4,
                                   flying_h = 3,
                                   foraging_h = 5,
                                   at_sea_h = 10,
                                   energy_nest = 2,
                                   energy_flight = 8,
                                   energy_forage = 5,
                                   energy_searest = 3,
                                   energy_warming = 1,
                                   assim_eff = 0.75,
                                   daylength = 24 )
  expected <- (2 * 4 / 24) + (8 * 3 / 24) + (5 * 5 / 24) + (3 * 10 / 24) + (1 * 24 / 24)
  expected <- expected / 0.75
  testthat::expect_equal(with_my_funct, expected)

  # Test 2:  when `alive = FALSE`
  with_my_funct <-  calc_adultdee( alive = FALSE,
                                   colony_h = 4,
                                   flying_h = 3,
                                   foraging_h = 5,
                                   at_sea_h = 10,
                                   energy_nest = 2,
                                   energy_flight = 8,
                                   energy_forage = 5,
                                   energy_searest = 3,
                                   energy_warming = 1,
                                   assim_eff = 0.75,
                                   daylength = 24 )
  expected <- 0
  testthat::expect_equal(with_my_funct, expected)

})
