
testthat::test_that("test calc_adultdee", {

  load("~/repositories/SeabORD/seabORD/local/test_values_v2.rdata")
  data <- data.frame(all_together_now$calc_adultdee)

  # Test 1:  when `alive = TRUE`
  with_my_funct <-  seabORD::calc_adultdee( alive = TRUE,
                                            colony_h = data$colony_h,
                                            flying_h = data$flying_h,
                                            foraging_h = data$foraging_h,
                                            at_sea_h = data$at_sea_h,
                                            energy_nest = data$energy_nest,
                                            energy_flight = data$energy_flight,
                                            energy_forage = data$energy_forage,
                                            energy_searest = data$energy_searest,
                                            energy_warming = data$energy_warming,
                                            assim_eff = data$assim_eff,
                                            daylength = data$daylength )


  expected <- with(data , ( (energy_nest * colony_h / daylength) +
                              (energy_flight * flying_h / daylength) +
                              (energy_forage * foraging_h / daylength) +
                              (energy_searest * at_sea_h / daylength) +
                              (energy_warming * daylength / 24) )
                   / assim_eff)

  testthat::expect_equal(with_my_funct, expected)

  # Test 2:  when `alive = FALSE`
  with_my_funct <-  calc_adultdee( alive = FALSE,
                                   colony_h = data$colony_h,
                                   flying_h = data$flying_h,
                                   foraging_h = data$foraging_h,
                                   at_sea_h = data$at_sea_h,
                                   energy_nest = data$energy_nest,
                                   energy_flight = data$energy_flight,
                                   energy_forage = data$energy_forage,
                                   energy_searest = data$energy_searest,
                                   energy_warming = data$energy_warming,
                                   assim_eff = data$assim_eff,
                                   daylength = data$daylength )
  expected <- 0
  testthat::expect_equal(with_my_funct, expected)

})
