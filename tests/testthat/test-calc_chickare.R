test_that("test calc_chickcare", {

  load("~/repositories/SeabORD/seabORD/local/test_values_v2.rdata")
  data <- all_together_now$calc_chickcare

  #preparing example inputs
  #parent A tibble containing the required variables from the adult birds (BirdID, PairID, feeding_mode, Ereq_intakef_c, colony_h, MF).
  parent <- tibble::as_tibble(data[1:6])

  #ChickState List holding the current state of all the chicks and metadata.
  ChickState <-  list(data$is_chick_alive)

  #Opt_BM_chick The optimum body mass for the chick at this stage of the season (a chick that has received full requirements every day).
  Opt_BM_chick <- data$Opt_BM_chick

  #Species List holding the species-specific variables (uses daylength, BM_Chick_mortf and chick_mass_a)
  Species <- list(data$chick_mass_a)

  expected <- readRDS("~/repositories/SeabORD/seabORD/local/chickacare.rds")
  with_my_funct <- seabORD::calc_chickcare(parent, ChickState, Opt_BM_chick, Species)
  testthat::expect_equal(with_my_funct, expected)

})
