test_that("test calc_strategy", {

  load("~/repositories/SeabORD/seabORD/local/test_values_v2.rdata")
  data <- all_together_now$calc_strategy

  #with mode == 4
  with_my_funct<- seabORD::calc_strategy(BM_condition = data$BM_condition,
                                         BM_adult_abdn = data$BM_adult_abdn,
                                         fmode = 4,
                                         Fg = data$Fg,
                                         tf = data$tf,
                                         tcapt = data$tcapt,
                                         maxf = data$maxf,
                                         ts = data$ts,
                                         tc = data$tc,
                                         tm = data$tm,
                                         ttotal = data$ttotal)
  expected <- tibble(trips_n = NA, foraging_h = NA, flying_h = NA, forage_g = NA,
                     colony_h = NA, at_sea_h = NA, feeding_mode = 4)

  testthat::expect_equal(with_my_funct, expected)

})
