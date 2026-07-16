################################################################################
#' @title SeabORD code to simulate one time step or simulated day
#'
#' @description This function simulates one time step in the season. Given the
#'   flight pattern, potential displacement or collision risk and foraging
#'   strategy calculate the activities in the time step for each adult bird and
#'   the chicks.
#'
#' @param Species Parameters for the bird species (List of 2)
#' @param Nscalefactor Fraction of the total population being modelled
#' @param popbirdsperkm2 Number of birds per km2
#' @param TimeVals List of time-related variables for the species, minimum and
#'   optimum time to be spent at the colony and minimum tome to be spent at sea.
#' @param TodaysFlights A tibble holding the flight information per bird for the
#'   current time step, including ORD effects.
#' @param TodaysForageComp Numbers of non-simulated birds per forage site
#' @param PreyAvailable A RasterLayer with prey availability for this time step
#' @param BirdType A tibble holding bird values that remain constant
#' @param BirdState A tibble holding bird values that may vary per day
#' @param ChickState A tibble holding chick values
#' @param Opt_BM_chick The current optimum chick body mass, for comparison
#' @param base_grid the default raster defining the region
#' @param fixedVals A list of miscellaneous values currently fixed but may
#'   become parameters in future versions
#'
#' @return A list with updated tibbles BirdState, ChickState and TodaysFlights,
#'   and Opt_BM_chick.
#' @noRd
seabord_daystep <- function(Species, Nscalefactor, popbirdsperkm2, TimeVals,
                            TodaysFlights, TodaysForageComp, PreyAvailable,
                            BirdType, BirdState, ChickState, Opt_BM_chick,
                            base_grid, fixedVals, EnergyMap = NULL) {


    # clearing chick state at the start of the day for dead birds
    # (NB: this restructure enables accurate plotting of unattendance time in individual plots)
    if (0 %in% ChickState$data$is_chick_alive) {
        ChickState$data <- ChickState$data %>% split(.$is_chick_alive==0)
        ChickState$data$`TRUE` <- ChickState$data$`TRUE` %>%
            dplyr::mutate(BM_chick = NA) %>%
            dplyr::mutate(BM_condition = NA) %>%
            dplyr::mutate(Ereq_chick = NA)    %>%
            dplyr::mutate(unattend_hrs = NA)
        ChickState$data <- bind_rows(ChickState$data$`TRUE`, ChickState$data$`FALSE`) %>%
            dplyr::arrange(PairID)
    }

    #> What part of the season is it? Not used yet, *for future expansion*
    BirdState$data <- BirdState$data %>%
        dplyr::mutate(season_stage = purrr::pmap_dbl(
            list(season_stage), .f = calc_season_stage))

    #> Update today's flight plans
    BirdState$data <- BirdState$data %>%
        dplyr::select(!any_of(c("BarrierOut", "BarrierRetn", "FirstChoiceIsIn",
                                "CollisionRisk", "Flighthrs"))) %>%
        dplyr::left_join(TodaysFlights[c("BirdID", "BarrierOut", "BarrierRetn",
                                         "FirstChoiceIsIn", "CollisionRisk",
                                         "Flighthrs")], by = "BirdID")

    # Update metadata
    BirdState$metadata <- bind_rows(BirdState$metadata, tidyr::tribble(
        ~VarName, ~VarDescription, ~VarUnits,
        "BarrierOut", "Flag to indicate if the bird encounters an ORD barrier on the outward trip", "0/1",
        "BarrierRetn", "Flag to indicate if the bird encounters an ORD barrier on the return trip", "0/1",
        "FirstChoiceIsIn", "Code name for the ORD(s) that displaced the bird", "Text",
        "CollisionRisk", "Flag to indicate if the bird travels though an ORD", "0/1",
        "Flighthrs", "Length of time to complete one round trip to the chosen destination", "hours"))  %>%
        dplyr::distinct()

    #--> Gather all the remaining data for this time step  ------------------------------>

    #> Join up the adults and chicks for this time step
    BirdState$data <- BirdState$data %>%
        dplyr::select(!any_of(c("PairID","is_chick_alive","Ereq_chick"))) %>%
        dplyr::left_join(BirdType$data[c("BirdID","PairID")], by = "BirdID") %>%
        dplyr::left_join(ChickState$data[c("PairID", "is_chick_alive", "Ereq_chick")],
                         by = "PairID")
    # Update metadata
    BirdState$metadata <- bind_rows(BirdState$metadata, tidyr::tribble(
        ~VarName, ~VarDescription, ~VarUnits,
        "PairID", "Identifier for the adult M/F pair to identify chick", "",
        "Ereq_chick", "Energy required by chick at time t", "kJ"))  %>%
        dplyr::distinct()

    #> Total daily energy requirement per bird depending if it has a chick
    # (Req_gram, the grams needed to meet this, is computed below once the
    #  foraging destination -- and hence its prey energy density -- is known.)
    BirdState$data <- BirdState$data %>%
        dplyr::select(!any_of(c("Ereq_total", "Req_gram", "E_dens"))) %>%
        dplyr::mutate(Ereq_total = purrr::pmap_dbl(list(
            feeding_mode, Ereq_adult, Ereq_chick, is_chick_alive),
            ~ ifelse((({..1} < 3) & ({..4} == 1)), {..2} + 0.5*{..3}, {..2})
        ))

    # Update metadata
    BirdState$metadata <- bind_rows(BirdState$metadata, tidyr::tribble(
        ~VarName, ~VarDescription, ~VarUnits,
        "Ereq_total", "Total daily energy requirement for adult bird plus its share of the chick", "kJ",
        "Req_gram", "Quantity of food required in this timestep (to cover previous step activity)", "g")) %>%
        dplyr::distinct()

    #> Prey available per bird for this flight
    BirdState$data <- BirdState$data %>%
        dplyr::select(!any_of(c("Destination", "Prey0"))) %>%
        dplyr::left_join(TodaysFlights[c("BirdID", "Destination")], by = "BirdID") %>%
        dplyr::mutate(Prey0 = purrr::pmap_dbl(list(is_alive, Destination),
                                              ~ ifelse({..1}>0, PreyAvailable[{..2}], 0)))

    #> Per-bird prey energy density (kJ/g) at the foraging destination.
    # Default: the uniform species value. If an EnergyMap raster is supplied, a
    # cell may have a different density (e.g. richer offal at 9 kJ/g); cells with
    # NA in EnergyMap fall back to the species value.
    ki_energy <- Species$data$energy_prey
    if (!is.null(EnergyMap)) {
        EnergyVec <- raster::values(EnergyMap)
        BirdState$data <- BirdState$data %>%
            dplyr::mutate(E_dens = purrr::pmap_dbl(list(is_alive, Destination),
                ~ if ({..1} > 0) {
                      e <- EnergyVec[{..2}]; if (is.na(e)) ki_energy else e
                  } else ki_energy))
    } else {
        BirdState$data <- BirdState$data %>% dplyr::mutate(E_dens = ki_energy)
    }

    # Grams needed to meet the energy requirement, using the destination's
    # energy density (a bird foraging on richer prey needs fewer grams).
    # (Harris et al. 2008 for the baseline species density.)
    BirdState$data <- BirdState$data %>%
        dplyr::mutate(Req_gram = Ereq_total / E_dens)

    # Update metadata
    BirdState$metadata <- bind_rows(BirdState$metadata, tidyr::tribble(
        ~VarName, ~VarDescription, ~VarUnits,
        "Destination", "Grid cell number of the foraging destination", "",
        "Prey0", "Prey available at the foraging site", "g/area")) %>%
        dplyr::distinct()

    #> Count the number of simulated birds foraging at each destination this step
    ForagingCount <- base_grid
    ForagingCount[] <- 0
    i <- raster::aggregate(TodaysFlights$Destination>0, by=list(c=TodaysFlights$Destination), FUN=sum)
    ForagingCount[i$c] <- i$x

    # 'Background' foraging count - birds from non-simulated SPAs
    ForagingCount <- (ForagingCount/Nscalefactor) + TodaysForageComp

    ## Amendment for EPSG:3035 'area' only useful for Raster* objects with a
    ## longitude/latitude coordinates. Was: GridArea <- area(base_grid)
    GridArea <- base_grid; values(GridArea) <- 1
    TodaysFlights <- TodaysFlights %>%
        dplyr::mutate(Birdsperkm2 = purrr::pmap_dbl(list(Destination),
                                                    ~ ForagingCount[{..1}]/GridArea[{..1}] )) %>%
        dplyr::mutate(ComFactor = Birdsperkm2 / popbirdsperkm2) %>%
        dplyr::mutate(IRhalf = round(
            Species$data$IR_half_a * (ComFactor^Species$data$IR_half_b), 0))

    # Join the IRhalf factor to the birds to facilitate calculations
    BirdState$data <- BirdState$data %>%
        dplyr::select(!any_of(c("IRhalf"))) %>%
        dplyr::left_join(TodaysFlights[c("BirdID","IRhalf")], by = "BirdID")

    # Update metadata
    BirdState$metadata <- bind_rows(BirdState$metadata, tidyr::tribble(
        ~VarName, ~VarDescription, ~VarUnits,
        "IRhalf", "Foraging competition factor", "")) %>% dplyr::distinct()

    # At this point we know how far the bird has to fly to get to its chosen
    # destination and the prey availability. Next, estimate number of flights
    # taken to acquire sufficient food and resulting time at colony or at sea.

    #--> Scale up to full timestep & optimise capture ------------------------

    #BirdState <- calc_scaleflights(BirdState, TimeVals, Species, fixedVals)

    # -- new strategy function start -->

    BirdState$data <- BirdState$data %>%
        dplyr::mutate(food = purrr::pmap(
            list(x0 = Prey0, h = IRhalf), .f = memoised_calc_foragecapture,
            a = Species$data$IR_max, maxt = 60*Species$data$daylength))

    BirdState$data <- BirdState$data %>%
        dplyr::mutate(strategy = purrr::pmap(
            list(BM_condition = BM_condition,
                 BM_adult_abdn = Species$data$BM_adult_abdn,
                 fmode = feeding_mode,
                 Fg = Req_gram,
                 tf = Flighthrs,
                 tcapt = food,
                 maxf = fixedVals$flights_max,
                 ts = TimeVals$at_sea_min_h,
                 tc = TimeVals$colony_opt_h,
                 tm = TimeVals$colony_min_h,
                 ttotal = Species$data$daylength), .f = calc_strategy)) %>%
        dplyr::select(-c(feeding_mode, food))

    # Delete any previous timestep strategy columns
    BirdState$data <- BirdState$data %>%
        dplyr::select(-any_of(c("trips_n", "foraging_h", "flying_h", "forage_g",
                                "colony_h", "at_sea_h")))

    BirdState$data <- BirdState$data %>% tidyr::unnest(cols = strategy)

    # > Update the metadata to reflect newly added variables to the BirdState tibble
    newmeta <- tidyr::tribble(
        ~VarName, ~VarDescription, ~VarUnits,
        "trips_n", "Number of return journeys for foraging", " ",
        "flying_h", "Time spent flying during this time step", "hours",
        "foraging_h", "Time spent foraging during this time step", "hours",
        "colony_h", "Time spent at the colony on this time step", "hours",
        "at_sea_h", "Time spent resting at sea on this timestep", "hours",
        "forage_g", "Total food acquired this time step", "g")
    BirdState$metadata <- bind_rows(BirdState$metadata, newmeta) %>%
        dplyr::distinct()

    # -- new strategy function end <--

    #--> Apply collision effect if appropriate --------------------------------->

    #source(("block-collision.R"))

    #<-- end apply collision effect if appropriate <--------------------------

    #--> Calculate the allocation of food to adults and chicks ----------------->

    # Convert grams caught to energy, using each bird's destination energy
    # density (E_dens): the species value everywhere, or a richer value (e.g.
    # offal at 9 kJ/g) in any cell flagged by EnergyMap.
    BirdState$data <- BirdState$data %>%
        dplyr::mutate(E_caught = forage_g * E_dens)

    # Apportion to the adult and the chick appropriately
    # Species$data$Adult_priority no longer set - fixing as 0 for now

    Adult_priority <- 0

    if (Adult_priority > 0) {

        BirdState$data <- BirdState$data %>%

            # If the adult was to give the chick everything it wants first...
            dplyr::mutate(minfood = E_caught - (0.5 * Ereq_chick)) %>%
            dplyr::mutate(minfood = purrr::pmap_dbl(
                list(minfood, 0.0), ~ ifelse({..1} < {..2}, {..2}, {..1}))) %>%

            # If the adult takes everything it wants first...
            dplyr::mutate(maxfood = purrr::pmap_dbl(
                list(E_caught, Ereq_adult), ~ ifelse({..1} < {..2}, {..1}, {..2}))) %>%

            # Sense check...
            dplyr::mutate(minfood = purrr::pmap_dbl(
                list(minfood, maxfood), ~ ifelse({..1} > {..2}, {..2}, {..1}))) %>%

            # What is the difference between the min and max the adult could receive?
            dplyr::mutate(gainrange = maxfood - minfood) %>%
            dplyr::mutate(gainrange = purrr::pmap_dbl(
                list(gainrange, 1e-06), ~ ifelse(({..1} < {..2}), 0.0, {..1}))) %>%

            # What is the actual adult and chick gain from food?
            dplyr::mutate(Egain_adult = minfood + (Species$data$Adult_priority*gainrange)) %>%
            dplyr::mutate(Egain_chick = E_caught - Egain_adult) %>%
            dplyr::mutate(Ereq_intakef_a = Egain_adult / Ereq_adult) %>%
            dplyr::mutate(Ereq_intakef_c = Egain_chick / (0.5*total_Ereq_c))

        # tidy up
        BirdState$data <- dplyr::select(BirdState$data, -c(minfood, maxfood, gainrange))

    } else {

        # When Adult_priority = 0 (the default) food shared in proportion
        BirdState$data <- BirdState$data %>%
            dplyr::mutate(Ereq_intakef_a = E_caught/Ereq_total) %>%
            dplyr::mutate(Ereq_intakef_c = Ereq_intakef_a) %>%
            dplyr::mutate(Egain_adult = Ereq_adult  *Ereq_intakef_a) %>%
            dplyr::mutate(Egain_chick = 0.5 * Ereq_chick * Ereq_intakef_c)

    }

    # Update the metadata to reflect newly added variables to BirdState
    newmeta <- tidyr::tribble(
        ~VarName, ~VarDescription, ~VarUnits,
        "E_caught", "Total energy intake this time step", "kJ",
        "Ereq_intakef_a", "Fraction of the food intake that goes to the adult bird", "",
        "Ereq_intakef_c", "Fraction of the food intake that goes to the chick", "",
        "Egain_adult", "Energy gain by the adult", "kJ",
        "Egain_chick", "Energy gain by the chick", "kJ")
    BirdState$metadata <- bind_rows(BirdState$metadata, newmeta) %>%
        dplyr::filter(VarName %in% unique(names(BirdState$data)))

    #<-- end Calculate the allocation of food to adults and chicks ------------<

    #--> Update the chick feeding and attendance ------------------------------>

    # Update the 'optimum chick' for comparison
    Opt_BM_chick <- Opt_BM_chick + Species$data$chick_mass_a

    # The chick receives food from both parents (independently) so we make
    # sure the adults are paired correctly...
    parent <- BirdState$data %>%
        dplyr::select(c(BirdID, PairID, feeding_mode, Ereq_intakef_c, colony_h)) %>%
        dplyr::left_join(BirdType$data[, c("BirdID", "MF")],
                         by = "BirdID", keep = FALSE)



    # Update chicks
    ChickState <- calc_chickcare(parent, ChickState, Opt_BM_chick, Species)

    #<-- end. update the chick feeding and attendance <------------------------<

    # > Puffin chick mortality from predation via hunger
    if (Species$data$SID == "PU") {
        ChickState$data <- ChickState$data %>%
            dplyr::mutate(CoD = purrr::pmap_chr(
                list(CoD = CoD, alive = is_chick_alive,
                     BM_condition = BM_condition,
                     BM_Chick_mortf = Species$data$BM_Chick_mortf), .f = calc_puffinmortality))

        if ("killed" %in% ChickState$data$CoD) {
            ChickState$data <- ChickState$data %>% split(.$CoD=="killed")
            ChickState$data$`TRUE` <- ChickState$data$`TRUE` %>%
                dplyr::mutate(is_chick_alive = 0)
            ChickState$data <- bind_rows(ChickState$data$`TRUE`, ChickState$data$`FALSE`) %>%
                dplyr::arrange(PairID)
        }
    }

    # > All species where unattendence applies - chick mortality via non-attendance
    # (NB must follow calc_chickcare)
    if (Species$data$unattend_max_hrs > 0) {
        ChickState$data <- ChickState$data %>%
            dplyr::mutate(CoD = purrr::pmap_chr(
                list(CoD = CoD, alive = is_chick_alive,
                     unattend_hrs = unattend_hrs,
                     max_hrs = Species$data$unattend_max_hrs), .f = calc_unattendmortality))

        if ("unattended" %in% ChickState$data$CoD) {
            ChickState$data <- ChickState$data %>% split(.$CoD=="unattended")
            ChickState$data$`TRUE` <- ChickState$data$`TRUE` %>%
                dplyr::mutate(is_chick_alive = 0)
            ChickState$data <- bind_rows(ChickState$data$`TRUE`, ChickState$data$`FALSE`) %>%
                dplyr::arrange(PairID)
        }
    }

    # > All species - chick mortality from flooding and other causes
    ChickState$data <- ChickState$data %>%
        dplyr::mutate(CoD = purrr::pmap_chr(
            list(CoD = CoD, alive = is_chick_alive,
                 seasonlength = Species$data$seasonlength), .f = calc_othermortality))

    if ("other" %in% ChickState$data$CoD) {
        ChickState$data <- ChickState$data %>% split(.$CoD=="other")
        ChickState$data$`TRUE` <- ChickState$data$`TRUE` %>%
            dplyr::mutate(is_chick_alive = 0)
        ChickState$data <- bind_rows(ChickState$data$`TRUE`, ChickState$data$`FALSE`) %>%
            dplyr::arrange(PairID)
    }

    #===========================================================================

    # > Update the adult bird state tibble with new chick status
    BirdState$data <- BirdState$data %>%
        dplyr::select(-any_of(c("is_chick_alive"))) %>%
        dplyr::left_join(ChickState$data[, c("PairID", "is_chick_alive")], by = "PairID")

    # Update the feeding mode if chick died this time step
    BirdState$data <- BirdState$data %>%
        dplyr::mutate(feeding_mode = purrr::pmap_dbl(
            list(feeding_mode, is_chick_alive),
            ~ ifelse(({..1} < 3) & ({..2}==0), 3, {..1})))

    #--> Update the adult body and condition ---------------------------------->

    # Update adult bird body mass based on food acquired.
    BirdState$data <- BirdState$data %>%
        dplyr::mutate(BM_adult = purrr::pmap_dbl(
            list(alive = is_alive, BM_adult = BM_adult,
                 Egain_adult = Egain_adult, Ereq_adult = Ereq_adult,
                 adult_mass_KG = Species$data$adult_mass_KG), .f = calc_adultbmchange)) %>%
        dplyr::mutate(BM_condition = BM_adult/BM_adult_t0)

    # Check which birds have lost too much condition to survive
    BirdState$data <- BirdState$data %>%
        dplyr::mutate(CoD = purrr::pmap_chr(
            list(is_alive, BM_condition, Species$data$BM_adult_mortf, CoD),
            ~ ifelse({..1} & ({..2}<={..3}), "newstarved", {..4})))

    if ("newstarved" %in% BirdState$data$CoD) {
        BirdState$data <- BirdState$data %>% split(.$CoD=="newstarved")
        BirdState$data$`TRUE` <- BirdState$data$`TRUE` %>%
            dplyr::mutate(CoD = "starved") %>%
            dplyr::mutate(is_alive = 0) %>%
            dplyr::mutate(feeding_mode = 4) %>%
            dplyr::mutate(BM_adult = NA) %>%
            dplyr::mutate(Ereq_adult = NA)
        BirdState$data <- bind_rows(BirdState$data$`TRUE`, BirdState$data$`FALSE`) %>%
            dplyr::arrange(BirdID)
    }

    # Check which birds are still alive but have lost too much condition to
    # keep raising chick. Set their feeding mode to 3
    BirdState$data <- BirdState$data %>%
        dplyr::mutate(feeding_mode = purrr::pmap_dbl(
            list(BM_condition, Species$data$BM_adult_mortf,
                 Species$data$BM_adult_abdn, feeding_mode),
            ~ ifelse(({..4}<3) & ({..1}>{..2}) & ({..1}<{..3}), 3, {..4})))

    # Calculate the adult bird DEE in timestep t for t+1 demands)
    BirdState$data <- BirdState$data %>%
        dplyr::mutate(Ereq_adult = purrr::pmap_dbl(
            list(alive = is_alive,
                 colony_h = colony_h,
                 flying_h = flying_h,
                 foraging_h = foraging_h,
                 at_sea_h = at_sea_h,
                 energy_nest = Species$data$energy_nest,
                 energy_flight = Species$data$energy_flight,
                 energy_forage = Species$data$energy_forage,
                 energy_searest = Species$data$energy_searest,
                 energy_warming = Species$data$energy_warming,
                 assim_eff = Species$data$assim_eff,
                 daylength = Species$data$daylength), .f = calc_adultdee)) %>%
        dplyr::mutate(Ereq_chick = purrr::pmap_dbl(
            list(feeding_mode, Ereq_chick), ~ ifelse({..1}>=3, 0, {..2})))

    # Tidy
    BirdState$data <- BirdState$data %>%
        dplyr::select(!any_of(c("BarrierOut", "BarrierRetn", "FirstChoiceIsIn",
                                "CollisionRisk", "Flighthrs")))

    # Update/tidy the metadata
    BirdState$metadata <- BirdState$metadata %>%
        dplyr::filter(VarName %in% unique(names(BirdState$data))) %>%
        dplyr::distinct()

    # Return the amended tibbles
    out <- list(BirdState = BirdState, ChickState = ChickState,
                Opt_BM_chick = Opt_BM_chick,
                TodaysFlights = TodaysFlights)

    return(out)
}
