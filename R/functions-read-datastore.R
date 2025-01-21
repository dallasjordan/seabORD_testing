## ########################################################################################
## Title: SeabORD Data Store Functions
## Date: March 2023
## Author: UKCEH
## ########################################################################################

# This file holds functions to read datasets from the SeabORD Data Store (using numbering
# consistent with the Cumulative Effects Framework)

# Contents:
# file.path("01_Species","101_SpeciesList"): read_specieslist()

# file.path("02_Colony","201_SPAList"): read_spalist()
# file.path("02_Colony","202_BDMPSpopsizes"): read_bdmpspopsizes()
# file.path("02_Colony","203_SMP"): read_smp()
# file.path("02_Colony","204_S2000"): read_s2000()
# file.path("02_Colony","205_SPAPolygons_GB"): read_spapolygonsgb()
# file.path("02_Colony","206_SPAPolygons_NI"): read_spapolygonsni()
# file.path("02_Colony","207_SPAcoords"): read_spacoords()
# file.path("02_Colony","208_SPASiteDetails"): read_spasitedetails()
# file.path("02_Colony","209_SPAInterestFeatures"): read_spainterestfeatures()
#
# file.path("03_Bird","301_BasicBird"): read_basicbird()
# file.path("03_Bird","304_Productivity"): read_productivity()
# file.path("03_Bird","309_EnergeticsPrey"): read_energeticsprey()
# file.path("03_Bird","310_SeasonalActivity"): read_seasonalactivity()
#
# file.path("05_ORD","501_ORDNames"): read_ordnames()
# file.path("05_ORD","502_Sites"): read_ordsites()
#
# file.path("06_Geographic","601_Coastline"): read_coastline()
# file.path("06_Geographic","602_FlightPathTL"): read_flightpathtl()
# file.path("06_Geographic","603_FineGrid"): read_finegrid()
# file.path("06_Geographic","605_GPSGrid"): read_gpsgrid()
# file.path("06_Geographic","606_DistFineGrid2SPA"): read_distfinegrid2spa()
#
# file.path("07_Distributions","702_GPSglobalMapsPlusKey"): read_gpsmaps()
# file.path("07_Distributions","705_GPSMapsbySPA"): read_gpsmapsbyspa()

## ########################################################################################
#' @title Import the current seabird & marine mammal combined species list
#'
#' @description This dataset is a list of species (seabirds and marine mammals)
#'   that are included in the Cumulative Effects Framework (CEF). Variables in the
#'   table indicate which tools include functionality for the given species, together
#'   with species names, codes and alternative names used within the CEF.
#'
#'   Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param val A string to search for in any character column in the table,
#'   leave blank to return the full table.
#'
#' @return A tibble with 15 variables: ENGLISH_NAME, LATIN_NAME, TAXA_CODE,
#'   SHORT_CODE, FIVE_LETTER, BOU_NAME, sCRM_NAME, CEF, iPCoD, SeabORD, sCRM,
#'   NEPVA, DispMat, ORJIPSMT, MSSapp
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr if_any
#' @importFrom readr cols
#'
#' @examples
#'    read_specieslist()
#'    read_specieslist(val = "mm")
#'    read_specieslist(val = "CX")$ENGLISH_NAME
#'    read_specieslist(val = "Harbour seal")$SHORT_CODE
#'
#' @noRd

read_specieslist <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                             dssection = file.path("01_Species","101_SpeciesList"),
                             verpathn = "v1.0",
                             dname = "specieslist",
                             val = NA) {

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols())

        if (!is.na(val)){
            f <- f %>% dplyr::filter(if_any(where(is.character), ~ .x == val))
        } else {
            f
        }

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_specieslist)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_specieslist)")})

    return(result)
}

## ########################################################################################
#' @title Import the SPA name dataset
#'
#' @description This dataset is a list of SPAs that are included in the Cumulative Effects
#'   Framework (CEF). Variables in the table include SPA primary name plus any codes or
#'   alternative names used within the CEF.
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param cefinc Return only SPAs included in the CEF. Logical.
#' @param code A string to search for in any character column in the table,
#'   leave blank to return the full table.
#'
#' @return A tibble with with 10 variables: SITE_CODE <chr>, SITE_NAME <chr>,
#'   CEF.include <chr>, Marine.only <chr>, Altnames.sufficient <chr>,
#'   Altnames.SPApolys <chr>, Altnames.SMP <chr>, Altnames.Productivity <chr>,
#'   Altnames.BDMPS <chr>, Altnames.FR <chr>
#'
#' @importFrom dplyr if_any
#' @importFrom readr cols col_character col_logical
#'
#' @examples
#'    read_spalist()
#'
#' @noRd

read_spalist <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                         dssection = file.path("02_Colony","201_SPAList"),
                         verpathn = "sept2021",
                         dname = "spalist",
                         cefinc = TRUE,
                         code = ".") {

    SITE_CODE <- CEF.include <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             na = "empty", comment = "#",
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 .default = readr::col_character())
        ) %>%
            dplyr::filter(stringr::str_detect(SITE_CODE, paste(stringr::str_to_upper(code), collapse = '|')))

        if (cefinc){
            f <- f %>% dplyr::filter(CEF.include == "Y")
        } else {
            f
        }


    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_spalist)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_spalist)")})

    return(result)

}

## ########################################################################################
#' @title Import the BDMPS population size file
#'
#' @description This function reads in the comma-separated value data file of population
#' sizes derived from the Biologically Defined Minimum Population Scales (BDMPS). Can be
#' filtered for specific species, population types or populations if required.
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param spcode Two-letter species code(s)
#' @param spname The required species name(s) (common name). Returns all species by default.
#' @param poptype Type of population: e.g. UK SPA, UK non-SPA, or overseas. Returns all
#'   population types by default.
#' @param pop Name of the population (SPA for the UK). Returns all by default.
#'
#' @return A tibble with with 5 variables: Species, Population, Poptype,
#'   Pairs, Most.recent.count
#'
#' @importFrom magrittr %>%
#' @importFrom readr cols col_character col_integer
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'    read_bdmpspopsizes()
#'    read_bdmpspopsizes(spcode = c("AC","E."))
#'    read_bdmpspopsizes(spname = c("Arctic skua","Atlantic puffin"))
#'    read_bdmpspopsizes(pop = c("Fair Isle","Ailsa Craig"))
#'    read_bdmpspopsizes(poptype = "UK SPA pop",spname = c("Gannet"))$Pairs
#'
#' @noRd

read_bdmpspopsizes <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                               dssection = file.path("02_Colony","202_BDMPSpopsizes"),
                               verpathn = "apr2021",
                               dname = "bdmpspopulationsizes",
                               spcode = c(""),
                               spname = c(""),
                               poptype = c(""),
                               pop = c("")) {

    SHORT_CODE <- Species <- Poptype <- Population <- NULL

    result <- tryCatch({

        # Read in the requested data files (csv)
        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 .default = col_character(),
                                 Pairs = col_integer(),
                                 Most.recent.count = col_character())) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(SHORT_CODE), paste(str_to_lower(spcode), collapse = '|'))) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Species), paste(str_to_lower(spname), collapse = '|'))) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Poptype), paste(str_to_lower(poptype), collapse = '|'))) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Population), paste(str_to_lower(pop), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_bdmpspopsizes)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_bdmpspopsizes)")})

    return(result)
}

## ########################################################################################
#' @title Import the seabird monitoring programme dataset (SMP)
#'
#' @description Read the SMP data from the Data Store, filtered by species, site ID, site
#'   name or master site name if required.
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param spname The required species name(s) (common name)
#' @param siteid The required site(s), given as ID number
#' @param site The required site name(s)
#' @param mastersite The required master site name(s)
#'
#' @return A tibble with 16 variables: Species, Country, County, Site ID, Site,
#'   Master site, StartGrid, EndGrid, Start date, End date, Times, Method, Unit,
#'  Count, Accuracy, Estimate type
#'
#' @importFrom magrittr %>%
#' @importFrom readr cols col_character col_integer col_date col_double
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_smp()
#'   read_smp(spname = c("Fulmar","Shag"))
#'   read_smp(site = c("Craro","Delph Reservoir"))
#'   read_smp(siteid = 101040, spname = "tern")
#'

#' @noRd
read_smp <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                     dssection = file.path("02_Colony","203_SMP"),
                     verpathn = "1986to2019",
                     dname = "ukseabirdmonitoringprogramme",
                     spname = c(""),
                     siteid = 0,
                     site = c(""),
                     mastersite = c("")) {

    Species <- Site <- `Master site` <- `Site ID` <- NULL

    result <- tryCatch({
        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 .default = col_character(),
                                 `Site ID` = col_integer(),
                                 `Master site` = col_character(),
                                 `Start date` = col_date(format = "%d/%m/%Y"),
                                 `End date` = col_date(format = "%d/%m/%Y"),
                                 Method = col_double(),
                                 Unit = col_character(),
                                 Count = col_character(),
                                 `Estimate type` = col_character())) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Species), paste(str_to_lower(spname), collapse = '|'))) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Site), paste(str_to_lower(site), collapse = '|'))) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(`Master site`), paste(str_to_lower(mastersite), collapse = '|')))

        if (siteid != 0){
            f <- f %>% dplyr::filter(`Site ID` == siteid)
        } else {
            f
        }

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_smp)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_smp)")})

    return(result)
}

## ########################################################################################
#' @title Import the Seabird 2000 census dataset
#'
#' @description Read either the original Seabird 2000 census data from the Data
#'   Store, with filters for species name, site name and subsite name (some
#'   columns are not imported as they are not used in the CEF) or the processed
#'   version created for the CEF, with filter for species code,
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param splname Seabird species name(s) (latin)
#' @param spcode Seabird species two-letter code
#' @param site Site name(s)
#' @param subsite subsite name(s)
#'
#' @return Either a tibble with 24 variables (Country, AdminArea, SubAdminArea, Site,
#'   Subsite, StartGrid, EndGrid, StartDate, EndDate, RecordType, Species,
#'   _Count, Qualifier, Accuracy, UCL, LCL, AdjustedCount, AdjustedQualifier,
#'   CountQuality, IdealPeriod, IdealTime, Ind_Pair, Inland/Coastal, Habitat
#'   class) or
#'
#' @importFrom magrittr %>%
#' @importFrom readxl readxl_progress
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_s2000()
#'   read_s2000(splname = c("Fulmarus","Phalacrocorax aristotelis"))
#'   read_s2000(verpathn = "Seabird2000_22_10_10_cef01", spcode = "KI")
#'
#' @noRd

read_s2000 <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                       dssection = file.path("02_Colony","204_S2000"),
                       verpathn = "Seabird2000_22_10_10",
                       splname = NA,
                       spcode = NA,
                       site = NA,
                       subsite = NA) {

    Site <- Species <- Subsite <- NULL

    result <- tryCatch({

        if (verpathn == "Seabird2000_22_10_10") {

            f <- readxl::read_excel(file.path(dspathn, dssection, verpathn, "Seabird 2000_22_10_10.xls"),
                                    sheet = "MasterUse", col_names = TRUE,
                                    trim_ws = TRUE, skip = 0, n_max = Inf, progress = readxl_progress(),
                                    col_types = c("skip", "text", "text", "text", "text", "text",
                                                  "text", "text", "date", "date", "text", "text",
                                                  "numeric", "text", "text", "numeric", "numeric",
                                                  "numeric", "text", "skip", "skip", "skip", "skip",
                                                  "numeric", "text", "text", "text", "text", "text"))

            if (!any(is.na(site))) {
                f <- f %>% dplyr::filter(stringr::str_detect(str_to_lower(Site), paste(str_to_lower(site), collapse = '|')))
            }
            if (!any(is.na(splname))) {
                f <- f %>% dplyr::filter(stringr::str_detect(str_to_lower(Species), paste(str_to_lower(splname), collapse = '|')))
            }
            if (!any(is.na(subsite))) {
                f <- f %>% dplyr::filter(stringr::str_detect(str_to_lower(Subsite), paste(str_to_lower(subsite), collapse = '|')))
            }

            f

        } else if (verpathn == "Seabird2000_22_10_10_cef01") {

            filename <- file.path(dspathn, dssection, verpathn, "Seabird2000_22_10_10_processed.csv")

            colony.meta <- read.csv(filename, sep=";")

            colony.meta <- colony.meta[colony.meta$Species == spcode,]

            ## Added 13 Dec 2022 - TEMPORARY ADJUSTMENT
            #colony.meta$Pairs <- adjust_s2000_colonysizes(colsizes = colony.meta$Pairs, spcode = spcode)

            if(is.null(colony.meta$colonyID)){ colony.meta$colonyID <- colony.meta$id } ## TEMP!!!!!

            colony.meta

        }

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_s2000)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_s2000)")})


    return(result)
}

## ########################################################################################
#' @title Import the SPA polygons for Great Britain
#'
#' @description This dataset is a list of Special Protection Areas for Great Britain
#'   including polygons (OSGB36)
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse,
#'   https://CRAN.R-project.org/package=sf
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param fname The stub name for the shapefile (without the extension)
#' @param sitecode Vector of site codes for the required SPAs
#'
#' @return A tibble with 8 variables: OBJECTID, SITECODE, SITENAME, Shape_Leng,
#'   Shape_Area, Country, STATUS, geometry
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#'
#' @examples
#'   read_spapolygonsgb()
#'   read_spapolygonsgb(sitecode = c("UK9013061","UK9020323"))
#'
#' @noRd

read_spapolygonsgb <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                               dssection = file.path("02_Colony","205_SPAPolygons_GB"),
                               verpathn = "GB-SPA-OSGB36-20210219",
                               fname = "GB_SPA_OSGB36_20210209",
                               sitecode = c("")) {

    SITECODE <- NULL

    result <- tryCatch({

        f <- sf::read_sf(file.path(dspathn, dssection, verpathn, paste0(fname,".shp"))) %>%
            dplyr::filter(stringr::str_detect(SITECODE, paste(sitecode, collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_spapolygonsgb)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_spapolygonsgb)")})

    return(result)
}

## ########################################################################################
#' @title Import the SPA polygons for Northern Ireland
#'
#' @description This dataset is a list of Special Protection Areas for Northern Ireland,
#'   simple feature collection with 16 features and 3 fields. The function imports the
#'   dataset and reprojects to OSGB 1936 / British National Grid.
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse,
#'   https://CRAN.R-project.org/package=sf
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param fname The stub name for the shapefile (without the extension)
#' @param sitecode Vector of site codes for the required SPAs
#'
#' @return A tibble with 3 variables: SITECODE, SITENAME, geometry
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#'
#' @examples
#'   read_spapolygonsni()
#'   read_spapolygonsni(sitecode = c("UK9020051","UK9020290"))
#'
#' @noRd

read_spapolygonsni <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                               dssection = file.path("02_Colony","206_SPAPolygons_NI"),
                               verpathn = "NI-SPA-TM65-20171114",
                               fname = "NI_SPA_TM65_20171114",
                               sitecode = c("")) {

    SITECODE <- NULL

    result <- tryCatch({

        f <- sf::read_sf(file.path(dspathn, dssection, verpathn, paste0(fname,".shp"))) %>%
            dplyr::filter(stringr::str_detect(SITECODE, paste(sitecode, collapse = '|')))%>%
            sf::st_transform(27700)

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_spapolygonsni)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_spapolygonsni)")})

    return(result)
}

## ########################################################################################
#' @title Import the SPA coordinates as used in the CEF
#'
#' @description Some parts of the Cumulative Effects Framework need a point
#'   location for each colony or SPA and this point must be located in a region
#'   designated as sea (outside the coastline polygon). As this set of points is
#'   specifically for the CEF, and may differ from other location datasets, the
#'   coordinates are stored as a separate dataset. This function imports the
#'   dataset.
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname The stub name for the dataset
#' @param sitecode Vector of site codes for the required SPAs
#'
#' @importFrom magrittr %>%
#' @importFrom readr cols col_integer col_logical col_character col_double
#'
#' @return A tibble with 23 variables:
#'
#' @examples
#'   read_spacoords()
#'   read_spacoords(sitecode = c("UK9005012", "UK9001431"))
#'
#' @noRd

read_spacoords <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                           dssection = file.path("02_Colony","207_SPAcoords"),
                           verpathn = "july22",
                           dname = "spacoordinates",
                           sitecode = c("")) {

    SITECODE <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 .default = col_double(),
                                 SITECODE = col_character(),
                                 CEF.include = col_logical(),
                                 Marine = col_logical(),
                                 dat.ATSEA = col_logical(),
                                 flt.ATSEA = col_logical(),
                                 flt.near = col_logical(),
                                 datxy.ATSEA = col_logical(),
                                 fltxy.ATSEA = col_logical(),
                                 fltxy.near = col_logical()
                             )
        ) %>%
            dplyr::filter(stringr::str_detect(SITECODE, paste(sitecode, collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_spacoords)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_spacoords)")})

    return(result)
}

## ########################################################################################
#' @title Import the SPA Site Details dataset
#'
#' @description <description>
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param sitecode Vector of site codes for the required SPAs
#'
#' @return <return>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_upper
#' @importFrom readr cols col_character col_integer col_double
#'
#' @examples
#'   read_spasitedetails()
#'
#' @noRd

read_spasitedetails <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                dssection = file.path("02_Colony","208_SPASiteDetails"),
                                verpathn = "v2020-12-18",
                                dname = "spasitedetails",
                                sitecode = c("")) {

    SITE_CODE <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 .default = col_character(),
                                 IS_MARINE = col_integer(),
                                 `SITE_MARINE_AREA % of site` = col_double(),
                                 Zone = col_character()))%>%
            dplyr::filter(stringr::str_detect(str_to_upper(SITE_CODE), paste(str_to_upper(sitecode), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_spasitedetails)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_spasitedetails)")})

    return(result)
}

## ########################################################################################
#' @title Import the SPA Interest Features dataset
#'
#' @description <description>
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param splname Species name(s) (latin)
#' @param sitecode Code name(s) for the site
#'
#' @return <return>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower str_to_upper
#'
#' @examples
#'   read_spainterestfeatures()
#'   read_spainterestfeatures()$Species
#'   read_spainterestfeatures(splname = c("Rissa tridactyla","Uria aalge"))
#'   read_spainterestfeatures(sitecode = c("UK9003051","UK9001624"))
#'   read_spainterestfeatures(splname = read_specieslist(val = "Puffin")$LATIN_NAME)
#'
#' @noRd

read_spainterestfeatures <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                     dssection = file.path("02_Colony","209_SPAInterestFeatures"),
                                     verpathn = "v2020-12-18",
                                     dname = "spainterestfeatures",
                                     splname = c(""),
                                     sitecode = c("")) {

    Species <- SITE_CODE <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 .default = col_character(),
                                 X_coord = col_double(),
                                 Y_coord = col_double(),
                                 IS_MARINE = col_integer())) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Species), paste(str_to_lower(splname), collapse = '|'))) %>%
            dplyr::filter(stringr::str_detect(str_to_upper(SITE_CODE), paste(str_to_upper(sitecode), collapse = '|')))


    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_spainterestfeatures)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_spainterestfeatures)")})

    return(result)
}

## ########################################################################################
#' @title Import the basic seabird parameter dataset
#'
#' @description <description>
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and
#'   fields_dname.csv)
#' @param spcode Two-letter species code(s)
#' @param spname Common name(s) for the seabird species. Leave blank to return
#'   all species.
#'
#' @return <return>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_basicbird()
#'   read_basicbird(spcode = c("E.","Pu"))
#'   read_basicbird(spname = c("Eider","Puffin"))
#'
#' @noRd

read_basicbird <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                           dssection = file.path("03_Bird","301_BasicBird"),
                           verpathn = "v1.0",
                           dname = "basicbirddata",
                           spcode = c(""),
                           spname = c("")) {

    SHORT_CODE <- Species <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols())  %>%
            dplyr::filter(stringr::str_detect(str_to_lower(SHORT_CODE), paste(str_to_lower(spcode), collapse = '|'))) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Species), paste(str_to_lower(spname), collapse = '|')))


    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (basicbird)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (basicbird)")})

    return(result)
}

## ########################################################################################
#' @title Import the seabird foraging range dataset
#'
#' @description <description>
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param spcode Two-letter species code(s)
#'
#' @return A tibble with 10 variables: SITE_CODE <chr>, SPA <chr>, Subsite name
#'   <chr>, SHORT_CODE <chr>, Receptor <chr>, ForagingRangeMean <dbl>,
#'   ForagingRangeMeanSD <dbl>, ForagingRangeMeanMax <dbl>,
#'   ForagingRangeMeanMaxSD <dbl>, ForagingRangeMaxMax <dbl>
#'
#' @importFrom stringr str_detect str_to_lower
#' @importFrom readr cols col_character col_double
#'
#' @examples
#'   read_foragingranges()
#'   read_foragingranges(spcode = "E.")
#'   read_foragingranges(spcode = read_specieslist(val = "Arctic Skua")$SHORT_CODE)
#'
#' @noRd

read_foragingranges <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                dssection = file.path("03_Bird","302_ForagingRanges"),
                                verpathn = "v1.0",
                                dname = "foragingrangedata",
                                spcode = c("")) {

    SHORT_CODE <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 SITE_CODE = col_character(),
                                 SPA = col_character(),
                                 `Subsite name` = col_character(),
                                 SHORT_CODE = col_character(),
                                 Receptor = col_character(),
                                 ForagingRangeMean = col_double(),
                                 ForagingRangeMeanSD = col_double(),
                                 ForagingRangeMeanMax = col_double(),
                                 ForagingRangeMeanMaxSD = col_double(),
                                 ForagingRangeMaxMax = col_double())) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(SHORT_CODE), paste(str_to_lower(spcode), collapse = '|')))


    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (foragingranges)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (foragingranges)")})

    return(result)
}

## ########################################################################################
#' @title Import the seabird survival dataset
#'
#' @description <description>
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and
#'   fields_dname.csv)
#' @param spcode Two-letter species code(s)
#'
#' @return A tibble with 29 variables: SHORT_CODE <chr>, Species <chr>, Age
#'   class <chr>, Age.lo <dbl>, Age.hi <dbl>, Source <chr>, Source_old_version
#'   <chr>, Type <chr>, Study area <chr>, Study Period <chr>, Region <chr>,
#'   Country <chr>, Reference <chr>, Data_collection_method <chr>, Estimation
#'   method <chr>, study.nyears <dbl>, Study.Mean <dbl>, Study.SD.Orig <dbl>,
#'   Study.SE.Orig <dbl>, Study.SD <dbl>, Number.of.studies <dbl>, Weighted?
#'   <chr>, Nat.Mean.HRN <dbl>, Nat.Mean <dbl>, Nat.SD.HRN <dbl>, Nat.SD <dbl>,
#'   Comparison.to.HRN <chr>, SV.mean <dbl>, SV.SD <dbl>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_survival()
#'   read_survival(spcode = "PU")
#'   read_survival(spcode = read_specieslist(val = "Arctic Skua")$SHORT_CODE)
#'
#' @noRd

read_survival <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                          dssection = file.path("03_Bird","303_Survival"),
                          verpathn = "v1.0",
                          dname = "survivaldata",
                          spcode = c("")) {

    SHORT_CODE <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 .default = col_character(),
                                 Age.lo = col_double(),
                                 Age.hi = col_double(),
                                 study.nyears = col_double(),
                                 Study.Mean = col_double(),
                                 Study.SD.Orig = col_double(),
                                 Study.SE.Orig = col_double(),
                                 Study.SD = col_double(),
                                 Number.of.studies = col_double(),
                                 Nat.Mean.HRN = col_double(),
                                 Nat.Mean = col_double(),
                                 Nat.SD.HRN = col_double(),
                                 Nat.SD = col_double(),
                                 SV.mean = col_double(),
                                 SV.SD = col_double())) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(SHORT_CODE), paste(str_to_lower(spcode), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (survival)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (survival)")})

    return(result)
}

## ########################################################################################
#' @title Import the seabird productivity dataset
#'
#' @description <description>
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param spcode Two-letter species code(s)
#'
#' @return A tibble with 10 variables: Regclass <chr>, SHORT_CODE <chr>, Species
#'   <chr>, Region <chr>, nsites <dbl>, nyears <dbl>, BS.mean <dbl>, BS.sd1
#'   <dbl>, BS.sd2 <dbl>, BS.sd <dbl>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_productivity()
#'   read_productivity(spcode = "PU")
#'   read_productivity(spcode = read_specieslist(val = "Arctic Skua")$SHORT_CODE)

#'
#' @noRd

read_productivity <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                              dssection = file.path("03_Bird","304_Productivity"),
                              verpathn = "v1.0",
                              dname = "productivitydata",
                              spcode = c("")) {

    SHORT_CODE <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 .default = col_character(),
                                 nsites = col_double(),
                                 nyears = col_double(),
                                 BS.mean = col_double(),
                                 BS.sd1 = col_double(),
                                 BS.sd2 = col_double(),
                                 BS.sd = col_double())) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(SHORT_CODE), paste(str_to_lower(spcode), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (productivity)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (productivity)")})

    return(result)
}

## ########################################################################################
#' @title Import the seabird flight speed dataset
#'
#' @description <description>
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and
#'   fields_dname.csv)
#' @param spcode Two-letter species code(s)
#' @param splname Species name(s) (latin)
#'
#' @return A tibble with 11 variables: Source <chr>, Species <chr>, Flight_Type
#'   <chr>, Flight_Speed_Mean <dbl>, Flight_Speed_SD <dbl>, Vertical_Speed_Mean
#'   <dbl>, Min_Power_Speed <dbl>, Max_Range_Speed <dbl>, N_observations <dbl>,
#'   Track_Time <dbl>, Further_info <chr>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_flightspeeds()
#'   read_flightspeeds(spcode = "PU")
#'   read_flightspeeds(splname = "Rissa tridactyla")
#'   read_flightspeeds(splname = read_specieslist(val = "Atlantic Puffin")$LATIN_NAME)
#'

#' @noRd
read_flightspeeds <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                              dssection = file.path("03_Bird","305_FlightSpeeds"),
                              verpathn = "v1.0",
                              dname = "flightspeeddata",
                              spcode = c(""),
                              splname = c("")) {

    SHORT_CODE <- Species <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 Further_info = col_character())) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Species_Code), paste(str_to_lower(spcode), collapse = '|'))) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Species), paste(str_to_lower(splname), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (flightspeeds)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (flightspeeds)")})

    return(result)
}

## ########################################################################################
#' @title Import the seabird nocturnal activity dataset
#'
#' @description <description>
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param spcode Two-letter species code(s)
#' @param spname Common name(s) for the species of interest
#'
#' @return A tibble with 5 variables: Source <chr>, Species <chr>, Season <chr>,
#'   Nocturnal.Activity.Score <dbl>, Nocturnal_Activity_Percent <dbl>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_nocturnalactivity()
#'   read_nocturnalactivity(spcode = c("E.","pu"))
#'   read_nocturnalactivity(spname = c("Eider","Black Tern"))
#'   read_nocturnalactivity(spname = read_specieslist(val = "E.")$ENGLISH_NAME)
#'
#' @noRd

read_nocturnalactivity <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                   dssection = file.path("03_Bird","306_NocturnalActivity"),
                                   verpathn = "v1.0",
                                   dname = "nocturnalactivitydata",
                                   spcode = c(""),
                                   spname = c("")) {

    SHORT_CODE <- Species <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 .default = col_character(),
                                 Nocturnal.Activity.Score = col_double(),
                                 Nocturnal_Activity_Percent = col_double())) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(SHORT_CODE), paste(str_to_lower(spcode), collapse = '|'))) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Species), paste(str_to_lower(spname), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (nocturnalactivity)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (nocturnalactivity)")})

    return(result)
}

## ########################################################################################
#' @title Import the seabird displacement dataset
#'
#' @description <description>
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param spname Common name(s) for the species of interest
#'
#' @return <return>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_displacement()
#'   read_displacement(spname = c("Eider","Black Tern"))
#'   read_displacement(spname = read_specieslist(val = "GB")$ENGLISH_NAME)
#'
#' @noRd

read_displacement <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                              dssection = file.path("03_Bird","307_Displacement"),
                              verpathn = "v1.0",
                              dname = "displacement",
                              spname = c("")) {
    Species <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 Species = col_character(),
                                 Displacement_Value = col_double()))  %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Species), paste(str_to_lower(spname), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (displacement)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (displacement)")})

    return(result)
}

## ########################################################################################
#' @title Import the seabird avoidance rates dataset
#'
#' @description <description>
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and
#'   fields_dname.csv)
#' @param spname Common name(s) for the species of interest
#'
#' @return A tibble with 5 variables: Species <chr>, Source <chr>,
#'   Avoidance_Model_Type <chr>, Avoidance_Rate <dbl>, Avoidance_Rate_SD <dbl>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_avoidancerates()
#'   read_avoidancerates(spcode = "KI")
#'
#' @noRd

read_avoidancerates <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                dssection = file.path("03_Bird","308_AvoidanceRates"),
                                verpathn = "v1.0",
                                dname = "avoidancerates",
                                spcode = c("")) {

    Species <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols())   %>%
            dplyr::filter(stringr::str_detect(str_to_lower(SHORT_CODE), paste(str_to_lower(spcode), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (avoidancerates)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (avoidancerates)")})

    return(result)
}

## ########################################################################################
#' @title Import the seabird energetics and prey-related parameters dataset
#'
#' @description Read in the energy-related parameters for seabird species. This
#'   is used by the SeabORD model.
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param spcode Species two-letter code
#'
#' @return A tibble with 28 variables: Code <chr>, BM_adult_mn <dbl>,
#'   BM_adult_sd <dbl>, BM_adult_mortf <dbl>, BM_adult_abdn <dbl>, BM_chick_mn
#'   <dbl>, BM_chick_sd <dbl>, BM_Chick_mortf <dbl>, daylength <dbl>,
#'   seasonlength <dbl>, unattend_max_hrs <dbl>, adult_DEE_mn <dbl>,
#'   adult_DEE_sd <dbl>, chick_DER <dbl>, IR_max <dbl>, IR_half_a <dbl>,
#'   IR_half_b <dbl>, flight_msec <dbl>, assim_eff <dbl>, energy_prey <dbl>,
#'   energy_nest <dbl>, energy_flight <dbl>, energy_searest <dbl>, energy_forage
#'   <dbl>, energy_warming <dbl>, chick_mass_a <dbl>, adult_mass_KG <dbl>, beta
#'   <dbl>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_energeticsprey()
#'   read_energeticsprey(spcode = c("KI","RA"))
#'
#' @noRd

read_energeticsprey <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                dssection = file.path("03_Bird","309_EnergeticsPrey"),
                                verpathn = "v1.0",
                                dname = "energeticsandpreydata",
                                spcode = c("")) {
    Code <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 .default = col_double(),
                                 Code = col_character(),
                                 Species = col_character()))  %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Code), paste(str_to_lower(spcode), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (energeticsprey)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (energeticsprey)")})

    return(result)
}

## ########################################################################################
#' @title Import the seabird seasonal activity dataset
#'
#' @description Import the seasonal activity for one or more species, using the two-letter
#'   species code.
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param spcode Two-letter code for the seabird species. Leave blank to return all species.
#'
#' @return <return>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower str_to_upper
#' @importFrom readr col_character col_factor
#'
#' @examples
#'   read_seasonalactivity()
#'   read_seasonalactivity(spcode = c("AC","F."))
#'   read_seasonalactivity(spcode = read_specieslist(val = "Atlantic Puffin")$SHORT_CODE)
#'

#' @noRd
read_seasonalactivity <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                  dssection = file.path("03_Bird","310_SeasonalActivity"),
                                  verpathn = "v1.0",
                                  dname = "seasonalactivity",
                                  spcode = c("")) {

    Speccode <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             skip_empty_rows = TRUE,
                             col_types = readr::cols(
                                 Source = col_character(),
                                 Receptor = col_character(),
                                 Speccode = col_character(),
                                 Season = col_character(),
                                 Appropriate_BDMPS_season = col_character(),
                                 Month = col_factor(levels = month.name),
                                 Notes = col_character())) %>%
            dplyr::filter(stringr::str_detect(Speccode, paste(stringr::str_to_upper(spcode), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (seasonalactivity)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (seasonalactivity)")})

    return(result)
}

## ########################################################################################
#' @title Import the seabird flight height dataset
#'
#' @description This function is used to read the required flight height data file from the
#'   Data Store. There is a separate file for each bird species, identified by the SHORT_CODE
#'   for the species (a 2-letter code).
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version of the data
#' @param spname The species to extract from the Data Store, using the name format that matches
#'   the file names (e.g. <name>_ht_dflt.csv). See SpeciesList$sCRM_NAME.
#'
#' @return A list containing a tibble (500 x 201) for each requested species
#'
#' @examples
#'   read_flightheight(spname = c("Northern_Fulmar","Eider"))
#'   read_flightheight(spname = read_specieslist(val = "PU")$sCRM_NAME)
#'
#' @noRd

read_flightheight <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                              dssection = file.path("03_Bird","311_FlightHeight"),
                              verpathn = "v1.0",
                              spname) {

    result <- lapply(spname, function(x) {
        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(x,"_ht_dflt.csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols())
    })
    names(result) <- sapply(spname, function(x) paste(x[1]))

    return(result)
}

## ########################################################################################
#' @title Import the Sensitivity Scores dataset
#'
#' @description <description>
#'
#' Required package description
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param spcode Species code(s)
#'
#' @return <return>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_sensitivityscores()
#'
#' @noRd

read_sensitivityscores <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                   dssection = file.path("03_Bird","312_SensitivityScores"),
                                   verpathn = "v1.0",
                                   dname = "sensitivityscores",
                                   spcode = c("")) {

    Specie_Code <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols())  %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Species_Code), paste(str_to_lower(spcode), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_sensitivityscores)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_sensitivityscores)")})

    return(result)
}

## ########################################################################################
#' @title Import the BDMPS 'imprat' dataset
#'
#' @description <description>
#'
#' Required package description
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param spcode Two-letter code for the seabird species. Leave blank to return all species.
#'
#' @return <return>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_bdmpsimprat()
#'   read_bdmpsimprat(spcode = "GX")
#'   read_bdmpsimprat(spcode = read_specieslist(val = "Puffin")$SHORT_CODE)
#'

#' @noRd
read_bdmpsimprat <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                             dssection = file.path("03_Bird","313_BDMPSimprat"),
                             verpathn = "apr2021",
                             dname = "bdmpsimmaturesratio",
                             spcode = c("")) {

    Speccode <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols())  %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Speccode), paste(str_to_lower(spcode), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_bdmpsimprat)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_bdmpsimprat)")})

    return(result)
}

## ########################################################################################
#' @title Import the offshore renewable development name dataset
#'
#' @description This function loads the dataset holds the list of Offshore
#'   renewable Developments (ORDs), their full names and 5-letter codes used in
#'   the CEF and the corresponding name used in the Emodnet polygon dataset.
#'   Alternative names may be added as a look-up table for other functions.
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param pjcode Project code(s) (5-letter code). Returns all if left blank.
#'
#' @return A tibble with 5 variables: code, name, Emodnet_Site_Name,
#'   Emodnet_PolygonID, Emodnet_Status
#'
#' @examples
#'   read_ordnames()
#'   read_ordnames(pjcode = c("WALN1", "BARRO"))
#'   read_ordnames()$code
#'
#' @noRd

read_ordnames <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                          dssection = file.path("05_ORD","501_ORDNames"),
                          verpathn = "v1.1",
                          dname = "offshorerenewabledevelopmentnames",
                          pjcode = c()) {

    code <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols(
                                 .default = col_character(),
                                 Emodnet_PolygonID = col_integer()
                             )) %>%
            dplyr::filter(stringr::str_detect(code, paste(stringr::str_to_upper(pjcode), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (ordnames)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (ordnames)")})

    return(result)
}

## ########################################################################################
#' @title Import the windfarm polygon dataset
#'
#' @description This function loads the Offshore Renewable Development (ORD)
#'   polygons from a copy of the Emodnet dataset. Returns the whole dataset or
#'   can be filtered for country or ORD name.
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param gdbname Name of the gdb file for the required version
#' @param lname Name for the layer required
#' @param country Filter the results for one or more country names (default UK)
#' @param ordname Filter the results for one or more named
#'
#' @return Simple feature collection with n features and 11 fields
#'
#' @examples
#'   read_ordsites()
#'   read_ordsites(country = c())
#'   read_ordsites(ordname = c("Walney 1 Wind Farm", "Galloper Wind Farm"))
#'   read_ordsites(country = "France")$NAME
#'

#' @noRd
read_ordsites <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                          dssection = file.path("05_ORD","502_Sites"),
                          verpathn = "EMODnet_HA_WindFarms_20220324",
                          gdbname = "EMODnet_HA_WindFarms_20220324",
                          lname = "EMODnet_HA_WindFarms_pg_20220324",
                          country = c("United Kingdom"),
                          ordname = c("")) {

    NAME <- COUNTRY <- NULL

    lfn <- function(x) {
        return(str_to_lower(gsub("\\(|\\)", "", x)))
    }

    result <- tryCatch({

        fgdb <- file.path(dspathn, dssection, verpathn, paste0(gdbname,".gdb"))
        f <- sf::read_sf(dsn = fgdb, layer = lname, quiet = TRUE) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(COUNTRY), paste(str_to_lower(country), collapse = '|'))) %>%
            dplyr::filter(stringr::str_detect(lfn(NAME), paste(lfn(ordname), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (ordsites)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (ordsites)")})

    return(result)
}


## ########################################################################################
#' @title Import the turbine parameter dataset
#'
#' @description <description>
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param ordcode The 5-letter code(s) for the required ORDs
#'
#' @return A tibble with 25 variables
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_turbine()
#'

#' @noRd
read_turbine <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                         dssection = file.path("05_ORD","503_Turbine"),
                         verpathn = "v1.0",
                         dname = "windfarmturbineparameterdata",
                         ordcode = c("")) {

    Site_Code <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols()) %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Site_Code), paste(str_to_lower(ordcode), collapse = '|')))


    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (turbine)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (turbine)")})

    return(result)
}

## ########################################################################################
#' @title Import the seabird density and abundance per windfarm datasets
#'
#' @description These datasets contains density and abundance data relating to the
#'   pre-construction and/or baseline ornithology surveys undertaken for the Environmental
#'   Impact Assessments.
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version.
#' @param spname Common name(s) for species of interest. Leave blank to return
#'   all species.
#' @param ordcode The 5-letter code(s) for the ORDs to extract from the Data
#'   Store, using the name format that matches the file names (e.g.
#'   density_<name>.csv).
#'
#' @return A list with a tibble for each ORD, each with 22 variables: Site
#'   <chr>, Site_Code <chr>, Species <chr>, Period <chr>, Year <dbl>,
#'   Survey_Number <dbl>, Behaviour <chr>, Area <chr>, Buffer_Size <chr>,
#'   Area_Extent <dbl>, Survey_Type <chr>, Measurement_Type <chr>, Estimate_Type
#'   <chr>, Unidentified_Allocated <chr>, Correction_Method <chr>,
#'   Availability_Bias <chr>, Estimate <dbl>, LCL <dbl>, UCL <dbl>, CV <dbl>, SD
#'   <dbl>, SE <dbl>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_orddensity(ordcode = c("BARRO","GUNFL"))
#'   read_orddensity(ordcode = c("BARRO","BEATR","WALN1"), spname = "Cormorant")
#'
#' @noRd

read_orddensity <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                            dssection = file.path("05_ORD","504_Density"),
                            verpathn = "v1.0",
                            spname = c(""),
                            ordcode) {

    Species <- NULL

    result <- tryCatch({

        result <- lapply(ordcode, function(x) {
            f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0("density_",x,".csv")),
                                 col_names = TRUE,
                                 skip_empty_rows = TRUE,
                                 trim_ws = TRUE, skip = 0, n_max = Inf,
                                 locale = readr::locale(encoding = "UTF-8"),
                                 col_types = readr::cols(
                                     .default = col_character(),
                                     Year = col_double(),
                                     Survey_Number = col_double(),
                                     #Buffer_Size = col_double(),
                                     Area_Extent = col_double(),
                                     Estimate = col_double(),
                                     LCL = col_double(),
                                     UCL = col_double(),
                                     CV = col_double(),
                                     SD = col_double(),
                                     SE = col_double())) %>%
                dplyr::filter(stringr::str_detect(str_to_lower(Species), paste(str_to_lower(spname), collapse = '|')))

        })
        names(result) <- sapply(ordcode, function(x) paste(x[1]))

        result

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (orddensity)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (orddensity)")})

    return(result)
}

## ########################################################################################
#' @title Import the collision rate model output files
#'
#' @description These datasets contain results from the Collision Rate Modelling for the
#'   each wind farm, i.e. the number of predicted mortalities due to collision with
#'   turbines.
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param spname Common name(s) for species of interest. Leave blank to return
#'   all species.
#' @param ordcode The 5-letter code(s) for the ORDs to extract from the Data
#'   Store, using the name format that matches the file names (e.g.
#'   CRMOutputs_<name>.csv).
#'
#' @return A list with a tibble for each ORD with 18 variables: Site <chr>,
#'   Site_Code <chr>, Species <chr>, Scenario <chr>, Avoidance_Rate <dbl>,
#'   Avoidance_Rate_SD <dbl>, Recommended_AR <dbl>, Flight_Data <chr>, Model
#'   <chr>, Model_Option <int>, Period <chr>, Year <chr>, Age <chr>, Estimate
#'   <dbl>, LCL <dbl>, UCL <dbl>, SD <dbl>, SE <dbl>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'  read_crmoutputs(ordcode = "DOGGA")
#'  read_crmoutputs(ordcode = c("DOUNR","BEATR"), spname = "Kittiwake")
#' @noRd


read_crmoutputs <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                            dssection = file.path("05_ORD","505_CRMOutputs"),
                            verpathn = "v1.0",
                            spname = c(""),
                            ordcode) {

    Species <- NULL

    result <- tryCatch({

        result <- lapply(ordcode, function(x) {
            f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0("CRMOutputs_",x,".csv")),
                                 col_names = TRUE,
                                 skip_empty_rows = TRUE,
                                 trim_ws = TRUE, skip = 0, n_max = Inf,
                                 locale = readr::locale(encoding = "UTF-8"),
                                 col_types = readr::cols(
                                     .default = col_character(),
                                     Scenario = col_character(),
                                     Avoidance_Rate = col_double(),
                                     Avoidance_Rate_SD = col_double(),
                                     Recommended_AR = col_double(),
                                     Model_Option = col_integer(),
                                     Year = col_character(),
                                     Age = col_character(),
                                     Estimate = col_double(),
                                     LCL = col_double(),
                                     UCL = col_double(),
                                     SD = col_double(),
                                     SE = col_double())) %>%
                dplyr::filter(stringr::str_detect(str_to_lower(Species), paste(str_to_lower(spname), collapse = '|')))

        })
        names(result) <- sapply(ordcode, function(x) paste(x[1]))

        result



    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (crmoutput)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (crmoutput)")})

    return(result)
}

## ########################################################################################
#' @title Import the additional flight height dataset
#'
#' @description These datasets mostly contain data relating to the observed flight heights
#'   of birds during the pre-construction and/or baseline ornithology surveys.
#'
#' Required package description https://CRAN.R-project.org/package=tidyverse
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param spcode Two-letter code(s) for species of interest. Leave blank to return
#'   all species.
#' @param ordcode The 5-letter code(s) for the ORDs to extract from the Data
#'   Store, using the name format that matches the file names (e.g.
#'   density_<name>.csv).
#'
#' @return A list holding one tibble for each ORD, with 14 variables: Site
#'   <chr>, Site_Code <chr>, Species <chr>, Species_Code <chr>, Period <chr>,
#'   Year <dbl>, Survey_Number <dbl>, Survey_Type <chr>, Measure <chr>,
#'   Height_Min <dbl>, Height_Max <dbl>, Total_birds <chr>, N_birds <chr>, Prop
#'   <dbl>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_addflight(ordcode = c("BARRO"))
#'   read_addflight(ordcode = c("FWIND","KTFEX"), spcode = "KI")
#'
#' @noRd

read_addflight <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                           dssection = file.path("05_ORD","506_AddFlight"),
                           verpathn = "v1.0",
                           spcode = c(""),
                           ordcode) {

    Species_Code <- NULL

    result <- tryCatch({

        result <- lapply(ordcode, function(x) {
            f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0("addflight_",x,".csv")),
                                 col_names = TRUE,
                                 skip_empty_rows = TRUE,
                                 trim_ws = TRUE, skip = 0, n_max = Inf,
                                 locale = readr::locale(encoding = "UTF-8"),
                                 col_types = readr::cols(
                                     .default = col_character(),
                                     Period = col_character(),
                                     Year = col_double(),
                                     Survey_Number = col_double(),
                                     #Survey_Type = col_character(),
                                     Height_Min = col_double(),
                                     Height_Max = col_double(),
                                     #Total_Birds = col_double(),
                                     #N_Birds = col_double(),
                                     Prop = col_double())) %>%
                dplyr::filter(stringr::str_detect(str_to_lower(Species_Code), paste(str_to_lower(spcode), collapse = '|')))

        })
        names(result) <- sapply(ordcode, function(x) paste(x[1]))

        result


    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (addflight)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (addflight)")})

    return(result)
}

## ########################################################################################
#' @title Import the ORD Periods dataset
#'
#' @description <description>
#'
#' Required package description
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#'
#' @return <return>
#'
#' @examples
#'   read_ordperiods()
#'
#' @noRd

read_ordperiods <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                            dssection = file.path("05_ORD","506_ORDPeriods"),
                            verpathn = "v1.0",
                            dname) {

    result <- tryCatch({


    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_ordperiods)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_ordperiods)")})

    return(result)
}


## ########################################################################################
#' @title Reading and Cropping Coastline Data from the Data Store
#'
#' @description The Data Store holds polygons for the coastline for a rectangle
#'   centred on the UK. This functions reads the chosen file.
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn The name of the folder holding the desired version.
#' @param crsname CRS version to read. Either "4326" (default, full extent)
#'   or "3035" (cropped).
#'
#' @return A SpatialPolygonsDataFrame (version GADM_UK_01) or Simple feature
#'   collection (version GADM_UKplus_40)
#'
#' @examples
#'    read_coastline()
#'
#' @noRd

read_coastline <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                           dssection = file.path("06_Geographic","601_Coastline"),
                           verpathn = "GADM_UKplus_40",
                           crsname = "4326") {

    # Load the base coastline dataset & crop

    if (verpathn == "GADM_UK_01") {

        result <- tryCatch({

            readRDS(file.path(dspathn, dssection, verpathn, "UK_01_adm0.rds"))

        }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (coast)")
        }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (coast)")})

    } else if (verpathn == "GADM_UKplus_40") {

        result <- tryCatch({

            readRDS(file.path(dspathn, dssection, verpathn, paste0("cef_coast_",crsname,".rds")))

        }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (coast)")
        }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (coast)")})

    }

    return(result)

}

## ########################################################################################
#' @title Import the Flight Path Transition Layer
#'
#' @description <description>
#'
#' Required package description - gdistance
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset
#' @param tltype Name for the required Transition layer ("", "correction", "corrected")
#'
#' @return Large TransitionLayer
#'
#' @examples
#'   read_flightpathtl()
#'
#' @noRd

read_flightpathtl <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                              dssection = file.path("06_Geographic","602_FlightPathTL"),
                              verpathn = "v1.0",
                              dname = "FlightGrid",
                              tltype = "correction"
) {

    result <- tryCatch({

        filename <- file.path(dspathn, dssection, verpathn, paste0(dname,tltype,"_3035.RData"))

        temp.space <- new.env()
        foo <- load(filename, temp.space)
        TL <- get(foo, temp.space)
        rm(temp.space)

        TL

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_flightpathtl)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_flightpathtl)")})

    return(result)
}

## ########################################################################################
#' @title Import the Fine Grid dataset
#'
#' @description The area covered by the CEF is divided into land and sea cells.
#'   This dataset holds the definition of the finest grid, approximately 1km by
#'   1km cells. The default CRS is EPSG:4326, but the user can specify 3
#'   alternative forms (see examples).
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param crsname The coordinate reference system to select which file to return
#' @param ext Temporary option to read 'grd' or 'tif' for testing
#'
#' @importFrom raster raster
#'
#' @return A RasterLayer holding the land/sea mask, sea = NA, land = 0
#'
#' @examples
#'   read_finegrid()
#'   read_finegrid(crsname = "epsg27700")
#'   read_finegrid(crsname = "epsg3035")
#'   read_finegrid(crsname = "epsgbird")
#'   read_finegrid(crsname = "epsg4326", ext = 'tif')
#'

#'
#' @noRd
read_finegrid <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                          dssection = file.path("06_Geographic","603_FineGrid"),
                          verpathn = "v1.0",
                          crsname = "epsg4326",
                          ext = "grd") {

    if (crsname == "epsg27700") {
        result <- raster(file.path(dspathn, dssection, verpathn, paste0("seamask_27700",".", ext)))
        result <- readAll(result)
    } else if (crsname == "epsg3035") {
        result <- raster(file.path(dspathn, dssection, verpathn, paste0("seamask_3035",".", ext)))
        result <- readAll(result)
    } else if (crsname == "epsgbird") {
        result <- raster(file.path(dspathn, dssection, verpathn, paste0("seamask_bird",".", ext)))
        result <- readAll(result)
    } else if (crsname == "epsg4326"){
        result <- raster(file.path(dspathn, dssection, verpathn, paste0("seamask_4326",".", ext)))
        result <- readAll(result)
    }

    return(result)
}

## ########################################################################################
#' @title Import the At-sea Grid dataset
#'
#' @description Function to read the 'at-sea' grid definition
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset
#'
#' @return Large list (4 elements): tmpl.land:Formal class 'RasterLayer',
#'   spatdat:'data.frame', pars.grid:List of 5, costgrid:Formal class 'TransitionLayer'
#'
#' @examples
#'   read_atseagrid()
#'
#' @noRd

read_atseagrid <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                           dssection = file.path("06_Geographic","604_AtseaGrid"),
                           verpathn = "v1.0",
                           dname = "atseagrid"
) {

    # Returned if successful
    gridmeta <- NULL

    result <- tryCatch({

        filename <- file.path(dspathn, dssection, verpathn, paste0(dname, ".RData"))

        load(filename) ## contains object "gridmeta"

        gridmeta

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_atseagrid)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_atseagrid)")})

    return(result)
}

## ########################################################################################
#' @title Import the GPS Grid dataset
#'
#' @description Function to import the GPS grid definition
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset
#' @param spcode Two-letter code for the seabird species.
#'
#' @importFrom stringr str_to_lower str_to_upper
#'
#' @return Large list (4 elements): tmpl.land:Formal class 'RasterLayer',
#'   spatdat:'data.frame', pars.grid:List of 5, costgrid:Formal class 'TransitionLayer'
#'
#' @examples
#'   read_gpsgrid(spcode = "KI")
#'
#' @noRd

read_gpsgrid <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                         dssection = file.path("06_Geographic","605_GPSGrid"),
                         verpathn = "v1.0",
                         dname = "gpsgrid",
                         spcode) {

    gridmeta <- NULL

    result <- tryCatch({

        filename <- file.path(dspathn, dssection, verpathn,
                              paste0(dname, "_", stringr::str_to_upper(spcode),
                                     ".RData"))

        load(filename) ## contains object "gridmeta"

        # Return the object
        gridmeta

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_gpsgrid)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_gpsgrid)")})

    return(result)
}

## ########################################################################################
#' @title Import the distance by sea from SPA, fine Grid dataset
#'
#' @description This dataset is a set of tif files holding RasterLayers
#'   (resolution 1km) covering the Cumulative Effects Framework area around the
#'   UK. Each cell in the raster holds the distance, in km, between the flight
#'   start point for a colony to the cell.
#'
#' Required package description https://CRAN.R-project.org/package=raster
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param crsnum EPSG code for the coordinate reference system (4326 or 3035 only)
#' @param dname Stub name for the dataset (for file name)
#' @param sitecode Code name for the site(s)
#'
#' @importFrom raster raster
#' @importFrom stringr str_trim
#'
#' @return A list of RasterLayers holding the distance by sea (km)
#'
#' @examples
#'   read_distfinegrid2spa(sitecode = "UK9001011")
#'   read_distfinegrid2spa(sitecode = "UK9001011", crsnum = 3035)
#'
#' @noRd

read_distfinegrid2spa <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                  dssection = file.path("06_Geographic","606_DistFineGrid2SPA"),
                                  verpathn = "v1.0",
                                  crsnum = 4326,
                                  dname = "bysea",
                                  sitecode) {

    result <- tryCatch({

        # Construct the required file names
        files <- lapply(sitecode, function(f) {
            paste0(stringr::str_trim(f),"_",dname,"_",as.character(crsnum),".tif")
        }) %>% unlist()

        # See which files are present in the Data Store
        rasterlist <- list.files(path = file.path(dspathn, dssection, verpathn), pattern='.tif',
                                 all.files=TRUE, full.names=FALSE)

        # Only read the files needed for the simulations
        allmaps <- lapply(file.path(dspathn, dssection, verpathn, intersect(files, rasterlist)), raster)

        # Return the list of RasterLayers
        allmaps

        ## WARNING RGDAL WARNING
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_distfinegrid2spa)")})

    return(result)
}

## ########################################################################################
#' @title Import the distance by sea from SPA, fine Grid dataset
#'
#' @description This dataset is a set of tif files holding RasterLayers
#'   (resolution 1km) covering the Cumulative Effects Framework area around the
#'   UK. Each cell in the raster holds the distance, in km, between the flight
#'   start point for a colony to the cell. This function is only used in the
#'   CEF interface and creates a SpatRaster using terra to allow rapid
#'   calculations without loading the tifs into memory.
#'
#' Required package description https://CRAN.R-project.org/package=terra
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param crsnum EPSG code for the coordinate reference system (4326 or 3035 only)
#' @param dname Stub name for the dataset (for file name)
#' @param sitecode Code name for the site(s)
#'
#' @importFrom terra rast
#'
#' @return A SpatRaster holding the distance by sea (km) for the supplied sites
#'
#' @examples
#'   stack_minfinegrid2spa(sitecode = "UK9001011")
#'
#' @noRd

read_minfinegrid2spa <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                 dssection = file.path("06_Geographic","606_DistFineGrid2SPA"),
                                 verpathn = "v1.0",
                                 crsnum = 3035,
                                 dname = "bysea",
                                 sitecode) {

    result <- tryCatch({

        # Construct the required file names
        files <- lapply(sitecode, function(f) {
            paste0(stringr::str_trim(f),"_",dname,"_",as.character(crsnum),".tif")
        }) %>% unlist()

        # See which files are present in the Data Store
        rasterlist <- list.files(path = file.path(dspathn, dssection, verpathn), pattern='.tif',
                                 all.files=TRUE, full.names=FALSE)

        # Only read the files needed for the simulations
        allmaps <- terra::rast(file.path(dspathn, dssection, verpathn,intersect(files, rasterlist)))

        # Return the list of RasterLayers
        allmaps

        ## WARNING RGDAL WARNING
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_minfinegrid2spa)")})

    return(result)
}

## ########################################################################################
#' @title Import the Dist At-sea Grid to S2000 dataset
#'
#' @description Function to import distance by sea from each Seabird 2000
#'   subsite to each cell on the grid used in the modelling of at-sea survey
#'   data
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset
#' @param spcode Two-letter code for the seabird species.
#'
#' @importFrom stringr str_to_upper
#'
#' @return Large matrix
#'
#' @examples
#'   read_distatseagrids2000(spcode = "KI")
#'
#' @noRd

read_distatseagrids2000 <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                    dssection = file.path("06_Geographic","607_DistAtseaGridS2000"),
                                    verpathn = "v1.0",
                                    dname = "distatseagrids2000",
                                    spcode) {

    # Set
    dist2colonybysea <- NULL

    result <- tryCatch({

        filename <-  file.path(dspathn, dssection, verpathn,
                               paste0(dname, "_",
                                      stringr::str_to_upper(spcode), ".RData"))

        # contains object "dist2colonybysea"
        load(filename)

        # return
        dist2colonybysea

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_distatseagrids2000)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_distatseagrids2000)")})

    return(result)
}

## ########################################################################################
#' @title Import the Dist At-sea Grid to S2000 dataset
#'
#' @description Function to import the distance to seabird 2000 sites matrices,
#'   for a specified species.
#'
#' Required package description
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset
#' @param spcode Two-letter code for the seabird species.
#' @param foraging.range The required foraging range, km
#'
#' @importFrom stringr str_to_upper
#'
#' @return Named num - attr(*, "names")
#'
#' @examples
#'   read_prseaatseagrids2000(spcode = "KI", foraging.range = 200)
#'
#' @noRd

read_prseaatseagrids2000 <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                     dssection = file.path("06_Geographic","607_DistAtseaGridS2000"),
                                     verpathn = "v1.0",
                                     dname = "prseaatseagrids2000",
                                     spcode,
                                     foraging.range){
    # Set
    prsea <- NULL

    result <- tryCatch({

        filename <-  file.path(dspathn, dssection, verpathn,
                               paste0(dname, "_",
                                      stringr::str_to_upper(spcode), ".RData"))

        # contains object "prsea"
        load(filename)

        franges <- as.numeric(as.character(colnames(prsea)))
        df <- abs(franges - foraging.range)
        sel.fr <- which(df == min(df))[1]
        prsea <- prsea[,sel.fr]

        # Return
        prsea

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_prseaatseagrids2000)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_prseaatseagrids2000)")})

    return(result)
}

## ########################################################################################
#' @title Import the Dist GPS Grid to S2000 dataset
#'
#' @description <description>
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the data set
#' @param spcode Two-letter code for the seabird species.
#'
#' @importFrom stringr str_to_upper
#'
#' @return Large matrix
#'
#' @examples
#'   read_distgpsgrids2000(spcode = "RA")
#'
#' @noRd

read_distgpsgrids2000 <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                  dssection = file.path("06_Geographic","608_DistGPSGrid2000"),
                                  verpathn = "v1.0",
                                  dname = "distgpsgrids2000",
                                  spcode) {

    # Set
    dist2colonybysea <-  NULL

    result <- tryCatch({

        filename <-  file.path(dspathn, dssection, verpathn,
                               paste0(dname, "_",
                                      stringr::str_to_upper(spcode), ".RData"))

        # contains object "dist2colonybysea"
        load(filename)

        # Return
        dist2colonybysea

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_distgpsgrid2000)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_distgpsgrid2000)")})

    return(result)
}

## ########################################################################################
#' @title Import dataset
#'
#' @description <description>
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the data set
#' @param spcode Two-letter code for the seabird species.
#' @param foraging.range The required foraging range, km
#'
#' @importFrom stringr str_to_upper
#'
#' @return df
#'
#' @examples
#'   read_prseagpsgrids2000(spcode = "RA", foraging.range = 200)
#'
#' @noRd

read_prseagpsgrids2000 <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                   dssection = file.path("06_Geographic","608_DistGPSGrid2000"),
                                   verpathn = "v1.0",
                                   dname = "prseagpsgrids2000",
                                   spcode,
                                   foraging.range) {

    # Set
    prsea <- NULL

    result <- tryCatch({

        filename <-  file.path(dspathn, dssection, verpathn,
                               paste0(dname, "_",
                                      stringr::str_to_upper(spcode), ".RData"))

        # contains object "prsea"
        load(filename)

        franges <- as.numeric(as.character(colnames(prsea)))
        df <- abs(franges - foraging.range)
        sel.fr <- which(df == min(df))[1]
        prsea <- prsea[,sel.fr]

        # Return
        prsea

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_prseagpsgrids2000)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_prseagpsgrids2000)")})

    return(result)
}

## ########################################################################################
#' @title Import the Proportion of Seabird 2000 Subsite in each SPA
#'
#' @description <description>
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset
#' @param s2000ids Subsite code
#'
#' @return  num - attr(*, "dimnames") = List of 2
#'
#' @examples
#'   read_propnspains2000(s2000ids = "UK9010111")
#'
#' @noRd

read_propnspains2000 <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                                 dssection = file.path("06_Geographic","609_PropnSPAinS2000"),
                                 verpathn = "v1.0",
                                 dname = "PropnSPAinS2000",
                                 s2000ids) {

    # Set
    out <- NULL

    result <- tryCatch({

        filename <-  file.path(dspathn, dssection, verpathn, paste0(dname,".RData"))

        # contains object "out"
        load(filename)

        # Return
        out

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_propnspains2000)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_propnspains2000)")})

    return(result)
}

## ########################################################################################
#' @title Import the At-sea Maps
#'
#' @description Function to load the at-sea maps for a specified species and
#'   behaviour for given months of the year.
#'
#' Required package description
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset
#' @param spcode Two-letter code for the seabird species. Leave blank to return all species.
#' @param atseabehav Behaviour to include; e.g. "fly", "water"
#' @param months.atsea Vector containg the months of the year to include,
#'   specified as strings with 2 characters, e.g. "01" for January.
#' @param withuncertainty Logical.
#'
#' @importFrom stringr str_to_lower str_to_upper
#'
#' @return Large numeric
#'
#' @examples
#'   read_atseamaps(spcode = "FF", atseabehav = "fly", months.atsea = c("01","02","03"), withuncertainty = FALSE)
#'
#' @noRd

read_atseamaps <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                           dssection = file.path("07_Distributions","701_AtseaMaps"),
                           verpathn = "v1.0",
                           dname = "atseamaps",
                           spcode,
                           atseabehav,
                           months.atsea,
                           withuncertainty) {

    atseamaps <-  NULL

    result <- tryCatch({

        swunc <- c("est", "sim")[1 + withuncertainty]

        nm <- length(months.atsea)

        filenames <- file.path(dspathn, dssection, verpathn,
                               paste0(
                                   dname, "_", stringr::str_to_lower(swunc), "_",
                                   stringr::str_to_lower(atseabehav), "_",
                                   stringr::str_to_upper(spcode), "_",
                                   months.atsea, ".RData")
        )

        # loop over months and calculate sum
        for(k in 1:nm){
            load(filenames[k]) # contains object: 'atseamaps'
            if(k == 1){
                new <- atseamaps
            }
            else{
                new <- new + atseamaps
            }
        }

        ## convert sum to monthly mean
        out <- new/nm

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_atseamaps)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_atseamaps)")})

    return(result)
}

## ########################################################################################
#' @title Import the GPS Global Maps dataset
#'
#' @description Function to load the requested global map from dataset 702 in
#'   the Data Store
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset
#' @param grid The name of the required grid; 'atseagrid' or 'gpsgrid'
#' @param spcode Two-letter code for the seabird species. Leave blank to return all species.
#'
#' @importFrom stringr str_to_lower str_to_upper
#'
#' @return Large matrix (>1GB)
#'
#' @examples
#'   read_gpsmaps(grid = "gpsgrid", spcode = "ki")
#'
#' @noRd

read_gpsmaps <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                         dssection = file.path("07_Distributions","702_GPSglobalMapsPlusKey"),
                         verpathn = "v1.0",
                         dname = "gpsmaps",
                         grid,
                         spcode) {

    # Returned if successful
    gpsmaps <- filename <- NULL

    result <- tryCatch({

        filename <- file.path(dspathn, dssection, verpathn,
                              paste0(dname, "_", stringr::str_to_lower(grid),
                                     "_", stringr::str_to_upper(spcode),
                                     ".RData"))

        # contains object "gpsmaps"
        load(filename)

        # Return the object
        gpsmaps

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_gpsmaps)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_gpsmaps)")})

    return(result)
}

## ########################################################################################
#' @title Import the TDR maps dataset
#'
#' @description Loads utilisation distributions for common guillemot (GU),
#'   razorbill (RA) or European shag (SA) for only diving (foraging) locations.
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset
#' @param gridn Grid type (atseagrid, gpsgrid)
#' @param spcode Two-letter code for the seabird species.
#'
#' @return Large matrix
#'
#' @importFrom stringr str_to_lower str_to_upper
#'
#' @examples
#'   read_tdrmaps(grid = "gpsgrid", spcode = "GU")
#'
#' @noRd

read_tdrmaps <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                         dssection = file.path("07_Distributions","703_TDRMaps"),
                         verpathn = "v1.0",
                         dname = "tdrmaps",
                         grid,
                         spcode) {

    # Returned if successful
    tdrmaps <- filename <- NULL

    result <- tryCatch({

        filename <- file.path(dspathn, dssection, verpathn,
                              paste0(dname, "_", stringr::str_to_lower(grid),
                                     "_", stringr::str_to_upper(spcode),
                                     ".RData"))

        # contains object "tdrmaps"
        load(filename)

        # Return the object
        tdrmaps

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_tdrmaps)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_tdrmaps)")})

    return(result)
}

## ########################################################################################
#' @title Import the BDMPS spatdist dataset
#'
#' @description <description>
#'
#' Required package description
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset (expects to find dname.csv and fields_dname.csv)
#' @param spcode Two-letter species code(s)
#'
#' @return <return>
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_to_lower
#'
#' @examples
#'   read_bdmpsspatdist()
#'   read_bdmpsspatdist(spcode = c("pu","AC"))
#'
#' @noRd

read_bdmpsspatdist <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                               dssection = file.path("07_Distributions","704_BDMPSspatdist"),
                               verpathn = "apr2021",
                               dname = "bdmpsspatialdistribution",
                               spcode = c("")) {

    Speccode <- NULL

    result <- tryCatch({

        f <- readr::read_csv(file.path(dspathn, dssection, verpathn, paste0(dname,".csv")),
                             col_names = TRUE,
                             skip_empty_rows = TRUE,
                             trim_ws = TRUE, skip = 0, n_max = Inf,
                             locale = readr::locale(encoding = "UTF-8"),
                             col_types = readr::cols())  %>%
            dplyr::filter(stringr::str_detect(str_to_lower(Speccode), paste(str_to_lower(spcode), collapse = '|')))

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_bdmpsspatdist)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_bdmpsspatdist)")})

    return(result)
}

## ########################################################################################
#' @title Import the GPS Maps by SPA dataset
#'
#' @description This function loads all of the GPS maps and returns the requested
#'   subset, defined by the species and SPA codes.
#'
#' @param dspathn Path to the Data Store
#' @param dssection Path to the Data Store section holding the dataset.
#' @param verpathn Folder name for the required version
#' @param dname Stub name for the dataset
#' @param spcode Two-letter code for the seabird species. Leave blank to return all species.
#' @param spacodes Vector of site codes for the required SPAs
#'
#' @importFrom raster match
#'
#' @return A list with RasterLayer maps and corresponding 'colsizes' number of birds.
#'
#' @examples
#'   read_gpsmapsbyspa(spcode = "KI", spacodes = "UK9004171")
#'
#' @noRd

read_gpsmapsbyspa <- function(dspathn = file.path("C:","Users","dcmo","OneDrive - UKCEH","00000_SeabORD_DataStore"),
                              dssection = file.path("07_Distributions","705_GPSMapsbySPA"),
                              verpathn = "v1.0",
                              dname = "GPS-maps-by-SPA",
                              spcode = NULL,
                              spacodes = NULL) {

    result <- tryCatch({

        load(file.path(dspathn, dssection, verpathn, paste0(dname,".RData")))
        mm <- raster::match(paste0(spcode, spacodes), paste0(out$species, out$site_codes))
        new <- list(maps = as.list(NULL), colsizes = rep(NA, length(mm)))

        for(k in 1:length(mm)){

            if(! is.na(mm[k])){

                new$maps[[k]] <- out$maps[[mm[k]]]

                new$colsizes[k] <- out$spacolsize[[mm[k]]]
            }
            else{

                new$maps[[k]] <- NA
            }
        }

        new

    }, warning = function(w) {w$message <- paste0("CEF warning: ",w$message, " (read_gpsmapsbyspa)")
    }, error = function(e) {e$message <- paste0("CEF error: ",e$message, " (read_gpsmapsbyspa)")})

    return(result)
}

# end
