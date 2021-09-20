#' Benthis gears lookup table
#'
#' A lookup table linking metier level 6 to Benthis gear categories
#'
#' @name benthis_gears
#' @references \url{https://ices.dk}
#' @format a csv file
#' @tafOriginator ICES VMS and Logbook database
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

# load repo icesVMS package
library(icesTAF)
taf.library(icesConnect)
taf.library(icesVMS)
library(dplyr)


gear_widths <-
  icesVMS::get_benthis_parameters() %>%
  select(
    benthisMet, subsurfaceProp, gearWidth, firstFactor, secondFactor,
    gearModel, gearCoefficient, contactModel
  ) %>%
  mutate(
    subsurfaceProp = as.numeric(subsurfaceProp)
  )

metier_lookup <-
  icesVMS::get_metier_lookup() %>%
  tibble() %>%
  select(leMetLevel6, benthisMetiers, fishingCategory) %>%
  mutate(benthisMetiers = ifelse(benthisMetiers == "NA", NA, benthisMetiers)) %>%
  filter(!is.na(benthisMetiers) & benthisMetiers != "Benthis_metiers")

# join widths and lookup
aux_lookup <-
  gear_widths %>%
  right_join(metier_lookup, by = c("benthisMet" = "benthisMetiers")) %>%
  tibble()

save(aux_lookup, file = "aux_lookup.rData")
