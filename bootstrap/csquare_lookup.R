#' Statistical Rectangle lookup table subset to SWE EEZ
#'
#' A list of stat squares and accociated info, such as ICES area
#'
#' @name csquare_lookup
#' @references \url{https://ices.dk}
#' @format a csv file
#' @tafOriginator ICES VMS and Logbook database
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

# utility function here:

library(ggplot2)
library(sf)
taf.library(sfdSAR)

# get the ospar regions
load(taf.data.path("shapefiles/ospar.rData"), verbose = TRUE)
st_is_valid(ospar, reason = TRUE)

csquares <- read.taf(taf.data.path("csquare_list/csquares.csv"))
csquares$lat <- sfdSAR::csquare_lat(csquares$csquare)
csquares$lon <- sfdSAR::csquare_lon(csquares$csquare)
csquares <- st_as_sf(csquares, coords = c("lon", "lat"), crs = 4326)

msg("calculating csquares within ospar")
idx <- which(st_within(csquares, ospar, sparse = FALSE))
msg("done calculating")

csquares_ospar <- csquares[idx, ]

save(csquares_ospar, file = "csquares_ospar.rData")
