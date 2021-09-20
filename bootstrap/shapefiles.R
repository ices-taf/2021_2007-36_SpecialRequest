#' title
#'
#' description
#'
#' @name shapefiles
#' @format csv file
#' @tafOriginator ICES
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

library(sf)
library(dplyr)

url <- "https://odims.ospar.org/geoserver/odims/wfs?service=WFS&version=2.0.0&request=GetFeature&typeName=ospar_regions_2017_01_002&outputFormat=KML"
download.file(url, destfile = "ospar.kml", mode = "w")

ospar <- st_read("ospar.kml")
unlink("ospar.kml")

ospar <-
  ospar %>%
  st_make_valid %>%
  st_union

save(ospar, file = "ospar.rData")
