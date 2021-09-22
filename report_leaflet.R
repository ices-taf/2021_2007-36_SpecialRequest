
library(icesTAF)
taf.library(sfdSAR)
library(data.table)
library(dplyr)
library(raster)
library(leaflet)
library(htmlwidgets)
library(sf)
library(ggplot2)

source("utilities.R")

# check
value <- "FishingHour_mid"
# value <- "sar"
gear <- "OT_MIX"
year <- 2020
breaks <- 20

years <- 2009:2020
trans <- function(x) x^.25
trans_inv <- function(x) x^4

breaks <- 20

mkdir(paste0("report/breaks_", breaks))

sa_benth <- fread(paste0("output/breaks_", breaks, "/benth.csv")) %>% as_tibble()
sa_benth <- sa_benth %>% dplyr::select(-wkt)

for (value in
  c(
    "sar", "subsar", "surface", "subsurface",
    "kWH_mid", "Hour_mid", "TotWt_mid", "TotVal_mid"
)) {
  for (gear in unique(sa_benth$benthisMet)) {
    fname <- sprintf("%s-%s.html", gear, value)
    if (file.exists(file.path("report", paste0("breaks_", breaks), fname))) next

    m <- make_map(value, gear)

    if (is.null(m)) {
      next
    }

    # save map
    msg("  saving map")
    saveWidget(
      m,
      file = fname,
      selfcontained = TRUE,
      title = sprintf("%s %s all years", gear, value)
    )
    cp(fname, paste0("report/breaks_", breaks), move = TRUE)
  }
}


sa_total <- fread(paste0("output/breaks_", breaks, "/total.csv")) %>% as_tibble()
sa_total <- sa_total %>% dplyr::select(-wkt)

for (value in
  c(
    "sar", "subsar", "surface", "subsurface",
    "kWH_mid", "Hour_mid", "TotWt_mid", "TotVal_mid"
)) {
  fname <- sprintf("%s-%s.html", "total", value)
  if (file.exists(file.path("report", paste0("breaks_", breaks), fname))) next

  m <- make_map(value)

  if (is.null(m)) {
    next
  }

  # save image
  mapshot(
    m,
    url = NULL,
    file = NULL,
    remove_controls = c(
      "zoomControl", "layersControl", "homeButton", "scaleBar",
      "drawToolbar", "easyButton"
    )
  )

  # save map
  msg("  saving map")
  saveWidget(
    m,
    file = fname,
    selfcontained = TRUE,
    title = sprintf("%s %s all years", gear, value)
  )
  cp(fname, paste0("report/breaks_", breaks), move = TRUE)

}
