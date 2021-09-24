## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)

mkdir("report")

# collate documents into a zip file
files <- dir("output", full.names = TRUE)

zip(
  file.path(
    "report",
    "shapefiles.zip"
  ),
  files[-grep(".csv", files)],
  extras = "-j"
)

zip(
  file.path(
    "report",
    "simple_features.zip"
  ),
  files[grep(".csv", files)],
  extras = "-j"
)

# zip up with disclaimer, and advice document
files <-
  c(
    taf.data.path("ospar.2021.12.pdf"),
    taf.data.path("disclaimer", "disclaimer.txt"),
    "report/shapefiles.zip",
    "report/simple_features.zip",
    "README.md"
  )


#' Data output: OSPAR request on the production of
#'
#' The zip file contains .
#'
#' @name ICES.2021.OSPAR_production_of_spatial_fishing_pressure_data_layers.zip
#' @references \url{https://ices.dk}
#' @format a zip file
#' @tafOriginator ICES TAF
#' @tafYear 2021
#' @tafAccess Public

zip(
  file.path(
    "report",
    "ICES.2021.OSPAR_production_of_spatial_fishing_pressure_data_layers.zip"
  ),
  files,
  extras = "-j"
)

# clean up ?
