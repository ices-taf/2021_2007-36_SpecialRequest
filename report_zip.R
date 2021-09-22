## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)

mkdir("report")

# collate documents into a zip file
files <- dir("output/breaks_20/", full.names = TRUE)

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

zip(
  file.path(
    "report",
    "html_map_renders.zip"
  ),
  dir("report/breaks_20/", full.names = TRUE),
  extras = "-j"
)



# zip up with disclaimer, and advice document
files <-
  c(
    #taf.data.path("ospar.2021.10.pdf"),
    taf.data.path("disclaimer", "disclaimer.txt"),
    "report/shapefiles.zip",
    "report/simple_features.zip",
    "report/html_map_renders.zip",
    "README.md"
  )


#' Data output: EU request on the production of matrices by year and
#' age with F-at-age for stocks corresponding to the latest published
#' advice for each stock
#'
#' The zip file contains csv files of F-at-age by stock, along with
#' an R object of a named list of FLQuants containing the same
#' information.
#' Also included is partial F and proportion of the total catch in
#' numbers by stock and age for various fleet components.  Three gear
#' groupings are provided, from a high level, describing the type of
#' fishing (e.g. Otter trawl, pelagic trawl), a more detailed grouping
#' based on metier level 4 codes, and a more detailed still based on
#' metier level 5.  Metier level 5 is provided in case the user wished
#' to construct thier own fleet grouping.
#'
#' @name ICES.2020.matrices-of-F-at-age-for-selected-stocks.zip
#' @references \url{https://ices.dk}
#' @format a zip file
#' @tafOriginator ICES TAF
#' @tafYear 2020
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
