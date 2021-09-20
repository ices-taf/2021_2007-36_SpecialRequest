library(icesTAF)

if (FALSE) {
  taf.skeleton(".")

  header <- "#' Title
#'
#' Description
#'
#' @name %s
#' @format csv file
#' @tafOriginator ICES
#' @tafYear 2021
#' @tafAccess Public
#' @tafSource script

NULL
"

make_data <- function(name) {
  cat(
    sprintf(header, name),
    file = paste0(taf.boot.path(name), ".R")
  )
}

#make_data("x")

}

draft.software(c("icesConnect", "icesVMS", "RODBC", "sfdSAR"), file = TRUE)

taf.bootstrap(data = FALSE)

taf.roxygenise(
  files = c(
    "db.R",
    "benthis_gears.R",
    "shapefiles.R",
    "csquare_list.R",
    "csquare_lookup.R"
  )
)

taf.bootstrap(software = FALSE)

sourceAll()
