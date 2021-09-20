#' Benthis gears lookup table
#'
#' A lookup table linking metier level 6 to Benthis gear categories
#'
#' @name csquare_list
#' @references \url{https://ices.dk}
#' @format a csv file
#' @tafOriginator ICES VMS and Logbook database
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

library(icesTAF)
library(data.table)

vmsfiles <- dir(taf.data.path("db"), pattern = "vms*", full = TRUE)

csquares <-
  data.frame(
    csquares =
    unique(unlist(
      sapply(
        vmsfiles,
        function(fname) {
          x <- fread(fname)
          unique(x$`C-square`)
        }
      )
    ))
  )

# save unique list
write.taf(csquares)
