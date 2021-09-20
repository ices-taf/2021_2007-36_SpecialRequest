#' title
#'
#' description
#'
#' @name db
#' @format csv file
#' @tafOriginator ICES
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

# packages
library(icesTAF)
library(RODBC)

# settings
dbConnection <- "Driver={SQL Server};Server=SQL06;Database=VMS;Trusted_Connection=yes"

# connect to DB
conn <- odbcDriverConnect(connection = dbConnection)

datacallyear <- 2021

for (year in 2009:(datacallyear - 1)) {
  msg("downloading LE data for ... ", year)

  # set up sql command
  sqlq <- sprintf("SELECT * FROM dbo._%s_ICES_VMS_Datacall_LE WHERE Year = %i", datacallyear, year)

  # fetch
  le <- sqlQuery(conn, sqlq)
  # save to file
  write.taf(le, file = sprintf("le_%i.csv", year))
}

for (year in 2009:(datacallyear - 1)) {
  msg("downloading VMS data for ... ", year)

  # set up sql command
  sqlq <- sprintf("SELECT * FROM dbo._%s_ICES_VMS_Datacall_VMS WHERE Year = %i", datacallyear, year)

  # fetch
  vms <- sqlQuery(conn, sqlq)
  # save to file
  write.taf(vms, file = sprintf("vms_%i.csv", year))
}

# disconnect
odbcClose(conn)
