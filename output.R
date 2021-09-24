## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)
library(data.table)
library(dplyr)
library(sf)

source("utilities.R")

mkdir("output")

vms_sar <- fread("model/vms_sar.csv")
vms_sar <-
  vms_sar %>%
  filter(
    !CountryCode %in% c("IS", "NO")
  )

# compute summaries of swept area over benthis groups
out_total <-
  vms_sar %>%
  group_by(Year, `C-square`, lat, lon) %>%
  sums()

out_agg <-
  vms_sar %>%
  group_by(Year, `C-square`, lat, lon, fishingCategory) %>%
  sums()

out_benth <-
  vms_sar %>%
  group_by(Year, `C-square`, lat, lon, benthisMet) %>%
  sums()


# total layers
out_total_cat <- categorise_all(out_total)

out_total_cat

# write out a shapefile for each gear
fwrite(out_total_cat, file = file.path("output", "total.csv"))

# write shapes
by(
  out_total_cat,
  out_total_cat$Year,
  function(x) {
    x_sf <- st_as_sf(x, wkt = "wkt", crs = 4326)
    fname <- paste0("total-", x$Year[1], ".shp")
    if (!file.exists(file.path("output", fname))) {
      suppressWarnings(st_write(x_sf, file.path("output", fname)))
    }
  }
)




# aggregated layers
out_agg_cat <- by(out_agg, out_agg$fishingCategory, categorise_all)
out_agg_cat <-
  do.call(
    rbind,
    unclass(out_agg_cat)
  )

out_agg_cat

# write out a shapefile for each gear
fwrite(out_agg_cat, file = file.path("output", "fishing_categories.csv"))

# write shapes
by(
  out_agg_cat,
  interaction(out_agg_cat$fishingCategory, out_agg_cat$Year, drop = TRUE),
  function(x) {
    x_sf <- st_as_sf(x, wkt = "wkt", crs = 4326)
    fname <- paste0(x$fishingCategory[1], "-", x$Year[1], ".shp")
    if (!file.exists(file.path("output", fname))) {
      suppressWarnings(st_write(x_sf, file.path("output", fname)))
    }
  }
)




# do benthic stuff
out_benth_cat <- by(out_benth, out_benth$benthisMet, categorise_all)
out_benth_cat <-
  do.call(
    rbind,
    unclass(out_benth_cat)
  )

out_benth_cat

# write out a shapefile for each gear
fwrite(out_benth_cat, file = file.path("output", "benthic_metiers.csv"))

# write shapes
by(
  out_benth_cat,
  interaction(out_benth_cat$benthisMet, out_benth_cat$Year, drop = TRUE),
  function(x) {
    x_sf <- st_as_sf(x, wkt = "wkt", crs = 4326)
    fname <- paste0(x$benthisMet[1], "-", x$Year[1], ".shp")
    if (!file.exists(file.path("output", fname))) {
      suppressWarnings(st_write(x_sf, file.path("output", fname)))
    }
  }
)
