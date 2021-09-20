## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)
library(data.table)
library(dplyr)
library(sf)

mkdir("output")

vms_sar <- fread("model/vms_sar.csv")
vms_sar <-
  vms_sar %>%
  filter(
    !CountryCode %in% c("IS", "NO")
  )

# compute summaries of swept area over benthis groups

vessels <- function(id, n) {
  if (any(n >= 3)) {
    return(3)
  }

  ids <- paste(id, collapse = ";")
  ids <- strsplit(ids, ";")[[1]]
  ids <- unique(ids)
  pmin(3, length(ids))
}

ids <- function(id, n) {
  if (any(n >= 3)) {
    return("")
  }

  ids <- paste(id, collapse = ";")
  ids <- strsplit(ids, ";")[[1]]
  ids <- unique(ids)
  paste(sort(ids), collapse = ";")
}

sums <- function(x) {
  x %>%
    summarise(
      kWFishingHour = sum(kWFishingHour, na.rm = TRUE),
      FishingHour = sum(FishingHour, na.rm = TRUE),
      TotWeight = sum(TotWeight, na.rm = TRUE),
      TotValue = sum(TotValue, na.rm = TRUE),
      subsurface = sum(subsurface, na.rm = TRUE),
      surface = sum(surface, na.rm = TRUE),
      sar = sum(sar, na.rm = TRUE),
      subsar = sum(subsar, na.rm = TRUE),
      NoDistinctVessels = vessels(AnonymizedVesselID, NoDistinctVessels),
      AnonymizedVesselID = ids(AnonymizedVesselID, NoDistinctVessels)
    ) %>%
    ungroup()
}

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


# summarise tot weight and total value and fishinghours
if (FALSE) {
  x <- out_benth$FishingHour
  vs <- out_benth$NoDistinctVessels
  ids <- out_benth$AnonymizedVesselID
}
categorise <- function(x, vs, ids, nbreaks = 20, name) {
  breaks <- scales::breaks_pretty(nbreaks)(x)
  int <- findInterval(x, breaks)
  out <-
    tibble(
      min = breaks[int],
      max = breaks[int + 1],
      mid = (breaks[-1] + breaks[-length(breaks)])[int] / 2,
      vs = vs, ids = ids
    ) %>%
    group_by(min, max, mid) %>%
    mutate(
      anon = vessels(ids, vs) >= 3
    ) %>%
    select(-vs, -ids)

  if (!all(out$anon)) {
    warning("These breaks have resulted in groups with less than 3 unique vessels!")
  }

  names(out) <- paste0(name, names(out))
  out
}

save_output_benth <- function(nbreaks, shape = TRUE, check.only = FALSE) {
  out <- out_benth
  out <-
    cbind(
      out,
      categorise(out$FishingHour, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "FishingHour_"),
      categorise(out$TotWeight, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "TotWeight_"),
      categorise(out$TotValue, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "TotValue_")
    ) %>%
    select(
      -FishingHour, -TotWeight, -TotValue
    ) %>%
    mutate(
      wkt = paste(
        "POLYGON ((",
        lon - .025, " ", lat + 0.025, ",",
        lon + .025, " ", lat + 0.025, ",",
        lon + .025, " ", lat - 0.025, ",",
        lon - .025, " ", lat - 0.025, ",",
        lon - .025, " ", lat + 0.025, "))"
      )
    )
    tibble()

  if (check.only) return()

  # write out a shapefile for each gear
  dir <- file.path("output", paste0("breaks_", nbreaks))
  mkdir(dir)

  fwrite(out, file = file.path(dir, "Benth.csv"))
  if (!shape) {
    return()
  }

  by(out, interaction(out$benthisMet, out$Year, drop = TRUE), function(x) {
    x_sf <- st_as_sf(x, wkt = "wkt", crs = 4326)
    fname <- paste0(x$benthisMet[1], "-", x$Year[1], ".shp")
    suppressWarnings(st_write(x_sf, file.path(dir, fname)))
  })
}

save_output_agg <- function(nbreaks, shape = TRUE, check.only = FALSE) {
  out <- out_agg
  out <-
    cbind(
      out,
      categorise(out$FishingHour, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "FishingHour_"),
      categorise(out$TotWeight, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "TotWeight_"),
      categorise(out$TotValue, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "TotValue_")
    ) %>%
    select(
      -FishingHour, -TotWeight, -TotValue
    ) %>%
    mutate(
      wkt = paste(
        "POLYGON ((",
        lon - .025, " ", lat + 0.025, ",",
        lon + .025, " ", lat + 0.025, ",",
        lon + .025, " ", lat - 0.025, ",",
        lon - .025, " ", lat - 0.025, ",",
        lon - .025, " ", lat + 0.025, "))"
      )
    )
    tibble()

  if (check.only) {
    return()
  }

  # write out a shapefile for each gear
  dir <- file.path("output", paste0("breaks_", nbreaks))
  mkdir(dir)

  fwrite(out, file = file.path(dir, "agg.csv"))
  if (!shape) {
    return()
  }

  by(out, interaction(out$fishingCategory, out$Year, drop = TRUE), function(x) {
    x_sf <- st_as_sf(x, wkt = "wkt", crs = 4326)
    fname <- paste0(x$fishingCategory[1], "-", x$Year[1], ".shp")
    suppressWarnings(st_write(x_sf, file.path(dir, fname)))
  })
}

save_output_total <- function(nbreaks, shape = TRUE, check.only = FALSE) {
  out <- out_total
  out <-
    cbind(
      out,
      categorise(out$FishingHour, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "FishingHour_"),
      categorise(out$TotWeight, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "TotWeight_"),
      categorise(out$TotValue, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "TotValue_")
    ) %>%
    select(
      -FishingHour, -TotWeight, -TotValue, -AnonymizedVesselID, -NoDistinctVessels
    ) %>%
    mutate(
      wkt = paste(
        "POLYGON ((",
        lon - .025, " ", lat + 0.025, ",",
        lon + .025, " ", lat + 0.025, ",",
        lon + .025, " ", lat - 0.025, ",",
        lon - .025, " ", lat - 0.025, ",",
        lon - .025, " ", lat + 0.025, "))"
      )
    ) %>%
    tibble()

  if (check.only) {
    return()
  }

  # write out a shapefile for each gear
  dir <- file.path("output", paste0("breaks_", nbreaks))
  mkdir(dir)

  fwrite(out, file = file.path(dir, "Total.csv"))
  if (!shape) return()

  by(out, out$Year, function(x) {
    x_sf <- st_as_sf(x, wkt = "wkt", crs = 4326)
    fname <- paste0("Total-", x$Year[1], ".shp")
    suppressWarnings(st_write(x_sf, file.path(dir, fname)))
  })
}

# check anonymity holds
save_output_total(20, check.only = TRUE)
save_output_agg(20, check.only = TRUE)
save_output_benth(20, check.only = TRUE)

save_output_total(50, check.only = TRUE)
save_output_agg(50, check.only = TRUE)
save_output_benth(50, check.only = TRUE)

# these all have issues
save_output_total(100, check.only = TRUE)
save_output_agg(100, check.only = TRUE)
save_output_benth(100, check.only = TRUE)

save_output_total(200, check.only = TRUE)
save_output_agg(200, check.only = TRUE)
save_output_benth(200, check.only = TRUE)

# save
save_output_total(20)
save_output_agg(20)
save_output_benth(20)

save_output_total(50)
save_output_agg(50)
save_output_benth(50)
