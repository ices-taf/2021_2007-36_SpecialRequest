
if (FALSE) {
  layer <- "ReferenceLayers/OSPAR_Subregions"
  cols <- NULL
  where <- NULL
}

getLayer <-
  function(
    layer, cols = NULL, where = NULL, geometryPrecision = 20
    ) {
  url <- httr::parse_url("https://gis.ices.dk/gis/rest/services/")
  url$path <-
    sprintf(
      "gis/rest/services/%s/MapServer/0/query",
      layer
    )
  url$query <-
    list(
      geometryType = "esriGeometryPolygon",
      f = "geojson",
      where = if (is.null(where)) "1=1" else where,
      outFields = if (is.null(cols)) "*" else paste(cols, collapse = ","),
      returnGeometry = "true",
      geometryPrecision = geometryPrecision
    )
  url <- httr::build_url(url)

  message("downloading ... ", url)
  out <- geojsonsf::geojson_sf(url)

  return(out)
}

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
      AnonymizedVesselID = ids(AnonymizedVesselID, NoDistinctVessels),
      .groups = "keep"
    ) %>%
    ungroup()
}



if (FALSE) {
  x <- out_benth$FishingHour
  vs <- out_benth$NoDistinctVessels
  ids <- out_benth$AnonymizedVesselID
}
categorise <- function(x, vs, ids, nbreaks = 20, name) {
  breaks <- quantile(x, seq(0, 1, length = nbreaks + 1))
  breaks[length(breaks)] <- ceiling(breaks[length(breaks)] + 0.001)
  int <- findInterval(x, breaks, left.open = FALSE)
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

categorise_all <- function(out, nbreaks) {
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
      ) %>%
    tibble()

    out
}


# summarise tot weight and total value and fishinghours
save_output_benth <- function(nbreaks, shape = TRUE, check.only = FALSE) {
  out <- categorise_all(out_benth, nbreaks)

  if (check.only) {
    return()
  }
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
    if (!file.exists(file.path(dir, fname))) {
      suppressWarnings(st_write(x_sf, file.path(dir, fname)))
    }
  })

  invisible(out)
}

save_output_agg <- function(nbreaks, shape = TRUE, check.only = FALSE) {
  out <- categorise_all(out_agg, nbreaks)

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
    if (!file.exists(file.path(dir, fname))) {
      suppressWarnings(st_write(x_sf, file.path(dir, fname)))
    }
  })

  invisible(out)
}

save_output_total <- function(nbreaks, shape = TRUE, check.only = FALSE) {
  out <- categorise_all(out_total, nbreaks)

  if (check.only) {
    return()
  }
    # write out a shapefile for each gear
    dir <- file.path("output", paste0("breaks_", nbreaks))
    mkdir(dir)
  fwrite(out, file = file.path(dir, "total.csv"))
  if (!shape) return()

  by(out, out$Year, function(x) {
    x_sf <- st_as_sf(x, wkt = "wkt", crs = 4326)
    fname <- paste0("Total-", x$Year[1], ".shp")
    if (!file.exists(file.path(dir, fname))) {
      suppressWarnings(st_write(x_sf, file.path(dir, fname)))
    }
  })

  invisible(out)
}
