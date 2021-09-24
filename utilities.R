
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
  x <- out_benth$TotWeight[out_benth$benthisMet == "OT_SPF"]
  vs <- out_benth$NoDistinctVessels[out_benth$benthisMet == "OT_SPF"]
  ids <- out_benth$AnonymizedVesselID[out_benth$benthisMet == "OT_SPF"]
  nbreaks <- 20
}
categorise <- function(x, vs, ids, nbreaks = 20, name) {

  # first compute quantiles
  quantiles <- quantile(x, seq(0, 1, length = nbreaks + 1))
  quantiles[length(quantiles)] <- ceiling(max(x) + 0.01)

  # calculate the lowest category: 5th percentile of data with more than 2 unique vessels
  cut <- quantile(x[vs > 2], .05)

  # strip of the lower categories, such that all categories except for the bottom 2
  # are true 5th percentile ranges, working from the maximum down.
  # also the 2nd category will always contain between 5% and 10% of the data
  breaks <- c(0, cut, quantiles[quantiles > cut][-1])
  cats <- findInterval(x, breaks, left.open = FALSE)

  coverage <- signif(table(cats) / length(cats), 2)

  out <-
    tibble(
      low = breaks[cats],
      upp = breaks[cats + 1],
      cov = coverage[cats],
      vs = vs, ids = ids
    ) %>%
    group_by(low, upp, cov) %>%
    mutate(
      anon = vessels(ids, vs) >= 3
    ) %>%
    dplyr::select(-vs, -ids)

  if (!all(out$anon)) {
    print(name)
    warning(name, ": These breaks have resulted in groups with less than 3 unique vessels!")
  }

  out <- out %>% dplyr::select(-anon)
  names(out) <- paste0(name, names(out))
  out
}

categorise_all <- function(out, nbreaks = 20) {
    print(out[1,5])
    out <-
      cbind(
        out,
        categorise(out$kWFishingHour, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "kWH_"),
        categorise(out$FishingHour, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "Hour_"),
        categorise(out$TotWeight, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "TotWt_"),
        categorise(out$TotValue, out$NoDistinctVessels, out$AnonymizedVesselID, nbreaks = nbreaks, name = "TotVal_")
      ) %>%
      dplyr::select(
        -kWFishingHour, -FishingHour, -TotWeight, -TotValue, -NoDistinctVessels, -AnonymizedVesselID
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



make_map <- function(value, gear, sa_gear) {
  msg("doing: ", value, " - ", gear)

  msg("  making rasters")
  rasts <-
    lapply(years, function(year) {
      sa_gear_y <- filter(sa_gear, Year == year)
      sa_gear_y <- sa_gear_y[sa_gear_y[[value]] > 0, ]
      msg("    ", year)
      if (nrow(sa_gear_y) == 0) {
        return(NULL)
      }

      # make raster
      resolution <- 0.05
      loc <- sa_gear_y %>%
        dplyr::select(lon, lat) %>%
        rename(y = lat, x = lon)

      # set up an 'empty' raster, here via an extent object derived from your data
      r <- raster(extent(loc) + resolution / 2,
        resolution = resolution,
        crs = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      )

      r <- rasterize(loc, r, trans(sa_gear_y[[value]]), fun = "sum")

      r
    })
  names(rasts) <- years

  if (all(sapply(rasts, is.null))) {
    msg("skipping, no non-zero data")
    return(NULL)
  }
  rvalues <- sort(unique(unname(unlist(lapply(rasts, function(x) if (!is.null(x)) values(x) else NA)))))

  # palatte
    rvalues <- range(rvalues[!is.na(rvalues)])
    pal <- colorNumeric("Spectral", rvalues,
      na.color = "transparent", reverse = TRUE
    )

  msg("  making map")
  m <-
    leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap)

  # add layers

  rnames <-
    paste0(
      names(rasts), ": ",
      sapply(rasts, function(x) {
        if (!is.null(x)) {
          paste(round(trans_inv(range(values(x), na.rm = TRUE)), 3), collapse = " - ")
        } else {
          NA
        }
      })
    )

  for (layer in seq_along(rasts)) {
    if (is.null(rasts[[layer]])) next
    m <- addRasterImage(m, rasts[[layer]], colors = pal, opacity = 0.8, group = rnames[[layer]])
  }

  # add legend
  m <- addLegend(m,
    pal = pal, values = rvalues,
    title = sprintf("%s (%s)", value, gear), opacity = .8,
    labFormat = labelFormat(transform = trans_inv)
  )

  # add controls
  m <- addLayersControl(m,
    baseGroups = rnames,
    options = layersControlOptions(collapsed = FALSE)
  )

  m <- addScaleBar(
    m,
    position = "bottomright",
    options = scaleBarOptions()
  )

  m
}
