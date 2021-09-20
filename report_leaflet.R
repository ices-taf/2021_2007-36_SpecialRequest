
library(icesTAF)
taf.library(sfdSAR)
library(data.table)
library(dplyr)
library(raster)
library(leaflet)
library(htmlwidgets)
library(sf)

sa_benth <- fread("output/breaks_20/benth.csv") %>% as_tibble()
sa_benth <- sa_benth %>% dplyr::select(-wkt)

# check
value <- "FishingHour_mid"
#value <- "sar"
gear <- "TBB_MOL"
year <- 2020

years <- 2009:2020
trans <- function(x) x^.25
trans_inv <- function(x) x^4

make_map <- function(value, gear) {
      msg("doing: ", value, " - ", gear)

    sa_gear <- sa_benth %>%
      filter(
        benthisMet == gear
      )

    msg("  making rasters")
    rasts <-
      lapply(years, function(year) {
        sa_gear_y <- filter(sa_gear, Year == year)
        sa_gear_y <- sa_gear_y[sa_gear_y[[value]] > 0,]
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
      next
    }
    rvalues <- unname(unlist(lapply(rasts, function(x) if (!is.null(x)) values(x) else NA)))
    rvalues <- range(rvalues[!is.na(rvalues)])

    # palatte
    if (value %in% c("FishingHour_mid", "TotWeight_mid", "TotValue_mid")) {
      pal <- colorFactor("Spectral", rvalues,
        na.color = "transparent"
      )
    } else {
      pal <- colorNumeric("Spectral", rvalues,
        na.color = "transparent"
      )
    }

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



for (value in
    c(
      #"sar", "subsar", "surface", "subsurface", "kWFishingHour",
      "FishingHour_mid", "TotWeight_mid", "TotValue_mid"
    )) {
  for (gear in unique(sa_benth$benthisMet)) {

    m <- make_map(value, gear)

    # save map
    msg("  saving map")
    fname <- sprintf("%s-%s.html", gear, value)
    saveWidget(
      m,
      file = fname,
      selfcontained = TRUE,
      title = sprintf("%s %s all years", gear, value)
    )
    cp(fname, "report", move = TRUE)
  }
}
