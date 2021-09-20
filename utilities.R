
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
