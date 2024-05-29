# TO DO:
# - possibility to add legend (e.g. species name)
# - possibility to add axes with coordinates
# - possibility to add scale bar (mapmisc::scaleBar)



#' Map species occurrences
#'
#' Plot map of species occurrences (or any set of point coordinates) on top of different background layers.
#'
#'
#' @export
#' @return A map (plot), unless `bg = 'KML'` in which case a kmz file is saved to be explored with Google Earth. In some cases, a raster layer, leaflet object, or ggplot object is returned in addition to the map.
#' @importFrom scales alpha
#' @param locs A matrix, dataframe, SpatialPoints or SpatialPointsDataFrame containing coordinates of species occurrences. If locs is a matrix or dataframe, it will be converted to a spatial object using [locs2sp()].
#' @param proj Character string specifying the projection of coordinates data (see [sp::proj4string()] or <http://spatialreference.org>). Default is geographic (unprojected) coordinates, datum WGS84. Not used if locs is already an Spatial object with defined projection.
#' @param ras Raster* object to be used as background for points. Default is NULL, in which case a background map defined by `bg` will be used.
#' @param bg Type of background map. Either
#' 'google' for Google maps background (using [dismo::gmap()]) (note this requires a Google API key - currently not working!),
#' 'coast' for coastlines (using `coastsCoarse` from \pkg{rworldmap} package),
#' 'ggmap' for maps provided by [ggmap::get_map()] in \pkg{ggmap} package,
#' 'leaflet' for an interactive HTML map using \pkg{leaflet},
#' 'mapmisc' for using any of the layers available in \pkg{mapmisc} package, or
#' 'kml' for producing a KMZ file to be opened with Google Earth.
#' @param pcol Colour to be used for points. Default is "red".
#' @param alpha Colour transparency for points, between 0 (fully transparent) and 1 (fully opaque).
#' @param psize Point size. Default is 1 (cex = 1).
#' @param add Logical. Add these occurrences to a previous map? (e.g. for a new species). Default is FALSE. Note this feature doesn't work for all map types. For leaflet maps, when add = TRUE, a leaflet basemap (e.g. as produced by a previous call to occmap) must be provided (see `leaflet.base`).
#' @param leaflet.base Leaflet map to be used as basemap to add further points when add is TRUE.
#' @param mapmisc_server character. Server/type of background map to be used when bg = "mapmisc". Run [mapmisc::osmTiles()] to see all the available layers.
#' @param filename Character. Path and filename of the KMZ file produced when bg = "kml".
#' @param ... additional parameters to be passed to
#' dismo::gmap if bg == 'google'
#' plot if bg == 'coast'
#' ggmap::get_map if bg == 'ggmap'
#' leafletR::leaflet if bg == 'leaflet'
#' raster::plot or raster::plotRGB if bg == 'mapmisc'
#' See these functions help files for details.
#' @details If using ggmap and stamen maps, large regions seem to give problems.
#' @examples
#' \dontrun{
#' # Using acaule dataset from dismo package:
#' library(dismo)
#' data(acaule)
#'
#' occmap(locs = acaule)  # If 'ras' not provided, default is bg = "ggmap"
#' occmap(acaule, bg = "coast")  # just coastlines as background map
#' occmap(acaule, bg = "leaflet") # leaflet interactive map
#'
#' # Watercolor map from Stamen using ggmap
#' # NB: use small regions, otherwise give error to download map tiles
#' locs_redux <- subset(acaule, lon > -80 & lon < -60 & lat > -30 & lat < -10)
#' occmap(locs = locs_redux, maptype = 'watercolor', source = 'stamen',
#'            pcol = "darkgreen", psize = 4)
#'
#' # Plot occurrences in a specific country:
#' occmap(locs = subset(acaule, country=="Bolivia"), bg = "leaflet")
#' # (note there are georeferencing errors in the data)
#'
#'
#' # Add transparency to points
#' occmap(locs = acaule, pcol = "red", alpha = 0.5, bg = "coast")
#' occmap(locs = acaule, pcol = "red", alpha = 0.5, bg = "leaflet")
#'
#'
#' ## Providing spatial objects ##
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#' occmap(meuse)
#'
#' #alternatively, provide projection argument:
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' occmap(meuse, proj = "+init=epsg:28992")
#' }



occmap <- function(locs,
                   ras = NULL,
                   bg = "Esri.WorldImagery",
                   type = c("base", "ggplot", "leaflet"),
                   pcol = 'red',
                   alpha = 1,
                   psize = 1,
                   add = FALSE,
                   leaflet.base = NULL,
                   ...) {

  if (!inherits(locs, "sf")) {
    if (!inherits(locs, "SpatVector")) {
      stop("locs must be a sf or SpatVector object. You may find 'locs2sf()' or 'locs2vect()' useful.")
    }
  }

  if (inherits(locs, "SpatVector")) {
    locs <- sf::st_as_sf(locs)
  }

  if (is.null(ras)) {
    if (!is.null(bg)) {
      if (type != "leaflet") {
        ras <- maptiles::get_tiles(x = locs, provider = bg)
      }
    }
  }

  type <- match.arg(type)

  ## Project to geographical for mapping
  locs <- sf::st_transform(locs, crs = 4326)

  if (alpha < 1) pcol <- scales::alpha(pcol, alpha)  # point transparency


  ### PLOTTING ###

  if (type == "base") {
    map_terra(locs = locs, ras = ras, add = add, pcol = pcol, psize = psize, ...)
  }

  if (type == "ggplot") {
    return(map_ggplot(locs = locs, ras = ras, pcol = pcol, psize = psize, ...))
  }

  if (type == "leaflet") {
    bgmap <- map_leaflet(locs = locs, pcol = pcol, alpha = alpha, psize = psize,
                         prev.map = leaflet.base, ...)
  }

  if (exists("bgmap")) invisible(bgmap)

}





###### INDIVIDUAL PLOTTING FUNCTIONS ######

#### Map with terra (base) ####

map_terra <- function(locs = NULL, ras = NULL, add = FALSE, pcol, psize, ...) {

  if (!isTRUE(add)) {
    if (is.null(ras)) {
      terra::plot(coastsCoarse,
                  xlim = c(min(sf::st_coordinates(locs)[,1]) - 1,
                           max(sf::st_coordinates(locs)[,1]) + 1),
                  ylim = c(min(sf::st_coordinates(locs)[,2]) - 1,
                           max(sf::st_coordinates(locs)[,2]) + 1),
                  ...)
    }

    if (!is.null(ras)) {
      if (terra::nlyr(ras) == 3) {
        maptiles::plot_tiles(ras, ...)
      }
      terra::plot(ras, ...)
    }

  }

  # add points
  locs <- locs[, 1]
  terra::plot(locs, add = TRUE, pch = 20, col = pcol, cex = psize)

}




#### Map with ggplot on top of coastlines (borders) ####

map_ggplot <- function(locs = NULL, ras = NULL, pcol, psize, ...) {

  locs.bbox <- sf::st_bbox(locs)

  occmap <- ggplot2::ggplot(locs) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "", y = "")

  if (is.null(ras)) {
    occmap <- occmap +
      ggplot2::borders(fill = "grey95", colour = NA)
  }

  if (!is.null(ras)) {
    if (terra::nlyr(ras) == 3) {
      occmap <- occmap +
        tidyterra::geom_spatraster_rgb(data = ras, ...)
    } else {
      occmap <- occmap +
        tidyterra::geom_spatraster(data = ras, ...)
    }

  }

  occmap <- occmap +
    ggplot2::geom_sf(size = psize, col = pcol) +
    ggplot2::coord_sf(
      xlim = c(locs.bbox$xmin - 1, locs.bbox$xmax + 1),
      ylim = c(locs.bbox$ymin - 1, locs.bbox$ymax + 1)
    )

  return(occmap)

}

#### Leaflet maps ####

map_leaflet <- function(locs = NULL, pcol, alpha, psize, prev.map = NULL, ...) {

  if (is.null(prev.map)) {

    bgmap <- leaflet::leaflet(locs) |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(stroke = FALSE, fillColor = pcol,
                                fillOpacity = alpha, radius = 3*psize, ...)

  }

  if (!is.null(prev.map)) {
    bgmap <- prev.map |>
      leaflet::addCircleMarkers(data = locs,
                                stroke = FALSE, fillColor = pcol, fillOpacity = alpha,
                                radius = 3*psize, ...)

  }

  print(bgmap)
  invisible(bgmap)

}













