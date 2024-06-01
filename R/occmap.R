
#' Map species occurrences
#'
#' Plot map of species occurrences (or any set of point coordinates) on top of different background layers.
#'
#' @param locs An [sf::sf()] or [terra::SpatVector()] object with point coordinates,
#' e.g. as generated from [locs2sf()] or [locs2vect()].
#' @param ras A [terra::SpatRaster()] object to be used as background for points.
#' If NULL (default), a background map defined by `bg` will be used.
#' @param bg Character. Type of background map to be used if `ras` is not provided.
#' `bg` should be one of the providers listed in [maptiles::get_tiles()] if
#' `type` is 'base' or 'ggplot', or one of the providers listed in
#' [leaflet::addProviderTiles()] if `type` is 'leaflet'.
#' @param type Character. One of "base", "ggplot" or "leaflet" to define the type
#' of map produced.
#' @param pcol Colour to be used for points. Default is "red".
#' @param alpha Colour transparency for points, between 0 (fully transparent)
#' and 1 (fully opaque).
#' @param psize Point size. Default is 1 (cex = 1).
#' @param add Logical. Add `locs` coordinates to a previous 'base' map? (e.g. for a new species).
#' @param prev.map Map to be used as basemap to add further points
#' (only applicable for "leaflet" and "ggplot" map types).
#' @param ... additional parameters to be passed to
#' [terra::plot()] if `type = "base"`
#' [tidyterra::geom_spatraster()] if `type = "ggplot"`
#' or [leaflet::addCircleMarkers()] if `type = "leaflet"`.
#'
#' @export
#' @return A map plus a leaflet or ggplot object, depending on `type`.

#' @examplesIf interactive()
#' ## Example coordinates
#' locs <- data.frame(lon = c(-5.8, -5.5, -5.9), lat = c(35.7, 36.2, 36.5))
#' locs <- locs2sf(locs, crs = 4326)
#'
#' ## Default map
#' occmap(locs, psize = 6)
#' occmap(locs, psize = 6, bg = "CartoDB.Positron") # Change background
#'
#' ## Interactive (leaflet) map
#' occmap(locs, psize = 6, type = "leaflet")
#' occmap(locs, psize = 6, type = "leaflet", bg = "CartoDB.Positron")
#'
#' ## ggplot map
#' occmap(locs, psize = 6, type = "ggplot")
#'
#' ## Adding points to a previous map
#' new.locs <- data.frame(lon = c(-5.8, -5.4), lat = c(36.2, 36.5))
#' new.locs.sf <- locs2sf(new.locs, crs = 4326)
#'
#' ## base
#' map <- occmap(locs, psize = 6)
#' occmap(new.locs.sf, add = TRUE, psize = 6, pcol = "blue")
#'
#' ## Adding points to a previous map (leaflet)
#' map <- occmap(locs, psize = 6, type = "leaflet")
#' occmap(new.locs.sf, prev.map = map, psize = 6, pcol = "blue")
#'
#' ## Adding points to a previous map (ggplot)
#' map <- occmap(locs, psize = 6, type = "ggplot")
#' occmap(new.locs.sf, prev.map = map, psize = 6, pcol = "blue")

occmap <- function(locs,
                   ras = NULL,
                   bg = "Esri.WorldImagery",
                   type = c("base", "ggplot", "leaflet"),
                   pcol = 'red',
                   alpha = 1,
                   psize = 1,
                   add = FALSE,
                   prev.map = NULL,
                   ...) {

  type <- match.arg(type)
  if (!is.null(prev.map)) {
    if (inherits(prev.map, "ggplot")) {
      type <- "ggplot"
    }
    if (inherits(prev.map, "leaflet")) {
      type <- "leaflet"
    }
  }

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


  ## Project to geographical for mapping
  locs <- sf::st_transform(locs, crs = 4326)

  if (alpha < 1) pcol <- scales::alpha(pcol, alpha)  # point transparency


  ### PLOTTING ###

  if (type == "base") {
    map_terra(locs = locs, ras = ras, add = add, pcol = pcol, psize = psize, ...)
  }

  if (type == "ggplot") {
    bgmap <- map_ggplot(locs = locs, ras = ras, pcol = pcol, psize = psize,
                        prev.map = prev.map, ...)
  }

  if (type == "leaflet") {
    bgmap <- map_leaflet(locs = locs,
                         bg = bg,
                         pcol = pcol, alpha = alpha, psize = psize,
                         prev.map = prev.map, ...)
  }

  if (exists("bgmap")) return(bgmap)

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

map_ggplot <- function(locs = NULL, ras = NULL, pcol, psize, prev.map, ...) {

  if (is.null(prev.map)) {

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
      ggplot2::scale_fill_continuous(na.value = "transparent") +
      ggplot2::geom_sf(size = psize, col = pcol)

  }

  if (!is.null(prev.map)) {
    suppressMessages(
      {occmap <- prev.map +
        ggplot2::geom_sf(data = locs, size = psize, col = pcol)}
    )
  }

  return(occmap)

}

#### Leaflet maps ####

map_leaflet <- function(locs = NULL,
                        bg = NULL,
                        pcol, alpha, psize,
                        prev.map = NULL,
                        ...) {

  if (is.null(prev.map)) {

    lfmap <- leaflet::leaflet(locs)

    if (!is.null(bg)) {
      lfmap <- leaflet::addProviderTiles(lfmap, provider = bg)
    } else {
      lfmap <- leaflet::addTiles(lfmap)
    }

    lfmap <- leaflet::addCircleMarkers(lfmap, stroke = FALSE, fillColor = pcol,
                                       fillOpacity = alpha, radius = 3*psize, ...)

  }

  if (!is.null(prev.map)) {
    lfmap <- leaflet::addCircleMarkers(prev.map, data = locs,
                                       stroke = FALSE, fillColor = pcol,
                                       fillOpacity = alpha, radius = 3*psize, ...)

  }

  return(lfmap)

}













