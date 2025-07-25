#' Move point occurrences to the nearest raster cell with data
#'
#' Move point occurrences falling in raster cells without data (NA) to the
#' nearest raster cell with data.
#'
#' @param locs An [sf::sf()] or [terra::SpatVector()] object with point coordinates,
#' e.g. as generated from [locs2sf()] or [locs2vect()].
#' @param ras [terra::SpatRaster()] object.
#' @param layer Integer. Raster layer to use for comparing with point locations (default = 1).
#' @param move Logical. Change coordinates of points to those of the nearest raster cells?
#' If FALSE, the function will show the nearest raster cells but coordinates of `locs`
#' will not be changed.
#' @param distance Numeric (optional). Maximum distance to move points.
#' Point coordinates are only changed if the distance to the nearest raster cell
#' is below `distance`.
#' @param table Logical. Print table with old and new coordinates?
#' @param map Character. One of "none", "base", "ggplot" or "leaflet", to choose
#' the type of map showing the old and new point coordinates. See [occmap()].
#'
#' @export
#'
#' @return An [sf::sf()] or [terra::SpatVector()] object
#' (with corrected coordinates if move is TRUE).
#'
#' @seealso <https://search.r-project.org/CRAN/refmans/spatstat.geom/html/nearest.raster.point.html>
#' and <https://search.r-project.org/CRAN/refmans/gecko/html/move.html>.
#'
#' @examples
#'
#' ## Generate example point coordinates and raster
#' locs <- data.frame(lon = c(1, 2, 1, 2, 2.2), lat = c(1.2, 1, 2.3, 3, 2))
#' locs.sf <- locs2sf(locs)
#' library(terra)
#' ras <- rast(nrows = 2, ncols = 2, xmin = 0.5, xmax = 3.5, ymin = 0.5, ymax = 3.5,
#'  resolution = 1, vals = c(NA, 1, 1, NA, NA, 1, NA, 1, 1))
#' occmap(locs.sf, ras, pcol = "black", psize = 3)
#'
#' ## Move point coordinates falling outside of raster
#' moved <- points2nearestcell(locs.sf, ras)
#' moved
#'
#' ## Move points only if moving distance is lower than specified threshold:
#' moved <- points2nearestcell(locs.sf, ras, distance = 100000)


points2nearestcell <- function(locs = NULL,
                               ras = NULL,
                               layer = 1,
                               move = TRUE,
                               distance = NULL,
                               table = TRUE,
                               map = c("base", "ggplot", "leaflet", "none")
) {

  map <- match.arg(map)

  locs.orig <- locs

  if (inherits(locs, "SpatVector")) {
    locs <- sf::st_as_sf(locs)
  }

  miss <- point_in_cell(locs, ras, layer)


  ## if there are NA cells...

  if (sum(miss) > 0) {

    miss.sf <- locs[miss, ]
    coord.miss <- sf::st_coordinates(miss.sf)  # points without data

    ras <- terra::subset(ras, subset = layer)
    coord.ras <- terra::crds(ras, df = FALSE, na.rm = TRUE)

    cell.id <- factor(seq_len(nrow(coord.ras)))

    # find the nearest raster cell for each point with missing data
    nearest.cell <- class::knn1(coord.ras, coord.miss, cell.id)

    new.coords <- coord.ras[nearest.cell, , drop = FALSE]
    colnames(new.coords) <- c("X_new", "Y_new")
    new.coords <- as.data.frame(new.coords)

    if (!is.null(distance)) {

      new.coords.sf <- sf::st_as_sf(new.coords, coords = c("X_new", "Y_new"),
                                    crs = sf::st_crs(locs))

      # calculate distances between old and new coordinates
      distances <- as.numeric(sf::st_distance(miss.sf, new.coords.sf, by_element = TRUE))
      # if distance below threshold, accept, otherwise keep old coordinates
      x <- ifelse(distances < distance, new.coords[,1], coord.miss[,1])
      y <- ifelse(distances < distance, new.coords[,2], coord.miss[,2])
      new.coords <- data.frame(X_new = x, Y_new = y)

    }

    new.coords.sf <- sf::st_as_sf(new.coords, coords = c("X_new", "Y_new"),
                                  crs = sf::st_crs(locs))


    if (isTRUE(move)) {   # assign new coordinates to those points
      sf::st_geometry(locs[miss, ]) <- sf::st_geometry(new.coords.sf)
    }


    if (isTRUE(table)) {

      coords <- data.frame(coord.miss, sf::st_coordinates(locs[miss, ]))
      names(coords) <- c("X", "Y", "X_new", "Y_new")
      distance <- sf::st_distance(miss.sf, new.coords.sf, by_element = TRUE)
      moved <- apply(coords, 1, function(x){
        names(x) <- NULL
        !isTRUE(identical(x[1], x[3]) & identical(x[2], x[4]))
      })
      message(sum(moved), " out of ", nrow(locs), " points have been moved.")
      coords <- cbind(coords, distance, moved)
      row.names(coords) <- NULL
      print(coords)
    }


    if (map != "none") {

      if (map == "base") {
        occmap(new.coords.sf, ras, pcol = "black", psize = 2, legend = FALSE,
               main = "Points moved to nearest raster cell")
        occmap(miss.sf, add = TRUE, pcol = "red", psize = 2)
        graphics::segments(coord.miss[, 1], coord.miss[, 2],
                 new.coords[, 1], new.coords[, 2])
      }

      if (map == "leaflet") {
        outmap <- occmap(new.coords.sf, type = "leaflet", pcol = "black", bg = NULL)
        outmap <- occmap(miss.sf, type = "leaflet", prev.map = outmap, pcol = "red")
        for (i in 1:nrow(coord.miss)) {
          outmap <- leaflet::addPolylines(outmap,
                                          lng = c(coord.miss[i, 1], new.coords[i, 1]),
                                          lat = c(coord.miss[i, 2], new.coords[i, 2]))
        }
        print(outmap)
      }

      if (map == "ggplot") {
        outmap <- occmap(new.coords.sf, ras, type = "ggplot", pcol = "black", psize = 2) +
          ggplot2::theme(legend.position = "none")
        outmap <- occmap(miss.sf, type = "ggplot", prev.map = outmap,
                         pcol = "red", psize = 2)
        coords.change <- data.frame(point = rep(as.character(1:nrow(coord.miss)), times = 2),
                                    lon = c(coord.miss[, 1], new.coords[, 1]),
                                    lat = c(coord.miss[, 2], new.coords[, 2])
        )
        outmap <- outmap +
          ggplot2::geom_line(data = coords.change, ggplot2::aes(x = lon, y = lat, group = point)) +
          ggplot2::labs(title = "Points moved to nearest raster cell")
        print(outmap)

      }

    }

  } else {
    message("All points fall within a raster cell")
  }


  if (inherits(locs.orig, "SpatVector")) {
    locs <- terra::vect(locs)
  }

  return(locs)

}
