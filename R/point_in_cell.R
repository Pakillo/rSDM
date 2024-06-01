#' Check if points (occurrences) fall within raster cells with data
#'
#' This function examines which points fall within a raster cell with data (not NA).
#' Returns TRUE for points falling in a raster cell with data, and FALSE otherwise.
#'
#' @param locs An [sf::sf()] or [terra::SpatVector()] object with point coordinates,
#' e.g. as generated from [locs2sf()] or [locs2vect()].
#' @param ras [terra::SpatRaster()] object
#' @param layer Integer. Raster layer to use for comparing with point locations (default = 1).
#'
#' @return A logical vector.
#' @export
#' @examples
#' locs <- data.frame(lon = c(1, 2, 1, 2), lat = c(1, 1, 2, 3))
#' locs.sf <- locs2sf(locs)
#' library(terra)
#' ras <- rast(nrows = 2, ncols = 2, xmin = 0.5, xmax = 3.5, ymin = 0.5, ymax = 3.5,
#'  resolution = 1, vals = c(NA, 1, 1, NA, NA, 1, NA, 1, 1))
#' occmap(locs.sf, ras, pcol = "black", psize = 3)
#'
#' point_in_cell(locs.sf, ras)
#'
#' # adding column to original point data
#' locs.sf$inras <- point_in_cell(locs.sf, ras)
#' locs.sf
#'
point_in_cell <- function(locs = NULL, ras = NULL, layer = 1) {

  if (!inherits(locs, "sf")) {
    if (!inherits(locs, "SpatVector")) {
      stop("locs must be a sf or SpatVector object. You may find 'locs2sf()' or 'locs2vect()' useful.")
    }
  }

  stopifnot(is.numeric(layer))
  stopifnot(layer >= 1 && layer <= terra::nlyr(ras))

  locs.terra <- terra::vect(locs)

  if (!isTRUE(terra::same.crs(locs.terra, ras))){
    stop("Coordinate data and raster object must have the same projection.")

  }

  ## Get NA cells
  rasvals <- terra::extract(ras, locs.terra, layer = layer, raw = TRUE, ID = FALSE)
  missing <- is.na(rasvals)
  missing[, "value"]

}
