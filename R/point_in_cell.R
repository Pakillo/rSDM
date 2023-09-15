#' Check if points (occurrences) fall within raster cells with data
#'
#' This function examines which points fall within a raster cell with data (not NA).
#' Returns TRUE for points falling in a raster cell with data, and FALSE otherwise.
#'
#' @param locs An [sf::sf()] object with point coordinates.
#' @param ras [terra::SpatRaster-class()] object
#' @param layer Integer. Raster layer to use for comparing with point locations (default = 1).
#'
#' @return A logical vector.
#' @export
#' @examples
#' points <- data.frame(lon = c(1, 2, 1, 2), lat = c(1, 1, 2, 3))
#' points
#' points.sf <- sf::st_as_sf(points, coords = c("lon", "lat"), crs = 4326)
#' ras <- rast(nrows = 2, ncols = 2, xmin = 0.5, xmax = 3.5, ymin = 0.5, ymax = 3.5,
#' resolution = 1, vals = c(NA, 1, 1, NA, NA, 1, NA, 1, 1))
#' plot(ras)
#' plot(points.sf, add = T)
#'
#' point_in_cell(locs, ras)
#'
point_in_cell <- function(locs, ras, layer = 1) {

  if (!inherits(locs, "sf")) {
    stop("locs must be a sf object. You may find 'locs2sf()' useful.")
  }

  locs.terra <- terra::vect(locs)

  if (!isTRUE(terra::same.crs(locs.terra, ras))){
    stop("Coordinate data and raster object must have the same projection.")

  }

  ## Get NA cells
  rasvals <- terra::extract(ras, locs.terra, layer = layer, raw = TRUE, ID = FALSE)
  missing <- is.na(rasvals)
  missing

}
