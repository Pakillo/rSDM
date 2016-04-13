#' Do points (occurrences) fall within raster cells?
#'
#' This function examines which points fall within a raster cell with data (not NA). Returns TRUE for points falling in a raster cell with data, and FALSE otherwise.
#'
#' @inheritParams occmap
#' @param ras Raster* object
#' @param layer Integer. For RasterStack and RasterBrick objects, which layer to use to compare with point locations (default = 1).
#'
#' @return A logical vector.
#' @export
#' @importFrom sp proj4string CRS
#' @importFrom raster compareCRS nlayers raster extract
#'
point_in_cell <- function(locs, ras, layer = 1){

  if (!isTRUE(raster::compareCRS(locs, ras))){
    stop("Coordinate data and raster object must have the same projection. Check their CRS or proj4string")

  } else {

    if (nlayers(ras) > 1) ras <- raster(ras, layer)
    ## Get NA cells
    rasvals <- extract(ras, locs)
    missing <- is.na(rasvals)
    missing

  }

}
