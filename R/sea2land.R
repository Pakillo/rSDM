# TODO:
# Set maximum distance to move points. Beyond that, set NA or leave original coordinates.

#' Move occurrences to the nearest raster cell with data
#'
#' Move point occurrences falling 'on sea' (i.e. NA) to the nearest raster cell with data ('land').
#'
#' @export
#' @import raster
#' @importFrom class knn1
#' @author F. Rodriguez-Sanchez.
#' @param locs A matrix, dataframe or SpatialPointsDataFrame containing coordinates of species occurrence. #' If a simple (non-spatial) dataframe, coordinates must be contained in columns named as ("x", "y") or #' #' ("lon", "lat"). If a matrix, coordinates must be the first two columns (longitude, latitude).
#' @param ras \code{Raster*} object containing the environmental (climatic) layers.
#' @param showmap Logical. Show map with original and new coordinates?
#' @return A SpatialPointsDataFrame with corrected coordinates.
#' @seealso \url{https://github.com/SEEG-Oxford/seegSDM/blob/master/man/nearestLand.Rd} and
#' \url{http://stackoverflow.com/questions/26652629/extracting-a-value-from-a-raster-for-a-specific-point-based-on-the-closest-cell/26688361#26688361}.
#' @examples
#' \donotrun{
#' data(acaule)
#' ras <- getData('worldclim', var = 'tmin', res = 10)
#' check.coords <- sea2land(locs = acaule, ras)
#' }

sea2land <- function(locs, ras, showmap = TRUE) {

  ## check occurrence data
  if (! class(locs) %in% c("Matrix", "data.frame", "SpatialPointsDataFrame")){
    stop("locs must be a Matrix, dataframe or SpatialPointsDataFrame")
  }

  if (is.matrix(locs)) locs <- as.data.frame(locs)

  ## Create Spatial Dataframe
  if (class(locs)!= "SpatialPointsDataFrame") {

    # retrieve coordinates
    if (ncol(locs)==2) coord.locs <- locs[, c(1,2)]
    if ("lon" %in% names(locs)) coord.locs <- locs[, c("lon", "lat")]
    if ("x" %in% names(locs)) coord.locs <- locs[, c("x", "y")]

    locs.sp <- subset(locs, !is.na(coord.locs[,1]) & !is.na(coord.locs[,2]))  # coordinates cannot have missing values

    coord.locs <- coord.locs[row.names(locs.sp), ]

    # make spatial dataframe
    coordinates(locs.sp) <- coord.locs

  }

  if (class(locs) == "SpatialPointsDataFrame") locs.sp <- locs



  ## check raster. Take only first layer
  if (nlayers(ras) > 1) ras <- raster(ras, 1)


  ## Get NA cells

  rasvals <- extract(ras, locs.sp)
  missing <- is.na(rasvals)

  ## if there are NA cells...
  if (sum(missing) > 0){

    misvals <- locs.sp[missing, ]
    coord.misvals <- coordinates(misvals)
    raster.cells <- rasterToPoints(ras, spatial=TRUE)  # get coordinates of cells with data ('land')
    coord.raster <- coordinates(raster.cells)
    cell.id <- factor(1:nrow(coord.raster))

    # find the nearest raster cell for each point with missing data
    near.cell <- class::knn1(coord.raster, coord.misvals, cell.id)
    locs.sp@coords[missing, ] <- coord.raster[near.cell, ]    # assign new coordinates to those points

  }

  if (showmap){

    plot(ras, legend=FALSE, main = "Points moved to nearest land cell")
    plot(misvals, add = TRUE, pch = 16, col = "red")
    plot(locs.sp[missing, ], add = TRUE, pch = 16, col = "black")
    arrows(coord.misvals[, 1], coord.misvals[, 2],
           locs.sp@coords[missing, 1], locs.sp@coords[missing, 2])

  }

#   if (spatial == FALSE){
#
#     if (class(locs) == "data.frame"){
#       if ("lon" %in% names(locs)) {
#         locs$lon[!missing] <- coordinates(locs.sp)[, 1]
#         locs$lat[!missing] <- coordinates(locs.sp)[, 2]
#       }
#       if ("x" %in% names(locs)) {
#         locs$x[!missing] <- coordinates(locs.sp)[, 1]
#         locs$y[!missing] <- coordinates(locs.sp)[, 2]
#       }
#
#     }
#
#     return(locs)
#
#   } else return(locs.sp)

  return(locs.sp)

}
