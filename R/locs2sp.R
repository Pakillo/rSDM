#' Convert matrix or dataframe with coordinates to a spatial object
#'
#' @param locs A matrix or dataframe containing coordinates data. If a matrix, the first two columns will be assumed to contain longitude and latitude coordinates, respectively. If a dataframe, the function will try to guess the columns containing the coordinates based on column names.
#' @param proj Character string specifying the projection (see \code{\link[sp]{proj4string}} or
#' \url{http://spatialreference.org}). Default is geographic (unprojected) coordinates, datum WGS84.
#' @param lon.col Character (optional). Name of the column containing longitude data.
#' @param lat.col Character (optional). Name of the column containing latitude data.
#' @return A SpatialPoints or SpatialPointsDataFrame.
#' @export
#' @import sp
#'
#' @examples
#' locs <- matrix(runif(20), ncol = 2)
#' locs.sp <- locs2sp(locs)
#'
#' locs <- data.frame(species = rep("Laurus nobilis", 10), x = runif(10), y = runif(10))
#' locs.sp <- locs2sp(locs)
locs2sp <- function(locs, proj = "+init=epsg:4326", lon.col = NULL, lat.col = NULL){

  if (! class(locs) %in% c("matrix", "data.frame")) stop("locs must be a matrix or dataframe")

  if (is.matrix(locs)){
    message("As locs is a matrix, assuming first two columns are longitude and latitude, respectively.")
    locs <- as.data.frame(locs)
    names(locs)[1:2] <- c("x", "y")
  }

  if (ncol(locs) < 2) stop("locs must be a matrix or dataframe with at least 2 columns.")

  # This version adapted from mapr::occ2sp

  if (ncol(locs) > 1){
    locs <- guess_latlon(locs, lat = lat.col, lon = lon.col)    # guess columns with coordinate data
    locs <- locs[complete.cases(locs$longitude, locs$latitude), ]  # omit cases without coordinates
    coordinates(locs) <- c("longitude", "latitude")
    proj4string(locs) <- sp::CRS(proj)   # define projection
    return(locs)
  }



}
