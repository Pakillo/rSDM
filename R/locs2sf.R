#' Convert matrix or dataframe with point coordinates to a spatial (sf) object
#'
#' @param locs A matrix or data.frame containing point coordinates data.
#' If a matrix, the first two columns will be assumed to contain longitude
#'  and latitude coordinates, respectively.
#' If a dataframe, the function will try to guess the columns containing the coordinates
#' based on column names.
#' @param crs A character string, number (EPSG value), or `crs` object specifying the
#' coordinate reference system (see [sf::st_crs()] or <https://spatialreference.org>).
#' Default is geographic (unprojected) coordinates, datum WGS84 (EPSG = 4326).
#' @param lon.col Character (optional). Name of the column containing longitude data.
#' @param lat.col Character (optional). Name of the column containing latitude data.
#' @return An [sf::sf()] object.
#' @export
#'
#' @examples
#' locs <- matrix(runif(20), ncol = 2)
#' locs.sf <- locs2sf(locs)
#'
#' locs <- data.frame(species = rep("Laurus nobilis", 10), x = runif(10), y = runif(10))
#' locs.sf <- locs2sf(locs)
locs2sf <- function(locs, crs = 4326, lon.col = NULL, lat.col = NULL) {

  if (!inherits(locs, c("matrix", "data.frame"))) stop("locs must be a matrix or data.frame")

  if (is.matrix(locs)){
    message("As locs is a matrix, assuming first two columns are longitude and latitude, respectively.")
    locs <- as.data.frame(locs)
    names(locs)[1:2] <- c("lon", "lat")
  }

  if (ncol(locs) < 2) stop("locs must be a matrix or dataframe with at least 2 columns.")

  # This version adapted from mapr::occ2sp

  if (ncol(locs) > 1){
    locs <- guess_latlon(locs, lat = lat.col, lon = lon.col)    # guess columns with coordinate data
    locs <- locs[!is.na(locs$longitude) & !is.na(locs$latitude), ]  # omit cases without coordinates
    locs.sf <- sf::st_as_sf(locs, coords = c("longitude", "latitude"), crs = crs)
    return(locs.sf)
  }

}
