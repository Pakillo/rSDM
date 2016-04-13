#' Move occurrences to the nearest raster cell with data
#'
#' Move point occurrences falling in raster cells without data (i.e. NA) to the nearest raster cell with data.
#'
#' @export
#' @import raster
#' @import sp
#' @importFrom class knn1
#' @param locs A SpatialPoints or SpatialPointsDataFrame.
#' @param ras \code{Raster*} object.
#' @param layer Integer. For RasterStack and RasterBrick objects, layer to use.
#' @param move Logical. Change coordinates of points to those of the nearest raster cells?
#' @param distance Numeric (optional). Maximum distance to move points. Point coordinates are only changed if the distance to the nearest raster cell is below \code{distance}.
#' @param showchanges Logical. Print table with old and new coordinates.
#' @param showmap Logical. Show map with original and new coordinates?
#' @param leaflet Logical. If TRUE, show leaflet map.
#' @return A SpatialPointsDataFrame (with corrected coordinates if move is TRUE).
#' @seealso \url{https://github.com/SEEG-Oxford/seegSDM/blob/master/man/nearestLand.Rd} and
#' \url{http://stackoverflow.com/questions/26652629/extracting-a-value-from-a-raster-for-a-specific-point-based-on-the-closest-cell/26688361#26688361}.
#' @examples
#' \donotrun{
#' data(acaule)
#' locs <- locs2sp(acaule)
#' ras <- getData('worldclim', var = 'tmin', res = 10)
#' check.coords <- points2nearestcell(locs, ras)
#' }

points2nearestcell <- function(locs, ras, layer = 1,
                               move = TRUE, distance = NULL,
                               showchanges = TRUE, showmap = TRUE, leaflet = FALSE) {

  miss <- point_in_cell(locs, ras, layer)

  ## if there are NA cells...

  if (sum(miss) > 0){

    coord.miss <- coordinates(locs[miss, ])  # points without data
    if (nlayers(ras) > 1) ras <- raster(ras, layer)
    cells.notNA <- rasterToPoints(ras, spatial = TRUE)  # get coordinates of cells with data
    coord.ras <- coordinates(cells.notNA)
    cell.id <- factor(1:nrow(coord.ras))

    # find the nearest raster cell for each point with missing data
    nearest.cell <- class::knn1(coord.ras, coord.miss, cell.id)

    new.coords <- coord.ras[nearest.cell, ]
    colnames(new.coords) <- c("longitude_new", "latitude_new")

    if (!is.null(distance)){

      # calculate distances between old and new coordinates
      distances <- pointDistance(coord.miss, new.coords,
                            lonlat = compareCRS(locs, "+init=epsg:4326"))
      # if distance below threshold, accept, otherwise keep old coordinates
      x <- ifelse(distances < distance, new.coords[,1], coord.miss[,1])
      y <- ifelse(distances < distance, new.coords[,2], coord.miss[,2])
      new.coords <- cbind(longitude_new = x, latitude_new = y)

    }


    if (move){   # assign new coordinates to those points
      locs@coords[miss, ] <- new.coords
    }


    if (showchanges) {
      print(cbind(coord.miss, new.coords))
    }


    if (showmap){

      if (leaflet){

        map <- occmap(new.coords, proj = proj4string(locs), bg = "leaflet", pcol = "black")
        map <- occmap(coord.miss, proj = proj4string(locs), bg = "leaflet",
                      add = TRUE, leaflet.base = map, pcol = "red")
        print(map)

      } else {

        plot(ras, legend = FALSE,
             main = "Points moved to nearest raster cell",
             ext = extent(rbind(1.1*coord.miss, 1.1*new.coords)))
        points(new.coords, pch = 16, col = "black")
        points(coord.miss, pch = 16, col = "red")
        segments(coord.miss[, 1], coord.miss[, 2],
                 new.coords[, 1], new.coords[, 2])

      }

    }


  }


  return(locs)

}
