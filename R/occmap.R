# TO DO:
# - possibility to add legend (e.g. species name)
# - possibility to add axes with coordinates
# - possibility to add scale bar (mapmisc::scaleBar)



#' Map species occurrences
#'
#' Plot map of species occurrences (or any set of georeferenced points) on top of different background layers.
#'
#'
#' @export
#' @return A map (plot), unless \code{bg = 'KML'} in which case a kmz file is saved to be explored with Google Earth. In some cases, a raster layer, leaflet object, or ggplot object is returned in addition to the map.
#' @import sp
#' @import rgdal
#' @importFrom scales alpha
#' @param locs A matrix, dataframe, SpatialPoints or SpatialPointsDataFrame containing coordinates of species occurrences. If locs is a matrix or dataframe, it will be converted to a spatial object using \code{\link{locs2sp}}.
#' @param proj Character string specifying the projection of coordinates data (see \code{\link[sp]{proj4string}} or \url{http://spatialreference.org}). Default is geographic (unprojected) coordinates, datum WGS84. Not used if locs is already an Spatial object with defined projection.
#' @param ras Raster* object to be used as background for points. Default is NULL, in which case a background map defined by \code{bg} will be used.
#' @param bg Type of background map. Either
#' 'google' for Google maps background (using \code{\link[dismo]{gmap}}) (default),
#' 'coast' for coastlines (using \code{coastsCoarse} from \pkg{rworldmap} package),
#' 'ggmap' for any of the maps provided by \code{\link[ggmap]{get_map}} in \pkg{ggmap} package,
#' 'leaflet' for an interactive HTML map using \pkg{leaflet},
#' 'mapmisc' for using any of the layers available in \pkg{mapmisc} package, or
#' 'kml' for producing a KMZ file to be opened with Google Earth.
#' @param pcol Colour to be used for points. Default is "red".
#' @param alpha Colour transparency for points, between 0 (fully transparent) and 1 (fully opaque).
#' @param psize Point size. Default is 1 (cex = 1).
#' @param add Logical. Add these occurrences to a previous map? (e.g. for a new species). Default is FALSE. Note this feature doesn't work for all map types. For leaflet maps, when add = TRUE, a leaflet basemap (e.g. as produced by a previous call to occmap) must be provided (see \code{leaflet.base}).
#' @param leaflet.base Leaflet map to be used as basemap to add further points when add is TRUE.
#' @param mapmisc_server character. Server/type of background map to be used when bg = "mapmisc". Run \code{\link[mapmisc]{osmTiles}} to see all the available layers.
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
#'
#' # Using acaule dataset from dismo package:
#' library(dismo)
#' data(acaule)
#' occmap(locs=acaule)
#' occmap(locs=acaule, bg="google", type="satellite")   # using options from dismo:gmap
#'
#'
#' # Watercolor map from Stamen using ggmap
#' # NB: use small regions, otherwise give error to download map tiles
#' locs_redux <- subset(acaule, lon>-80 & lon< -60 & lat>-30 & lat< -10)
#' occmap(locs=locs_redux,
#'            bg="ggmap", maptype='watercolor', source='stamen',
#'            pcol="darkgreen", psize=4)
#'
#'
#' # Plot occurrences in a specific country:
#' occmap(locs=subset(acaule, country=="Bolivia"))
#'
#' # Plot occurrences within given coordinates
#' occmap(locs=subset(acaule, lon>-80 & lon< -60 & lat>-30 & lat< -10))
#' # can use click() aftewards to identify specific points
#'
#'
#' # Add transparency to points
#' occmap(locs=acaule, pcol="red", alpha=0.7)
#'
#' # Save plot directly to file
#' pdf("map.pdf", paper="a4r")
#' occmap(locs=acaule, type="satellite", scale=2)
#' dev.off()
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



occmap <- function(locs, ras = NULL, bg = 'google', proj = "+init=epsg:4326",
                   pcol = 'red', alpha = 1, psize = 1,
                   add = FALSE, leaflet.base,
                   mapmisc_server = "maptoolkit", filename = "occmap.kmz",
                   ...){


  # Create Spatial object
  if (!class(locs) %in% c("SpatialPoints", "SpatialPointsDataFrame")) {
    locs <- locs2sp(locs, proj = proj)
  }

  if (is.na(proj4string(locs))) proj4string(locs) <- CRS(proj)

  ## Project to geographical for mapping
  locs <- spTransform(locs, CRS("+init=epsg:4326"))

  if (alpha < 1) pcol = scales::alpha(pcol, alpha)  # transparency




  ### PLOTTING ###


  if (!is.null(ras)){   # if raster provided, use it as background

    map_raster(locs, ras, add = add, pcol = pcol, psize = psize, ...)

  } else {

    if (isTRUE(add)){

      map_raster(locs, ras, add = add, pcol = pcol, psize = psize, ...)

    } else {      # use map as defined by 'bg'

      ### Google Maps background (using dismo::gmap) ###

      if (bg == 'google'){
        bgmap <- map_gmap(locs, pcol = pcol, psize = psize, add = add, ...)
      }


      ### KML ###

      if (bg == "kml")
        map_kml(locs, filename = filename, ...)


      ### Coastlines only ###

      if (bg == 'coast')
        map_coast(locs, add = add, pcol = pcol, psize = psize, ...)



      ### Leaflet map ###

      if (bg == "leaflet"){
        bgmap <- map_leaflet(locs, pcol = pcol, alpha = alpha, psize = psize,
                             add = add, prev.map = leaflet.base, ...)
      }



      ### ggmap ###

      if (bg == "ggmap"){
        bgmap <- map_ggmap(locs, add = add, pcol = pcol, psize = psize, ...)
      }




      ##### Using Oscar Perpinan's approach (spplot) #####

      if (bg == "spplot")
        map_spplot(locs)


      ### using mapmisc ###

      if (bg == "mapmisc"){
        bgmap <- map_mapmisc(locs, add = add, pcol = pcol, psize = psize, ...)
      }

    }

  }


  if (exists("bgmap")) invisible(bgmap)

}









###### INDIVIDUAL PLOTTING FUNCTIONS ######


#### Plot onto google map through dismo ####

#' @importFrom dismo Mercator gmap
#' @importFrom raster plot

map_gmap <- function(locs, pcol, psize, add, ...){

  locs.GM <- dismo::Mercator(coordinates(locs))
  bgmap <- dismo::gmap(locs.GM, lonlat = TRUE, ...)

  # Plot
  if (add == FALSE) plot_gmap(bgmap, interpolate = TRUE)
  points(coordinates(locs), pch=20, col=pcol, cex=psize)
  invisible(bgmap)

}





#### Generate KML/KMZ file for Google Earth ####

#' @importFrom raster KML

map_kml <- function(locs, filename, ...){
  raster::KML(locs, filename = filename, overwrite = TRUE, ...)
}



#### Map onto user-provided raster ####

#' @importFrom raster plot
#' @importFrom graphics points

map_raster <- function(locs, ras, add, pcol, psize, ...){

  if (add == FALSE) {
    if (length(ras@legend@colortable) > 0) {
      plot_gmap(ras, interpolate = TRUE, ...)
    } else {
      raster::plot(ras, interpolate = TRUE, ...)
    }

  }
  points(locs, pch = 20, col = pcol, cex = psize)

}


### Coastlines only ####

map_coast <- function(locs, add, pcol, psize, ...){

  if (add==FALSE){
    plot(coastsCoarse,
         xlim=c(min(coordinates(locs)[,1])-1, max(coordinates(locs)[,1])+1),
         ylim=c(min(coordinates(locs)[,2])-1, max(coordinates(locs)[,2])+1), ...)
  }
  points(locs, pch=20, col=pcol, cex=psize)
}



### Leaflet maps ###

#' @import leaflet

map_leaflet <- function(locs, pcol, alpha, psize, add, prev.map, ...){

  if (add == FALSE){

  bgmap <- leaflet(locs) %>%
    fitBounds(bbox(locs)[1,1], bbox(locs)[2,1], bbox(locs)[1,2], bbox(locs)[2,2]) %>%
    addTiles() %>%
    addCircleMarkers(stroke = FALSE, fillColor = pcol, fillOpacity = alpha,
                     radius = 3*psize, ...)


  } else {
    bgmap <- prev.map %>%
      addCircleMarkers(data = locs,
                       stroke = FALSE, fillColor = pcol, fillOpacity = alpha,
                       radius = 3*psize, ...)

  }

  print(bgmap)
  invisible(bgmap)
}




### GGMAP ###

#' @importFrom sp coordinates
#' @import ggmap ggplot2

map_ggmap <- function(locs, add, pcol, psize, ...){

  if (add == FALSE){
    bblocs <- bbox(locs)
    bblocs <- (bblocs - rowMeans(bblocs)) * 1.05 + rowMeans(bblocs)
          # increase bblocs by 5% (as in R. Lovelace's tutorial)
    bgmap <- ggmap::get_map(bblocs, crop = FALSE, ...)

    ## plot
    bgmap <- ggmap(bgmap) +
            geom_point(data = data.frame(lon=coordinates(locs)[,1], lat=coordinates(locs)[,2]),
                       aes(x=lon, y=lat), colour=pcol, size=psize, alpha=1) +
            xlab("Longitude") + ylab("Latitude")


  } else {

    bgmap <- last_plot() +
      geom_point(aes(x=lon, y=lat), colour=pcol, size=psize, alpha=1,
                 data = data.frame(lon=coordinates(locs)[,1], lat=coordinates(locs)[,2]))

  }

  print(bgmap)
  invisible(bgmap)

}


### Using spplot a la Perpiñan ###
# see http://procomun.wordpress.com/2013/04/24/stamen-maps-with-spplot/

#' @importFrom ggmap get_map

map_spplot <- function(locs){

  stop("map_spplot does not work yet")

  ## Download stamen tiles using the bounding box of the SpatialPointsDataFrame object
  #bbPoints <- bbox(caPV)
  gmap <- get_map(c(bbox(locs)), crop = FALSE, maptype='terrain')  # source = "stamen", maptype = "watercolor"

  ## http://spatialreference.org/ref/sr-org/6864/
  ## Bounding box of the map to resize and position the image with grid.raster
  bbMap <- attr(gmap, 'bb')
  latCenter <- with(bbMap, ll.lat + ur.lat)/2
  lonCenter <- with(bbMap, ll.lon + ur.lon)/2
  height <- with(bbMap, ur.lat - ll.lat)
  width <- with(bbMap, ur.lon - ll.lon)

  ## Use sp.layout of spplot: a list with the name of the function
  ## ('grid.raster') and its arguments
  sp.raster <- list('grid.raster', gmap,
                    x=lonCenter, y=latCenter,
                    width=width, height=height,
                    default.units='native')

  #     ## Define classes and sizes of the circle for each class
  #     breaks <- c(100, 200, 500, 1e3, 25e3)
  #     classes <- cut(caPV$Pdc.kW, breaks)
  #     meds <- tapply(caPV$Pdc.kW, classes, FUN=median)
  #     sizes <- (meds/max(meds))^0.57 * 1.8

  ## Finally, the spplot function
  spplot(locs[, "lon"],
         #  cuts = breaks,
         col.regions=pcol,   #brewer.pal(n=1, 'Greens'),
         #   cex=sizes,
         edge.col='black', alpha=0.7,
         scales=list(draw=FALSE), #key.space='right',
         sp.layout=sp.raster)
}




### using mapmisc ###

#' @importFrom mapmisc openmap map.new
#' @importFrom raster plot plotRGB nlayers

map_mapmisc <- function(locs, add, pcol, psize, ...){

  if (add == FALSE) {

    bgmap <- openmap(locs, path = mapmisc_server)
    map.new(locs, legendRight = FALSE)

    if (nlayers(bgmap) > 1) {
      raster::plotRGB(bgmap, add = TRUE, interpolate = TRUE, ...)
    } else
      raster::plot(bgmap, add = TRUE, interpolate = TRUE, ...)
  }

  points(locs, pch = 20, col = pcol, cex = psize)

  invisible(bgmap)

}


