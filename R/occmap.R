# - possibility to add legend (e.g. species name)
# - possibility to add axes with coordinates
# - possibility to add scale bar (mapmisc::scaleBar)
# - allow for SpatialPoints as well (not only SpatialPointsDataFrame)


#' Plot map of species occurrences
#'
#' Plot map of species occurrences on top of different background layers.
#'
#'
#' @export
#' @return A map (plot), unless \code{bg = 'KML'} in which case a kmz file is saved.
#' @import sp
#' @import rgdal
#' @import raster
#' @importFrom scales alpha
#' @importFrom dismo gmap Mercator
#' @import ggmap
#' @import rworldmap
#' @import leaflet
#' @import mapmisc
#' @author F. Rodriguez-Sanchez. Code for using spplot taken from Oscar Perpinan's blog.
#' @param locs A matrix, dataframe or SpatialPointsDataFrame containing coordinates of species occurrences. If a simple (non-spatial) dataframe,
#' coordinates must be contained in columns named as ("x", "y") or ("lon", "lat"). If a matrix, coordinates must be the first two columns (longitude, latitude).
#' @param projection CRS object giving the projection system of species occurrences.
#' Leave missing if species occurrences are in geographical projection, or 'locs' is a SpatialPoints* object containing projection information (proj4string).
#' @param bg Character. Either
#' 'google' for Google maps background (using dismo::gmap),
#' 'coast' for coastlines (coastsCoarse shapefile from rworldmap package),
#' 'ggmap' for any of the maps provided by get_map function in ggmap package,
#' 'leaflet' for an interactive HTML map using leaflet, or
#' 'mapmisc' for using any of the layers available in \code{mapmisc} package, or
#' 'kml' for producing a KML file to be opened with Google Earth.
#' Alternatively, a Raster object to be used as background.
#' @param pcol Colour to be used for points representing species' occurrences. Default is "red".
#' @param alpha Colour transparency for points between 0 (fully transparent) and 1 (fully opaque).
#' @param psize Point size for depicting species' occurrences. Default is 1 (cex=1).
#' @param add Logical. Add these occurrences to previous map (e.g. for a new species?). Default is FALSE. Note this feature doesn't work for all map types.
#' @param mapmisc_server character. Server/type of background map to be used when bg = "mapmisc". Run \code{osmTiles()} to see all the available layers.
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
#' data(acaule)
#' occmap(locs=acaule)
#' occmap(locs=acaule, bg="google")
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
#' occmap(meuse, projection = CRS("+init=epsg:28992"))



occmap <- function(locs, bg='google', projection, pcol='red', alpha = 1, psize=1, add=FALSE, mapmisc_server = "maptoolkit", ...){

  if (alpha < 1) pcol = scales::alpha(pcol, alpha)


  crs.geo <- CRS("+init=epsg:4326")

  if (is.matrix(locs)) locs <- as.data.frame(locs)

  # Create Spatial Dataframe
  if (class(locs)!= "SpatialPointsDataFrame") {

    # retrieve coordinates
    if (ncol(locs)==2) coord.locs <- locs[, c(1,2)]
    if ("lon" %in% names(locs)) coord.locs <- locs[, c("lon", "lat")]
    if ("x" %in% names(locs)) coord.locs <- locs[, c("x", "y")]

    locs <- subset(locs, !is.na(coord.locs[,1]) & !is.na(coord.locs[,2]))  # coordinates cannot have missing values

    coord.locs <- coord.locs[row.names(locs), ]

    # make spatial dataframe
    coordinates(locs) <- coord.locs

  }


  ### define projection and project to geographical if necessary

  if (!missing(projection)){
    proj4string(locs) <- projection
  }

  # assume geographical coordinates if Spatial object doesn't have projection information (proj4string is NA)
  # and projection argument is not given to the function (projection = NULL)
  if (is.na(proj4string(locs))) proj4string(locs) <- crs.geo

  locs <- spTransform(locs, crs.geo)




  ### PLOTTING ###


  ### Google Maps background

  if (bg=='google'){
    # Get Google map
    locs.GM <- dismo::Mercator(coordinates(locs))
    bgmap <- dismo::gmap(locs.GM, lonlat = TRUE, ...)

    # Plot
    if (add==FALSE) plot(bgmap, axes = FALSE)
    points(coordinates(locs), pch=20, col=pcol, cex=psize)
  }




  ### Coastlines background

  if (bg=='coast'){
    if (add==FALSE){
      data(coastsCoarse) # from rworldmap. Alternatively, use other shapefiles from Natural Earth
      plot(coastsCoarse, xlim=c(min(coordinates(locs)[,1])-1, max(coordinates(locs)[,1])+1),
           ylim=c(min(coordinates(locs)[,2])-1, max(coordinates(locs)[,2])+1), ...)
    }
    points(locs, pch=20, col=pcol, cex=psize)
  }


  ##### Using Oscar Perpinan's approach (spplot) #####
  # see http://procomun.wordpress.com/2013/04/24/stamen-maps-with-spplot/

  if (bg == "spplot"){

    stop("spplot does not work yet") # doesn't work yet

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



  #### Using GGMAP #####

  if (bg == "ggmap"){

    if (add == FALSE){
#     # define area
#     min_lon <- bbox(locs)[1, "min"]
#     max_lon <- bbox(locs)[1, "max"]
#     min_lat <- bbox(locs)[2, "min"]
#     max_lat <- bbox(locs)[2, "max"]
#     center_lon <- min_lon + (max_lon - min_lon)/2
#     center_lat <- min_lat + (max_lat - min_lat)/2
#     map_center <- c(lon = center_lon, lat = center_lat)

    ## get_map
    #bgmap <- get_map(location = map_center, crop = FALSE, ...)
    bblocs <- bbox(locs)
#     bblocs[1, ] <- (bblocs[1, ] - mean(bblocs[1, ])) * 3 + mean(bblocs[1, ])
#     bblocs[2, ] <- (bblocs[2, ] - mean(bblocs[2, ])) * 3 + mean(bblocs[2, ])
    bblocs <- (bblocs - rowMeans(bblocs)) * 1.05 + rowMeans(bblocs)
    # scale longitude and latitude (increase bblocs by 5%)
    # code above taken from R. Lovelace's tutorial
    bgmap <- ggmap::get_map(bblocs, crop=FALSE, ...)

    ## plot
    print(ggmap(bgmap) +
      geom_point(data = data.frame(lon=coordinates(locs)[,1], lat=coordinates(locs)[,2]),
                 aes(x=lon, y=lat), colour=pcol, size=psize, alpha=1) +
      xlab("Longitude") + ylab("Latitude"))

    } else {

      bgmap <- last_plot()
      print(bgmap + geom_point(aes(x=lon, y=lat), colour=pcol, size=psize, alpha=1,
                         data = data.frame(lon=coordinates(locs)[,1], lat=coordinates(locs)[,2])))
    }


  }


#   if (bg == "leafletR"){
#
#     locs.gj <- toGeoJSON(locs)
#     map <- leaflet(locs.gj, incl.data=TRUE, ...)
#     browseURL(map)
#
#   }


  if (bg == "leaflet"){

    bgmap <- leaflet::leaflet(locs) %>%
      fitBounds(bbox(locs)[1,1], bbox(locs)[2,1], bbox(locs)[1,2], bbox(locs)[2,2]) %>%
      addTiles() %>%
      addCircleMarkers(stroke = FALSE, fillColor = pcol, fillOpacity = alpha,
                       radius = 2*psize, ...)
    bgmap

  }






  if (bg == "mapmisc"){

    if (add == FALSE) {
    bgmap <- openmap(locs, path = mapmisc_server)
    map.new(locs, legendRight=FALSE)
    if (nlayers(bgmap) > 1) {
      raster::plotRGB(bgmap, add = TRUE, interpolate = TRUE, ...)
    } else raster::plot(bgmap, add = TRUE, interpolate = TRUE, ...)
    }
    points(locs, pch = 20, col = pcol, cex = psize)

  }


  if (bg == "kml"){

    raster::KML(locs, filename = "occmap.kmz", overwrite = TRUE, ...)
    #rgdal::writeOGR(locs, dsn = "occmap.kml", layer = "locs", driver = "KML")
  }


  if (class(bg) == "Raster*"){

    if (add == FALSE){
    raster::plot(bg, interpolate = TRUE, ...)
    }
    points(locs, pch = 20, col = pcol, cex = psize)
  }



}


