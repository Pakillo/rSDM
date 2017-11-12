
#' Plot Google Earth images obtained through dismo::gmap
#'
#' This function is very slightly modified from \code{.plotCT} function in \code{raster} package to avoid producing an empty plot before the actual Google image.
#'
#' @param x RasterLayer, as obtained through \code{\link[dismo]{gmap}}.
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot. If \code{maxpixels < ncell(x)}, \code{sampleRegular} is used before plotting. If \code{gridded=TRUE} maxpixels may be ignored to get a larger sample.
#' @param ext An extent object to zoom in a region
#' @param interpolate Logical. Should the image be interpolated (smoothed)?
#' @param axes not used
#' @param main character. Main plot title
#' @param xlab Optional. x-axis label
#' @param ylab Optional. y-axis label
#' @param asp not used
#' @param add Logical. Add to current plot?
#' @param addfun Function to add additional items such as points or polygons to the plot (map). Typically containing statements like "points(xy); plot(polygons, add=TRUE)". This is particularly useful to add something to each map when plotting a multi-layer Raster* object.
#' @param zlim not used
#' @param zlimcol not used
#' @param ... Graphical parameters. Any argument that can be passed to \code{\link[graphics]{rasterImage}}.
#'
#' @return A plot.
#' @export
#'
#' @examples
#' \dontrun{
#' library(dismo)
#' g <- gmap("France")
#' plot_gmap(g)
#' }
plot_gmap <- function(x, maxpixels=500000, ext=NULL, interpolate=FALSE, axes, main, xlab='', ylab='', asp, add=FALSE, addfun=NULL, zlim=NULL, zlimcol=NULL, ...) {
  # plotting with a color table

  if (missing(main)) {
    main <- ''
  }

  sethook <- FALSE
  if (!add) {
    #graphics::plot.new()    # just commented this line to avoid blank plot
    if (missing(axes)) {
      axes <- FALSE
    }
    if (!axes) {
      # if (main != "") { } else {
      old.par <- graphics::par(no.readonly = TRUE)
      graphics::par(plt=c(0,1,0,1))
      sethook <- TRUE
    }
    if (missing(asp)) {
      if (raster::couldBeLonLat(x)) {
        ym <- mean(c(x@extent@ymax, x@extent@ymin))
        asp <- 1/cos((ym * pi)/180)
      } else {
        asp <- 1
      }
    }
  }
  coltab <- colortable(x)
  x <- raster::sampleRegular(x, maxpixels, ext=ext, asRaster=TRUE, useGDAL=TRUE)
  z <- raster::getValues(x)


  if (!is.null(zlim)) { # not that relevant here, but for consistency....
    if (is.null(zlimcol)) {
      z[ z<zlim[1] ] <- zlim[1]
      z[ z>zlim[2] ] <- zlim[2]
    } else { #if (is.na(zlimcol)) {
      z[z<zlim[1] | z>zlim[2]] <- NA
    }
  }


  if (NCOL(coltab) == 2) {
    # not implemented
    z <- as.numeric(cut(z, coltab[,1]))
    coltab <- as.vector(coltab[,2])
  }

  z <- z + 1
  z[is.na(z)] <- 1
  if (! is.null(coltab) ) {
    z <- matrix(coltab[z], nrow=nrow(x), ncol=ncol(x), byrow=T)
    z <- as.raster(z)
  } else {
    z <- matrix(z, nrow=nrow(x), ncol=ncol(x), byrow=T)
    z <- as.raster(z, max=max(z)) #, na.rm=TRUE))
  }

  requireNamespace("grDevices")
  bb <- as.vector(t(bbox(x)))

  if (! add) {
    plot(c(bb[1], bb[2]), c(bb[3], bb[4]), type = "n", xlab=xlab, ylab=ylab, asp=asp, axes=axes, main=main, ...)
  }
  graphics::rasterImage(z, bb[1], bb[3], bb[2], bb[4], interpolate=interpolate, ...)

  if (!is.null(addfun)) {
    if (is.function(addfun)) {
      addfun()
    }
  }

  if (sethook) {
    setHook("plot.new", function(...) {
      w <- getOption('warn')
      on.exit(options('warn' = w))
      options('warn'=-1)
      on.exit(graphics::par(old.par))
    }, 	action="replace")
    setHook("plot.new", function(...) setHook("plot.new", NULL, "replace"))
  }

}
