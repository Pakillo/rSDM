% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/occmap.R
\name{occmap}
\alias{occmap}
\title{Map species occurrences}
\usage{
occmap(
  locs,
  ras = NULL,
  bg = "Esri.WorldImagery",
  type = c("base", "ggplot", "leaflet"),
  pcol = "red",
  alpha = 1,
  psize = 1,
  add = FALSE,
  prev.map = NULL,
  ...
)
}
\arguments{
\item{locs}{An \code{\link[sf:sf]{sf::sf()}} or \code{\link[terra:SpatVector-class]{terra::SpatVector()}} object with point coordinates,
e.g. as generated from \code{\link[=locs2sf]{locs2sf()}} or \code{\link[=locs2vect]{locs2vect()}}.}

\item{ras}{A \code{\link[terra:SpatRaster-class]{terra::SpatRaster()}} object to be used as background for points.
If NULL (default), a background map defined by \code{bg} will be used.}

\item{bg}{Character. Type of background map to be used if \code{ras} is not provided.
\code{bg} should be one of the providers listed in \code{\link[maptiles:get_tiles]{maptiles::get_tiles()}} if
\code{type} is 'base' or 'ggplot', or one of the providers listed in
\code{\link[leaflet:addProviderTiles]{leaflet::addProviderTiles()}} if \code{type} is 'leaflet'.}

\item{type}{Character. One of "base", "ggplot" or "leaflet" to define the type
of map produced.}

\item{pcol}{Colour to be used for points. Default is "red".}

\item{alpha}{Colour transparency for points, between 0 (fully transparent)
and 1 (fully opaque).}

\item{psize}{Point size. Default is 1 (cex = 1).}

\item{add}{Logical. Add \code{locs} coordinates to a previous 'base' map? (e.g. for a new species).}

\item{prev.map}{Map to be used as basemap to add further points
(only applicable for "leaflet" and "ggplot" map types).}

\item{...}{additional parameters to be passed to
\code{\link[terra:plot]{terra::plot()}} if \code{type = "base"}
\code{\link[tidyterra:geom_spatraster]{tidyterra::geom_spatraster()}} if \code{type = "ggplot"}
or \code{\link[leaflet:map-layers]{leaflet::addCircleMarkers()}} if \code{type = "leaflet"}.}
}
\value{
A map plus a leaflet or ggplot object, depending on \code{type}.
}
\description{
Plot map of species occurrences (or any set of point coordinates) on top of different background layers.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
## Example coordinates
locs <- data.frame(lon = c(-5.8, -5.5, -5.9), lat = c(35.7, 36.2, 36.5))
locs <- locs2sf(locs, crs = 4326)

## Default map
occmap(locs, psize = 6)
occmap(locs, psize = 6, bg = "CartoDB.Positron") # Change background

## Interactive (leaflet) map
occmap(locs, psize = 6, type = "leaflet")
occmap(locs, psize = 6, type = "leaflet", bg = "CartoDB.Positron")

## ggplot map
occmap(locs, psize = 6, type = "ggplot")

## Adding points to a previous map
new.locs <- data.frame(lon = c(-5.8, -5.4), lat = c(36.2, 36.5))
new.locs.sf <- locs2sf(new.locs, crs = 4326)

## base
map <- occmap(locs, psize = 6)
occmap(new.locs.sf, add = TRUE, psize = 6, pcol = "blue")

## Adding points to a previous map (leaflet)
map <- occmap(locs, psize = 6, type = "leaflet")
occmap(new.locs.sf, prev.map = map, psize = 6, pcol = "blue")

## Adding points to a previous map (ggplot)
map <- occmap(locs, psize = 6, type = "ggplot")
occmap(new.locs.sf, prev.map = map, psize = 6, pcol = "blue")
\dontshow{\}) # examplesIf}
}
