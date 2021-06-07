#' create_leaflet_base
#'
#' Creates base leaflet and sets options for app. Configures layers so that text
#' labels on map will appear above data added to base tiles, but the base tiles for
#' landscape and water appear below.
#'
#' See discussion at:
#' https://leafletjs.com/examples/map-panes/ or
#' https://stackoverflow.com/questions/54667968/controlling-the-z-index-of-a-leaflet-heatmap-in-r/54676391
#' for addMapPanel/zIndex details
#'
#' @export create_leaflet_base
create_leaflet_base <- function() {

  require(leaflet)

  leaflet(options = leafletOptions()) %>%
    addMapPane("gs", zIndex = 400) %>%
    addMapPane("div", zIndex = 430) %>%
    addMapPane("tileLabels", zIndex = 599) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels,
                     options = providerTileOptions(noWrap = TRUE,
                                                   pane = "tilePane")
    ) %>%
    addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                     options = providerTileOptions(noWrap = TRUE,
                                                   pane = "tileLabels")
    )
}


# leaflet helpers -------------------------------------------------------

#' bbox_to_lnglat
#'
#' Leaflet fcns like `leaflet::flyToBounds` and `leaflet::setMaxBounds` require bounds in
#' a format slightly different than that returned from sf::st_bbox. This
#' helper turns output of `sf::st_bbox` to an input for a leaflet zoom function. Can add a
#' small pad around the bbox as well.
#'
#' @param st_bbox output
#' @param padding padding to put around bbox. Units set by map crs
#'
#' @export bbox_to_lnglat
bbox_to_lnglat <- function(bbox, padding = 1) {

  bbox <- as.list(bbox)

  list(lng1 = bbox$xmax + padding,
       lng2 = bbox$xmin - padding,
       lat1 = bbox$ymax + padding,
       lat2 = bbox$ymin - padding)
}


#' zoom_to_bounds
#'
#' Flies to new boundary area
#'
#' @param x sf object to zoom to on map. (Or one with equivalent bounds)
#' @param mapID ID for leaflet map
#'
#' @return Nothing, but references a leaflet proxy to update view bounds.
#'
#' @export
zoom_to_bounds <- function( x, mapID="map" ) {
  bbox <- sf::st_bbox(x) %>% as.list()

  leaflet::leafletProxy(mapID) %>%
    leaflet::flyToBounds()
}




# functions to update leafletProxy ----------------------------------

#' i think these two should go away. Learning process!

#' iterative_leaflet_draw
#'
#' clear data, get number of iterations, draw first one. Return 0 if there's
#' only one, otherwise loop through the rest. Idea is that it can smooth out a
#' render to make loading feel nicer. Passes tooltips and other graphic parameters
#' to \code{leaflet_draw}.
#'
#' @inheritDotParams leaflet_draw
#' @param bkdwn_size Number of rows of sf object to draw at a time.
#'
#' @export iterative_leaflet_draw
iterative_leaflet_draw <- function(leaflet.proxy, st_df, tooltips, ..., bkdwn_size = 50) {

  require(leaflet)

  leaflet.proxy %>% clearGroup("dat")

  n_iterations <- ceiling(nrow(st_df) / bkdwn_size )
  leaflet_draw(leaflet.proxy,
               st_df[1:bkdwn_size,]
               ,tooltips = tooltips[1:bkdwn_size]
               , ...)

  if(n_iterations == 1) return()
  for ( i in 1:(n_iterations - 1)) {
    step <- i * bkdwn_size
    subset_index <- (step + 1):(step + bkdwn_size)

    leaflet_draw(leaflet.proxy,
                 st_df[subset_index,]
                 ,tooltips = tooltips[subset_index]
                 , ...)
  }
}

#' leaflet_draw
#'
#' Wraps leaflet call to create a choropleth from sf object.
#'
#' @param leaflet.proxy Leaflet proxy object
#' @param st_df sf object to map
#' @param tooltips A list of strings with html
#' @param pal A function to create color palette from; i.e., a all to
#'   leaflet::colorFactor.
#' @param opacity Opacity from 0-100
#' @param var.name Title of column to color map based on, possibly created with
#'   bin.var_format or bin_from_breaks.
#'
#' @export leaflet_draw
leaflet_draw <- function(leaflet.proxy,
                         st_df, tooltips, pal, opacity = 60,
                         var.name = "binned_x", outlines = NULL,
                         width = 0.5, popup_btns = NULL) {
  require(leaflet)

  # drop extras if we're at last loop of iteration.
  st_df <- st_df[!st_is_empty(st_df$geometry), ]
  tooltips <- lapply(tooltips[!is.na(tooltips)], shiny::HTML)
  # pull supplied column as vector
  vv <- pull(st_df, !!rlang::sym(var.name))

  # interpolate colors
  if(is.null(outlines))
    outlines <- vv   # colorspace::darken(pal(vv), .2)

  leaflet.proxy %>%
    addPolygons(group = "dat",
                data = st_df,
                fillColor = pal(vv),
                fillOpacity = opacity * 0.01,
                label = tooltips,
                color = outlines,
                weight = width,
                popup = popup_btns,
                stroke = TRUE
                ,highlightOptions = highlightOptions(fillColor = "#EE77A0"
                                                     ,weight=2
                                                     ,fillOpacity = 0.6
                                                     ,bringToFront = TRUE))
}



