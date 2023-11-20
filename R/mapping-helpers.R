# interpolation ------------------------------------------------------------------

#' col2opacity
#'
#' Designed to interpolate numerics to opacities for a map or other visualization.
#'
#' @param x numeric vector to interpolate opacities for.
#' @param n_breaks number of breaks cut values into (before using floor)
#' @param opacity_range Bounds opacities between these values. Acts as floor/ceiling
#'   as opposed to range to interpolate along.
#'
#' @return numeric vector that can be used to represent other values through opacity.
#'
#' @export col2opacity
col2opacity <- function(x, n_breaks = 5, opacity_range = c(.35, .95), ...) {

  # bin input vector
  binned_x <- bin.var_format(x, n_breaks = n_breaks, use_labels = F)

  # convert bins to numeric factor levels, scale to maximum
  alphas <-
    as.numeric(binned_x) /
    max(as.numeric(binned_x), na.rm = T)

  # apply opacity floor/ceiling
  alphas <- case_when( alphas > opacity_range[2] ~ opacity_range[2],
                       alphas < opacity_range[1] ~ opacity_range[1],
                       TRUE ~ alphas )

  return(alphas)
}


# cropping & static zooms ------------------------------------------------------

#' cntr2bbx
#'
#' Simple wrapper that converts a centroid to a bbox. Useful for creating ggplots at
#' various zoom levels, or cropping to an area of interest based on a focal point or
#' centroid.
#'
#' @param cntr centroid
#' @param buffer buffer radius around centroid in km
#'
#' @export cntr2bbx
cntr2bbx <- function(cntr, buffer, crs) {
  require(sf)
  cntr <- st_transform(cntr,  "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")

  st_buffer(cntr,
            buffer * 1e3) %>%
    st_transform(crs) %>%
    st_bbox()
}



# gglayer convenience ----------------------------------------------------------


#' add.map.layers
#'
#' Downloads map layers such as water and county lines and adds to a ggplot
#'
#' @param sfx sf object providing basis for map
#' @param p existing ggplot to add layers to. Blank ggplot is default
#' @param add.water,add.counties add water areas/counties lines. Specify NULL if you
#'   want to leave off this layer, otherwise specify a color.
#' @param lwd line width, to apply to linear features (places and counties)
#' @param spatial.trim Spatial trim method. Intersection or crop suggested.
#'
#' @export add.map.layers
add.map.layers <- function(sfx,
                           add.water = "#94bdff",
                           add.counties = "#666666",
                           add.places = "black",
                           lwd = .5,
                           spatial.trim = c(st_intersection,
                                            st_crop),
                           ...) {

  require(tidyverse)
  options(tigris_use_cache = TRUE)

  if(is.list(spatial.trim))
    spatial.trim <- spatial.trim[[1]]

  sfx <- st_union(sfx)
  .cos <- county.subset(sfx, ...)

  .cos <- st_boundary(.cos) %>%
    spatial.trim(sfx)

  lyrs <- list()

  if(!is.null(add.counties)) {
    lyrs$counties <-
      geom_sf(data = .cos,
              color = add.counties,
              size = lwd)
  }

  if(!is.null(add.water)) {
    .wtr <- visaux::water.wrapper(.cos$geoid, sfx, ...) %>%
      spatial.trim(sfx)

    lyrs$water <-
      geom_sf(data = .wtr,
              fill = add.water,
              color = NA)
  }


  if(!is.null(add.places)) {
    .plcs <- places.wrapper(.cos$geoid, sfx, ...)  %>%
      spatial.trim(sfx)

    lyrs$places <-
      geom_sf(data = .plcs,
              color = add.places,
              fill = NA,
              size = lwd)
  }
  return(lyrs)
}






# map/viz templates ------------------------------------------------------------

#' dot.map.template
#'
#' @param dots an sf object long by a group column
#' @param bbox optional bbx to crop/transform to
#' @param group.col group column to map by
#'
#' @export dot.map.template
dot.map.template <- function(dots, bbx = NULL, group.col = "group"
                             , size = 1, shape = 20) {

  if(!is.null(bbx))
    dots <- dots %>% transform.and.crop(bbx)

  # dot geom
  ggdot <-
    geom_sf(data= dots,
            aes(color = !!rlang::sym(group.col)),
            size = size
            ,shape = shape)

  # other plot elements
  p.elems <-
    list(
      scale_colour_brewer(palette = "Accent" #"Set1"
                          ,name = "")
      , theme_void()
      , theme(legend.position = "bottom",
              legend.margin = margin(t = -0.8, unit='cm'),
              legend.text = element_text(size = 10)
      )
      , guides(size = F,
               color = guide_legend(override.aes = list(size=5))
      )
    )

  c(ggdot,
    p.elems)

}



# setup leaflet tiles for ggplot bkg -------------------------------------------

#' get.stamen.bkg
#'
#' Gets stamen base tiles based on supplied bbox of `sf` object.
#'
#' Unfortunately, ggmap has some idiosyncracies with geom_sf. If combining these into
#' single plot, remember:
#'
#' 1) include `inherit.aes = F` in geom_sf layer
#'
#' 2) Match sf crs to that of stamen tiles. Should be epsg = 4326 (but looks like
#' google maps uses different)
#'
#' @param sfx bbox or sf object to get background for
#' @param maptype passed onto `ggmap::get_stamenmap`. 'toner-background' and
#'   'toner-lines' are good options for basic black/white background maps with major
#'   features. 'Lines' seems to emphasize small streets more and has less black on
#'   it. 'Watercolor' is pretty but very colorful and bad for chloropleths. Could be
#'   interesting for some other types of visuals.
#'
#' @export get.stamen.bkg
get.stamen.bkg <- function(sfx
                           ,maptype = c('toner-background'
                                        ,'toner-lines'
                                        ,'watercolor')
                           ,zoom = 10
                           , ...) {

  # load ggmap
  require(ggmap)

  maptype <- maptype[1]

  # turn sf to bbox if needed
  if(! 'bbox' %in% class(sfx)) {
    # to longlat
    sfx <- sfx %>% st_transform(4326)
    # to bbox
    sfx <- sfx %>% st_bbox()
  }

  sttm <- ggmap::get_stadiamap(
    bbox = c(left = sfx[['xmin']],
             bottom = sfx[['ymin']],
             right = sfx[['xmax']],
             top = sfx[['ymax']])
    ,zoom = zoom
    ,maptype = maptype
    ,...
  )

  return(sttm)
}


#' bbox2ggcrop
#'
#' Wraps `coord_sf`, which can be used to crop a map made in ggplot. Uses an sf object
#' or bbox to set crop area.
#'
#' @param sfx bbox or sf object
#' @param clip passed onto `ggplot2::coord_sf`
#'
#' @export bbox2ggcrop
bbox2ggcrop <- function(sfx, crs = 4326, clip = 'on') {

  # turn sf to bbox if needed
  if(! 'bbox' %in% class(sfx))
    sfx <- sfx %>% st_transform(crs) %>% st_bbox()


  # return coord_sf
  ggplot2::coord_sf( xlim = c(sfx[['xmin']], sfx['xmax'])
                     ,ylim = c(sfx[['ymin']], sfx['ymax'])
                     ,clip = clip)
}


# palettes ----------------------------------------------------------------

#' n.categories.discrete.palette
#'
#' It's possible to run out of colors when visualizing with a large number of
#' discrete categories. This wraps the steps to interpolote a palette across
#' your number of categories, so variations of the colors are re-used. Helpful
#'
#' It returns a ggplot2 layer, by default for a `fill` aesthetic
#'
#' @examples
#' library(ggplot2)
#' x <- factor(rnorm(30))
#' scale_fill_manual(values = n.categories.discrete.palette(length(x)))
#'
#' @export n.categories.discrete.palette
n.categories.discrete.palette <- function(
    ncats
    ,base.pal = RColorBrewer::brewer.pal(6, 'Dark2')
    ) {

  sample( colorRampPalette(base.pal)(ncats) )

}


# conversions -------------------------------------------------------------

#' sqm2acre
#'
#' Conversion factor for square meters to acres
#'
#' @export sqm2acre
sqm2acre <- function() {
  sqm2acre <- 1e6 / 247.105
}


# areas from bbx ---------------------------------------------------------------

# when I'm interested in creating a plot and limiting bounds using coord_sf, or
# otherwise basing plot on a bbox rather than vector barriers.

