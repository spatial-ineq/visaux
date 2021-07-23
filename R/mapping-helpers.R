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
#' @param spatial.trim Spatial trim method. Intersection or crop suggested.
#'
#' @export add.map.layers
add.map.layers <- function(sfx,
                           add.water = "#94bdff",
                           add.counties = "#666666",
                           add.places = "black",
                           spatial.trim = c(st_intersection,
                                            st_crop),
                           ...) {
  require(tidyverse)

  options(tigris_use_cache = TRUE)
  if(is.list(spatial.trim))
    spatial.trim <- spatial.trim[[1]]

  .cos <- county.subset(sfx, ...)
  .cos <- st_crop(.cos, sfx)
  .cos <- st_boundary(.cos) %>%
    spatial.trim(sfx)

  lyrs <- list()

  if(!is.null(add.counties)) {
    lyrs$counties <-
      geom_sf(data = .cos,
              color = add.counties,
              size = .7)
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
              size = .7)
  }
  return(lyrs)
}



#' ggsave.hirez
#'
#' Wraps ggsave with some defaults I'm finding sensible.
#'
#' @param dir,fn directory and filename to save to
#'
#' @export
ggsave.hirez <- function(plot,
                         dir, fn,
                         ext = "png",
                         height = 7.5,
                         units = "in",
                         dpi = 340,
                         ...) {

  require(ggplot2)
  width <- height * 1.228

  ggsave(
    filename = paste0(dir, fn, ".", ext),
    plot = plot,
    height = height,
    width = width,
    units = units,
    dpi = dpi,
    ...
  )
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

