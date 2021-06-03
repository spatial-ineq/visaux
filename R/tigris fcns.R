#' functions that interface with the `tigris` library to query layers.
#'
#' The flow is: given a spatial layer, can query counties with county_subset, a thin
#' wrapper. Then other thin wrappers will call

#' county.subset
#'
#' Given an `sf` object, get all counties that intersect. The spatial filter will
#' adapt based on input, but can be thrown off when `x` isn't coterminous with
#' counties. In this case the function `xwalks::get.spatial.overlap` can be helpful.
#'
#' @param x `sf` object
#' @param cos counties sf object. If none supplied, they are downloaded using
#'   `tigris` library
#' @param spatial.filter spatial filter approach. Intersection or cropping to bbox.
#' @param ... passed onto `tigris::counties`
#'
#' @export county.subset
county.subset <- function(x, cos = NULL, spatial.filter = c("intersects", "crop"), ...) {

  if(is.null(cos))
    cos <- tigris::counties(...)

  x <- st_sf(x)
  cos <- cos %>% st_sf() %>% st_transform(st_crs(x))

  # use geometry type to determine spatial filter, if none supplied
  if(spatial.filter[1] == "intersects") {
    geo.type <- st_geometry_type(x$geometry) %>% as.character()
    if(grepl("POINT", geo.type))
      sbgp <- st_intersects(cos,
                            x )
    else
      sbgp <- st_intersects(st_point_on_surface(cos),
                            x )

    cos <- cos[lengths(sbgp) > 0, ]
  } else if(spatial.filter[1] == "crop") {
    cos <- st_crop(cos, x)
  }
  colnames(cos) <-
    tolower(colnames(cos))

  return(cos)
}


#' water.wrapper
#'
#' Gets water areas based on supplied countyfp codes and/or other spatial area.
#'
#' @param countyfps 5-character state/county fp codes. Retrieved using
#'   `county.subset` and supplied `x` argument if null.
#' @param x `sf` object to get overlapping water areas for. Passed onto
#'   `county.subset` if no county fps are supplied. Also used to spatially subset
#'   water areas.
#' @param size.min Minimum size in m^2, after internal boundaries are resolved (if a
#'   water area is represented by multiple contiguous polygons)
#'
#' @return water areas for region.
#'
#' @export water.wrapper
water.wrapper <- function(countyfps = NULL, x = NULL, size.min = 5e6, ...) {

  if(is.null(countyfps)) {
    cos <- county.subset(x, ...)
    countyfps <- cos$geoid
  }

  # download water
  water <- map_dfr(countyfps,
                   ~tigris::area_water(state =
                                         substr(., 1, 2),
                                       county =
                                         substr(., 3, 5))
  )

  # union and explode water
  water <- st_union(water) %>% st_cast("POLYGON") %>% st_sf()

  # filter by size of union'd body
  water <- water %>% filter(as.numeric(st_area(.$geometry)) > size.min )

  if(!is.null(x)) {
    water <- st_transform(water, st_crs(x))
    water <- st_crop(water, x)
  }

  return(water)
}


#' parks.wrapper
#'
#'
parks.wrapper <- function(x, statefps = NULL, ...) {

  if(statefps(cos)) {
    cos <- county.subset(x, ...)
    statefps <- cos$statefp
  }

  parks <- map_dfr(statefps,
                   ~tigris::landmarks(type = "area")
  )
  parks <- parks[grepl('Park|Cmtry', parks$FULLNAME),]
  return(parks)
}
