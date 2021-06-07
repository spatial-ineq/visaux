#' functions that interface with the `tigris` library to query layers.
#'
#' The flow is: given a spatial layer, can query counties with county_subset, a thin
#' wrapper. Then other thin wrappers will call


#' flexible.spatial.filter
#'
#' Helper fcn that guesses appropriate spatial filter for subsetting polygons that
#' overlap with another sf object `x`, based on x's geometry type. If `x` contains
#' points, other polygon layer is subsetted with `st_intersects`; if `x` is polygons,
#' the other polygon is converted to points with `st_points_on_surface` first.
#'
#' Currently doesn't work with non-coterminous polys, but may add that later.
#'
#' @param subset.approach spatial filter approach; one of "intersects" or "crop". Intersection or cropping to bbox.
#'
flexible.spatial.filter <- function(x, polys,
                                    subset.approach = c("intersects", "crop"), ...) {

  require(sf)

  polys <- st_transform(polys, st_crs(x))

  # use geometry type to determine spatial filter, if none supplied
  if(subset.approach[1] == "intersects") {
    .geo.type <- st_geometry_type(x$geometry) %>% as.character()

    if(any(grepl("POINT", .geo.type)))
      sbgp <- st_intersects(polys,
                            x )
    else
      sbgp <- st_intersects(st_point_on_surface(polys),
                            x )

    polys <- polys[lengths(sbgp) > 0, ]
  } else if(subset.approach[1] == "crop") {
    polys <- st_crop(polys, x)
  }
  return(polys)

}

#' county.subset
#'
#' Given an `sf` object, get all counties that intersect. The spatial filter will
#' adapt based on input, but can be thrown off when `x` isn't coterminous with
#' counties. In this case the function `xwalks::get.spatial.overlap` can be helpful.
#'
#' @param x `sf` object
#' @param cos counties sf object. If none supplied, they are downloaded using
#'   `tigris` library
#' @inheritDotParams flexible.spatial.filter
#' @param ... other arguments passed onto `tigris::counties`
#'
#' @export county.subset
county.subset <- function(x, cos = NULL, ...) {

  if(is.null(cos))
    cos <- tigris::counties(...)

  x <- st_sf(x)

  cos <- flexible.spatial.filter(x, cos, ...)

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
#' @param x object to derive overlapping statefps from, if statefps is left as null. Aiddtional
#' @param statefps statefps to get parks for
#'
#' @export parks.wrapper
parks.wrapper <- function(x = NULL, statefps = NULL, ...) {

  if(!is.null(statefps)) {
    cos <- county.subset(x, ...)
    statefps <- cos$statefp
  }

  parks <- map_dfr(statefps,
                   ~tigris::landmarks(.x, type = "area")
  )

  colnames(parks) <- tolower(colnames(parks))

  parks <- parks[grepl('Park|Cmtry', parks$fullname),]

  parks <- st_crop(x, parks)

  return(parks)
}
