#' These functions use non-tigris queries that might be more specialized, but I like
#' for my personal convenience.

#' get.NHPN
#'
#' Get National Highway Planning Network highway data.
#'
#' @param sfx if not NULL, an `sf` object to use to crop hwy data
#' @param dropbox.dir,subdir concatenated to get to nhpn data
#'
#' @export get.NHPN
get.NHPN <- function(sfx = NULL,
                     dropbox.dir = Sys.getenv("drop_dir"),
                     subdir = "shapefiles/nhpn/") {
  require(sf)
  require(dplyr)

  nhpn.dir <-
    paste0(dropbox.dir,
           subdir)
  if(!is.null(sfx)) {
    wkt.str <- sfx %>%
      st_union() %>%
      st_as_text()

    hwys <- nhpn.dir %>%
      list.files(pattern = "shp$",
                 full.names = T) %>%
      st_read(wkt_filter = wkt.str)
  } else {
    hwys <- nhpn.dir %>%
      list.files(pattern = "shp$",
                 full.names = T) %>%
      st_read()
  }
  colnames(hwys) <- tolower(colnames(hwys))

  return(hwys)
}
