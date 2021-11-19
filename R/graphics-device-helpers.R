#' ragg.wrapper
#'
#' Wraps `ragg::agg_png` to save a plot as .png. An alternative to ggsave. See
#' https://www.tidyverse.org/blog/2019/07/ragg-0-1-0/ for pkg intro.
#'
#' @param fn filename to save to. If NULL, will just saved to i.e., `-1.png`,
#'   incrementing if other plots exist
#' @param plot to save
#' @param sv.dir save directory. Defaults to `visuals/` subdirectory w/in project
#'   dir.
#' @param width,height,res,units passed onto `ragg::agg_png`
#'
#' @export ragg.wrapper
ragg.wrapper <- function(fn = NULL
                         ,plot = ggplot2::last_plot()
                         ,sv.dir = 'visuals/'
                         , width = 10
                         , height = 8
                         , res = 188
                         , units = 'in'
                         ) {

  if(!dir.exists(sv.dir))
    dir.create(sv.dir)

  # gen filename if NULL
  if(is.null(fn)) {

    extant.defaults <- list.files(sv.dir
                                  , pattern = '-[0-9]*\\.png')

    nm <- stringr::str_extract(extant.defaults
                               ,'[0-9]*')
    if(length(nm) == 0)
      nm <- 0

    nm <- max(as.numeric(nm)) + 1

    fn <- glue::glue('-{nm}.png')
  }

  path <- paste0(sv.dir
                 ,fn, '.png')

  ragg::agg_png(path
                ,width = width
                ,height = height
                ,res = res
                ,units = units)
  print(plot)
  invisible(dev.off())
}




#' ggsave.hirez
#'
#' Wraps ggsave with some defaults I'm finding sensible. I think this is inferior to
#' `ragg.wrapper`
#'
#' @param dir,fn directory and filename to save to
#'
#' @export ggsave.hirez
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
