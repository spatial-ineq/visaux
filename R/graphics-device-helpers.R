#' ragg.wrapper
#'
#' Wraps `ragg::agg_png` to save a plot as .png. An alternative to ggsave. See
#' https://www.tidyverse.org/blog/2019/07/ragg-0-1-0/ for pkg intro.
#'
#' @param fn filename to save to
#'
#' @export ragg.wrapper
ragg.wrapper <- function(fn
                         ,plot = ggplot2::last_plot()
                         ,sv.dir = 'visuals/', ...) {

  if(!dir.exists(sv.dir))
    dir.create(sv.dir)

  path <- paste0(sv.dir
                 ,fn, '.png')

  ragg::agg_png(path
                , width = 10
                , height = 8
                , res = 188
                , units = 'in')
  print(plot)
  invisible(dev.off())
}
