#' ts.labels.geom
#'
#' Returns a ggplot2 geometry that anticipates labeling lines of a multi-group
#' time series plot. Labels will appear at end of the plot.
#'
#' @param tsx time series data
#' @param group.col column indicating group. Each will be labelled.
#' @param date.col date or time column for x axis.
#'
#'
#' @export ts.labels.geom
ts.labels.geom <- function(tsx,
                           group.col,
                           date.col = 'date'
                           ,size = 3
                           ,alpha = .6
                           ,color = 'black') {

  require(tidyverse)
  require(ggrepel)

  .date <- rlang::sym(date.col)
  .group <- rlang::sym(group.col)

  ts.lbls <- tsx %>%
    filter(!!.date == max(!!.date))

  lbls <- ggrepel::geom_label_repel( data = ts.lbls
                                     ,aes(label = !!.group
                                          ,fill = !!.group
                                     )
                                     ,color = color
                                     ,size = size
                                     ,alpha = alpha
  )
  return(lbls)
}
