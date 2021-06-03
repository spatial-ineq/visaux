# viz helpers ------------------------------------------------------------------

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
col2opacity <- function(x, n_breaks = 5, opacity_range = c(.35, .95)) {

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

