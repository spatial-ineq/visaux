

# capping and binning -----------------------------------------------------

#' cap.at.quantile
#'
#' Caps values of a vector at a given percentile. Can be helpful for visualizing
#' data with long-tail distributions, especially when color is mapped to the
#' long tail.
#'
#' @param x values
#' @param ptile percentile at which to cap `x`
#'
#' @export cap.at.quantile
cap.at.quantile <- function(x, ptile = .95, na.rm = T) {

  dplyr::if_else(x >= quantile(x, probs = ptile
                               ,na.rm = na.rm)
                 ,quantile(x, probs = ptile
                           , na.rm = na.rm)
                 ,x
  )

}


#' get.quantile.breaks
#'
#' bins a sequential variable based on value ranges. It can often
#' make visualizations easier to look at by allowing more striking differences
#' and keeping extremes from dominating the color scale. the highlight_top
#' parameter ensures a separate bin for top 1%. Called from
#' quantile.bin_format.
#'
#' @inheritParams quantile.bin_format
#' @param n_breaks target number of breaks for x
#' @param highlight_top_percent Whether to break out an additional bin for the top
#'   1%. Helpful to break out upper outliers.
#'
get.quantile.breaks <- function(x, n_breaks = 6, highlight_top_percent = F, ...) {

  if (highlight_top_percent)
    probs <- c(seq(0, 0.99, 0.99/n_breaks), 1)
  else
    probs <- seq(0, 1, 1/n_breaks)

  breaks <- quantile(x, probs = probs, na.rm = T)
  breaks <- unique(breaks)
  return(breaks)
}

#' bin_from_breaks
#'
#' From a determined set of breakpoints, put data into bins
#'
#' @inheritParams quantile.bin_format
#' @param format_breaks whether to create "x-y" type character labels for buckets rather
#'   than default labels from `cut`
#' @param digits Digits to pass on to `q.format` if using formatting
#'
#' @export bin_from_breaks
bin_from_breaks <- function(x, breaks, format_breaks = TRUE, digits = 2, ...) {

  if (length(breaks) == 1)
    return(x)

  if( format_breaks )
    .labels <-
      purrr::map_chr(1:(length(breaks) - 1),
                     ~paste0(q.format(breaks[.], digits = digits), " - ",
                             q.format(breaks[. + 1], digits = digits)))
  else
    .labels <- NULL

  cut(x, breaks = breaks,
      labels = .labels,
      include.lowest = T)
}


#' quantile.bin_format
#'
#' Bins a sequential variable based on value ranges. It can often make visualizations
#' easier to look at by allowing more striking differences and keeping outliers from
#' dominating the color scale. Wraps `get_breaks` and `get.quantile.breaks`, which can
#' also be used separately
#'
#' @param x numeric vector
#' @inheritDotParams bin_from_breaks
#' @inheritDotParams get.quantile.breaks
#'
#' @export quantile.bin_format
quantile.bin_format <- function(x, ...) {
  breaks <- get.quantile.breaks(x, ...)
  bin_from_breaks(x, breaks, ...)
}



#' get_mean_from_interval
#'
#' Gets means from intervals in (xx,yy) or (xx,yy] form (for example, common
#' output of `cut` function); this transforms into numeric for the point histogram
#'
#' @param interval interval in form (xx,yy) or (xx,yy]
#'
#' @export get_mean_from_interval
get_mean_from_interval <- function(interval) {
  mean_list <- stringr::str_extract_all(interval, "-?[e+0-9.]+") %>%
    purrr::map_dbl(~mean(as.numeric(.), na.rm = T))
  return(mean_list)
}



# formating ---------------------------------------------------------------





#' q.format
#'
#' Quick format. Quick helper function to format numerics for readability.
#'
#' @export q.format
q.format <- function(x, digits = 1, perc = F) {
  out <-
    formatC(x, big.mark = ",", digits = digits, format = "f", drop0trailing = TRUE) %>%
    trimws()
  if(perc)
    out <- paste0(out,"%")
  return(out)
}

#' formated2numeric
#'
#' Turns a formatted number string back into a numeric
#'
#' @export
format_as.numeric <- function(x) {
  as.numeric(
    gsub(",|%|\\$", "", x)
  )
}





# string formating -------------------------------------------------------------


#' linebreaks2string
#'
#' Removes white space (if `clean.ws` is true), then adds linebreaks at uniform
#' intervals. Can deal with very long ggplot captions, for example
#'
#' @export linebreaks2string
linebreaks2string <- function(x
                              , line.char.len = 80
                              ,clean.ws = T
                              , line.break = '\n') {
  # first remove breaks
  x <- gsub('\n', ' ', x)

  if(clean.ws)
    x <- gsub('  +', ' ', x)

  # then add new ones at every x characters
  regex <- paste0('(.{'
                  ,line.char.len,
                  ',}?)\\s')

  x <- gsub(regex, "\\1\n", x)
  return(x)
}

