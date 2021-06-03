

#' q.format
#' 
#' Quick format. Quick helper function to format numerics for readability.
#' 
#' @export
q.format <- function(x, digits = 1, perc = F) {
  out <- 
    formatC(x, big.mark = ",", digits = digits, format = "f", drop0trailing = TRUE) %>%
    trimws() 
  if(perc)
    out <- paste0(out,"%")
  return(out)
}

#' format_as.numeric
#' 
#' Turns a formatted number string back into a numeric
#' 
#' @export
format_as.numeric <- function(x) {
  as.numeric(
    gsub(",|%|\\$", "", x)
  )
}


#' get_breaks
#'
#' this function bins a seqential variable based on value ranges. It can often
#' make visualisations easier to look at by allowing more striking differences
#' and keeping extremes from dominating the color scale. the highlight_top
#' parameter ensures a separate bin for top 1%. This is called from
#' bin.var_format.
#' 
#' @inheritParams bin.var_format
#' 
#' @export
get_breaks <- function(x, n_breaks = 6, highlight_top_percent = F, ...) {
  
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
#' @inheritParams bin.var_format
#' @export
bin_from_breaks <- function(x, breaks, use_labels = TRUE, digits = 2, ...) {
  
  if (length(breaks) == 1) 
    return(x)
  
  if( use_labels ) {
    bin.labels <- 
      purrr::map_chr(1:(length(breaks) - 1),
                     ~paste0(q.format(breaks[.], digits = digits), " - ",
                             q.format(breaks[. + 1], digits = digits)))
  } else
    bin.labels <- NULL
  
  cut(x, breaks = breaks, 
      labels = bin.labels,
      include.lowest = T)
}
#' bin.var_format
#'
#' Bins a sequential variable based on value ranges. It can often make visualizations
#' easier to look at by allowing more striking differences and keeping outliers from
#' dominating the color scale. Can return formatted values for leaflet legend if
#' \code{use_labels} is TRUE. the highlight_top parameter ensures a separate bin for
#' top 1% The function wraps \code{get_breaks} and \code{bin_from_breaks}, which can
#' also be used separately
#' @param x numeric vector
#' @param n_breaks target number of breaks for x
#' @param highlight_top_percent Whether to break out an additional bin for the top
#'   1\%. Helpful to break out upper outliers.
#' @param use_labels whether to create "x-y" type character labels for buckets rather
#'   than default labels from \code{cut}
#' @param digits Digits to pass on to \code{q.format} if \code{use_labels} is TRUE.
#' @export
bin.var_format <- function(x, ...) {
  breaks <- get_breaks(x, ...)
  bin_from_breaks(x, breaks, ...)
}



#' get_mean_from_interval
#'
#' Gets means from intervals in (xx,yy) or (xx,yy] form (for example, common
#' output of \code{cut} function); this transforms into numeric for the point histogram
#' @param interval interval in form (xx,yy) or (xx,yy]
#' @export
get_mean_from_interval <- function(interval) {
  mean_list <- stringr::str_extract_all(interval, "-?[e+0-9.]+") %>%
    purrr::map_dbl(~mean(as.numeric(.), na.rm = T))
  return(mean_list)
}
