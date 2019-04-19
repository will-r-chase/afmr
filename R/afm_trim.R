#' Trim extreme values from afm maps
#'
#' This function will floor extreme values from an afm map. You can inspect the
#' distribution of values in an afm map with `afm_hist()` and then trim extreme
#' values either by quantile or specific cutoff. By default the cutoffs will be
#' set to the upper 97.5% and 2.5% quantiles. You can adjust these values, or
#' you can set the minimum and maximum values specifically using the `min` and
#' `max` arguments. The trimmed data will be added to your afm_scan object under
#' `maps` as eg. Height_Sensor_matrix_trimmed. The trimmed data should never be
#' used for calculation, only to improve image quality.
#'
#' @param data an "afm_scan" data object
#' @param matrix the name of the afm map you want to trim
#' @param lower the lower quantile (default 0.025)
#' @param upper the upper quantile (default 0.975)
#' @param min the minimum value cutoff (overrides quantile)
#' @param max the maximum value cutoff (overrides quantile)
#'
#' @return an "afm_scan" data object with the trimmed matrix added under `maps`
#' @export
#'
#' @examples
#' \donttest{load("data/half_um_scan.rda")
#' afm_trim(half_um_scan, matrix = "Height_Sensor_matrix")}
afm_trim <- function(data = NULL, matrix = NULL, lower = 0.025, upper = 0.975, min = NULL, max = NULL) {
  if(is.null(data)) {stop("data argument must be specified")}
  if(class(data) != "afm_scan") {stop("class of data must be 'afm_scan'")}
  if(!(matrix %in% names(data[["maps"]]))) {stop("matrix not found in data, check your spelling")}
  if(!is.numeric(lower)|!i.numeric(upper)) {stop("upper/lower must be numeric")}
  if((!is.null(min)|!is.null(max))&(!is.numeric(min)|!is.numeric(max))) {stop("min/max must be numeric")}

  my_dat <- data[["maps"]][[matrix]]
  new_name <- paste(matrix, "trim", sep = "_")

  if(is.null(min) | is.null(max)) {
    bot <- quantile(my_dat, c(lower, upper))[1]
    top <- quantile(my_dat, c(lower, upper))[2]
    my_dat[my_dat < bot] <- bot
    my_dat[my_dat > top] <- top
  } else {
    my_dat[my_dat < min] <- min
    my_dat[my_dat > max] <- max
  }

  data[["maps"]][[new_name]] <- my_dat
}

#' Plot histogram of afm map
#'
#' This is a wrapper to quickly plot a histogram of values in an afm map. To
#' make a customizable histogram with more options you should input your
#' scan_data to ggplot2 and design your own plot.
#'
#' @param data an "afm_scan" data object
#' @param channel the desired channel column name (eg. "Height_Sensor(nm)")
#'
#' @return a histogram of values for the selected channel
#' @export
#'
#' @examples
#' \donttest{load("data/half_um_scan.rda")
#' afm_hist(half_um_scan, channel = "Height_Sensor(nm)")}
afm_hist <- function(data = NULL, channel = NULL) {
  if(is.null(data)) {stop("data argument must be specified")}
  if(class(data) != "afm_scan") {stop("class of data must be 'afm_scan'")}
  if(!(channel %in% names(data[["scan_data"]]))) {stop("channel not found in data, check your spelling")}

  dat <- data[["scan_data"]]
  chan <- paste0("`", channel, "`")
  dat %>%
    ggplot2::ggplot(aes_string(x = chan)) +
    ggplot2::geom_histogram(color = "#1e7b66", fill = "#41b79c", alpha = 0.8) +
    ggplot2::theme_light()
}
