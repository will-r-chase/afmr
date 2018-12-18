#' Make image from afm map
#'
#' @param data an "afm_scan" data object
#' @param matrix the name of the matrix you want to plot as an image (eg. "Height_Sensor_matrix")
#' @param color the colorscale (see details for options)
#' @param normalized whether the data is normalized to a (0,1) scale
#'
#' @details When specifying the afm map that you want to plot as an image, you must use
#' the exact name that appears in data$maps, ie. "Height_Sensor_matrix". By default the data will
#' be plotted in greyscale, but there are several options for colors: "nanoscope" is a dupe of the yellow-tan
#' color palette in the nanoscope software; "distinct_rainbow" and "smooth_rainbow" are rainbow palettes with
#' 10 or 256 levels respectively; all palettes from the viridis line are available, eg. "viridis", "magma", etc.
#' By default the data is normalized to a 0-1 scale, this is because EBImage requires data to be scaled to properly
#' apply colors and display the image. If you set this as FALSE there is a good chance your image will be displayed as junk.
#' This function rotates the EBImage output by -90 degrees because the way that EBImage plots things is weird. The output rotation
#' matches the orientation of the scan direction, such that the image appears as it would in the nanoscope software.
#'
#'
#' @return an EBImage image object. This image can be viewed using display()
#' @export
#'
#' @examples
#' \donttest{load("data/two_um_scan.rda")
#' img <- afm_img(two_um_scan, matrix = "Height_Sensor_matrix", color = "nanoscope")
#' EBImage::display(img, method = "raster")
#' }
afm_img <- function(data = NULL, matrix = NULL, color = "none", normalized = TRUE) {
  if(is.null(data)) {stop("data argument must be specified")}
  if(class(data) != "afm_scan") {stop("class of data must be 'afm_scan'")}
  if(!(matrix %in% names(data[["maps"]]))) {stop("matrix not found in data, check your spelling")}

  nanoscope_colors <- c("#000000", "#000000", "#0A0000", "#140000", "#1E0000", "#280000", "#320000", "#3C0000", "#460000", "#500000", "#5A0400", "#641500", "#6E2300", "#783400", "#824300", "#8C5100", "#966100", "#A07102", "#AA801B", "#B49037", "#BE9F52", "#C8AD6C", "#D2BD87", "#DCCCA3", "#E6DCBF", "#F0ECDB", "#FAFAF5", "#FFFFFF", "#FFFFFF")
  nanoscope_pal <- colorRampPalette(nanoscope_colors)

  pal <- switch(
    color,
    "nanoscope" = nanoscope_pal(256),
    "distinct_rainbow" = rainbow(10),
    "smooth_rainbow" = rainbow(256),
    "viridis" = viridis::viridis(256),
    "magma" = viridis::magma(256),
    "plasma" = viridis::plasma(256),
    "inferno" = viridis::inferno(256),
    "none" = grey.colors(10)
  )

  my_dat <- data[["maps"]][[matrix]]

  if(normalized == TRUE) {
    normalized <- EBImage::normalize(my_dat, 1)
    img <- EBImage::Image(normalized)
  } else {
    img <- EBImage::Image(my_dat)
  }

  img_rot <- EBImage::rotate(img, -90)

  if(color == "none") {
    img_rot
  } else {
  img_col <- EBImage::colormap(img_rot, pal)
  }
}

#add functions for filter, equalize, recolor, overlay, crop
#reexport display()
#make pipable
# matrix %>%
#   afm_img() %>%
#   recolor %>%
#   equalize() %>%
#   crop() %>%
#   display()
