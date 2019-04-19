#' Rayshade an AFM height map
#'
#' This function wraps rayshader to produce a nice hillshade of an afm height map.
#' The output is just a hillshade and will need to be piped to afm_plot_3d() to view the map.
#'
#' @param data an "afm_scan" data object
#' @param matrix the afm map to hillshade, should be a height map
#' @param texture the texture to pass to rayshader (see ?rayshader::sphere_shade)
#' @param light_angle the angle of the light for the hillshade
#' @param zscale the zscale, should be set to "auto" for an accuate z scale based on the scan size
#' @param max_darken the lower limit for how much the image will be darkened. 0 is completely black, 1 is no shadow.
#'
#' @return a hillshade of the afm map
#' @export
#'
#' @examples
#' \donttest{
#' load("half_um_scan.rda")
#'
#' }
afm_rayshade <- function(data = NULL, matrix = "Height_Sensor_matrix", texture = "desert", light_angle = 315, zscale = "auto", max_darken = 0.7) {
  if(is.null(data)) {stop("data argument must be specified")}
  if(class(data) != "afm_scan") {stop("class of data must be 'afm_scan'")}
  if(!(matrix %in% names(data[["maps"]]))) {stop("matrix not found in data, check your spelling")}

  if(is.numeric(zscale)) {
    z <- zscale
  } else if(zscale == "auto") {
    zscale <- (data[["params"]][["scan_size"]])/(data[["params"]][["afm_lines"]])
  }

  elmat <- data[["maps"]][[matrix]]

  elmat %>%
    rayshader::sphere_shade(texture = texture) %>%
    rayshader::add_shadow(rayshader::ray_shade(elmat, zscale = zscale, maxsearch = 100, sunangle = light_angle), max_darken = max_darken) %>%
    rayshader::add_shadow(rayshader::ambient_shade(elmat, zscale = zscale, maxsearch = 20), max_darken = max_darken)

}


#' Title
#'
#' @param hillshade
#' @param data
#' @param matrix
#' @param color
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
afm_overlay <- function(hillshade = NULL, data = NULL, matrix = NULL, color = "smooth_rainbow", alpha = 0.7) {
  if(is.null(data)) {stop("data argument must be specified")}
  if(is.null(hillshade)) {stop("hillshade must be passed to afm_overlay")}
  if(is.null(matrix)) {stop("must specify a matrix to overlay")}
  if(class(data) != "afm_scan") {stop("class of data must be 'afm_scan'")}
  if(!(matrix %in% names(data[["maps"]]))) {stop("matrix not found in data, check your spelling")}
  if(!is.numeric(alpha)) {stop("alpha must be numeric between 0 and 0.99")}
  if(alpha < 0 | alpha > 0.99) {stop("alpha must be numeric between 0 and 0.99")}

  overlay <- afm_img(data = data, matrix = matrix, color = color, normalized = TRUE) %>%
    EBImage::rotate(90)

  EBImage::writeImage(overlay, files = "overlay_tmp.png", type = "png")
  overlay_png <- png::readPNG("overlay_tmp.png")
  file.remove("overlay_tmp.png")

  alpha_layer <- matrix(1, nrow = dim(overlay_png)[1], ncol = dim(overlay_png)[2])
  overlay_png <- abind::abind(overlay_png, alpha_layer)
  overlay_png[,,4] <- alpha

  hillshade %>%
    rayshader::add_overlay(overlay_png)
}


#' Title
#'
#' @param hillshade
#' @param data
#' @param matrix
#' @param zscale
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
afm_plot_3d <- function(hillshade = NULL, data = NULL, matrix = "Height_Sensor_matrix", zscale = "auto", ...) {
  if(is.null(hillshade)) {stop("hillshade must be passed to afm_plot_3d")}
  if(any(hillshade > 1 || hillshade < 0)) {stop("Argument `hillshade` must not contain any entries less than 0 or more than 1")}
  if(is.null(data)) {stop("data argument must be specified")}
  if(class(data) != "afm_scan") {stop("class of data must be 'afm_scan'")}
  if(!(matrix %in% names(data[["maps"]]))) {stop("matrix not found in data, check your spelling")}

  if(is.numeric(zscale)) {
    z <- zscale
  } else if(zscale == "auto") {
    zscale <- (data[["params"]][["scan_size"]])/(data[["params"]][["afm_lines"]])
  }

  elmat <- data[["maps"]][[matrix]]

  hillshade %>%
    rayshader::plot_3d(elmat, zscale = zscale, ...)
}

