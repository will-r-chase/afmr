#' Constructs an "afm_scan" S3 object from inputs
#'
#' @param scan_points (required) A tibble with the scan data
#' @param scanSize (required) The scan size
#' @param sampsPerLine (required) The samples per line
#' @param afmLines (required) The number of lines
#' @param maps (optional) afm maps (matrices)
#' @param optional_params (optional) Any other parameters to include (see
#'   details)
#'
#' @return An S3 object of class "afm_scan"
#' @export
#'
#' @details This function is typically used under the hood by afm_read_bruker,
#'   but it can be used to construct an afm_scan object from a list of values
#'   which could be useful as a control or for teaching. The first four
#'   parameters are required, while the maps and optional_params are optional.
#'   Supplied maps should be a list of matrices representing the matrix
#'   representation of different afm channels. Other parameters such as
#'   experimental details should be supplied as a list to optional_params.

afm_scan <- function(scan_points = NULL, scanSize = NULL, sampsPerLine = NULL, afmLines = NULL, maps = list(), optional_params = list()){
  assert_that(!is.null(scan_points), !is.null(scanSize), !is.null(sampsPerLine), !is.null(afmLines))
  assert_that(is.numeric(scanSize), is.numeric(sampsPerLine), is.numeric(afmLines))

  if(tibble::is.tibble(scan_points) == FALSE & is.data.frame(scan_points) == FALSE) {
    stop("scan_points must be a tibble or dataframe")
  }

  data <- list(
    params = list(scan_size = scanSize, samps_per_line = sampsPerLine, afm_lines = afmLines),
    scan_data = scan_points,
    opt_params = optional_params,
    maps = maps
  )

  class(data) <- "afm_scan"
  return(data)
}
