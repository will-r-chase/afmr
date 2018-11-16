#' Takes a vector of afm scan data and turns it into a matrix
#'
#' @param data A tibble containing the scan data for several channels
#' @param samps_line samples per line for the scan
#' @param afm_lines number of lines in the scan
#' @param channel the channel you want to turn into a matrix
#'
#' @return a matrix with dimensions [lines, samples_per_line]
#' @importFrom magrittr %>%

afm_matrix <- function(data, samps_line, afm_lines, channel = NULL){
  assert_that(!is.null(channel))
  if(tibble::is.tibble(data) == FALSE & is.data.frame(data) == FALSE) {
    stop("data must be a tibble or dataframe")
  }
  if(nrow(data) < samps_line*afm_lines) {
    stop("there are fewer data points than (samps_line*afm_lines), check that you have the right samples per line and afm lines and that you don't have missing data")
  }

  data %>%
    dplyr::pull(channel) %>%
    matrix(., ncol = samps_line, nrow = afm_lines, byrow = TRUE)
}
