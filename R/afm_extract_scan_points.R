#' Format scan data as a tibble
#'
#' This function takes a character vector (usually from readLines()) of afm scan
#' data and formats it as a nice tibble where each column is one afm channel
#' (ie. height, modulus)
#'
#' @param data The afm scan data (header removed) as a character vector
#'
#' @return A tibble with each column as one channel
#'
afm_extract_scan_points <- function(data){
  assert_that(is.character(data))

  afm_df <- data %>%
    stringr::str_squish() %>%
    tibble::as.tibble()

  names <- unlist(strsplit(x = as.character(afm_df[1, ]), split = " "))
  assert_that(is.character(names))

  afm_df %>%
    dplyr::slice(-1) %>%
    {
      tryCatch(
        {tidyr::separate(., col = value, into = names, sep = " ", convert = TRUE)},
        warning = function(w) {stop("The rows of your scan data had an unequal
                                    number of columns. Check that all lines of
                                    your scan data are complete.")}
      )
    }
}
