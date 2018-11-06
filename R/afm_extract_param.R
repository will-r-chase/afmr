#' Extract parameter values from the AFM header
#'
#' This is a flexible function that will first try to extract the supplied
#' parameter from the header. If the parameter is not found in the header, it
#' will simply return the parameter value that you specified. This allows for
#' addition of parameters that are not included in the nanoscope output, but
#' might be important for a user such as "treatment" "cell#" "material" etc.
#'
#' @param header The AFM file header as a character vector.
#' @param parameter The desired parameter. If you are trying to extract a
#'   parameter from the header, you must use a character vector that matches
#'   exactly (case sensitive) the parameter in the header. ie. "Scan Rate"
#'
#' @return If the parameter was found in the header, it will be returned either
#'   as numeric or character. If the parameter was not found in the header, it
#'   will be returned as supplied and a warning will be triggered
#' @export
#'
#' @examples
#' afm_extract_param(afm_header, "Scan Rate")
#'
#' afm_extract_param(afm_header, "treatment_1")
#'
afm_extract_param <- function(header, parameter){
  parameter_regex <- paste0("\\\\", parameter, ":")
  parameter_text <- header[str_detect(header, parameter_regex)][1]
  if (is.na(parameter_text)) {
    warning(paste(parameter, "was not found in the header, returning", parameter, "directly"))
    return(parameter)
  } else {
    tryCatch({readr::parse_number(parameter_text)},
             warning = function(w){str_extract(parameter_text, pattern = '(?<=: )(.*)(?=")')})
  }
}

