extract_parameter <- function(header, parameter){
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
