#' Reads a Bruker afm image file
#'
#' This function will read an AFM image file in .txt format from a Bruker Icon
#' AFM. Bruker scans will be produced in .spm format and can be easily exported
#' from the Nanoscope software as a .txt file. A header should always be
#' included with the scan data when exporting.
#'
#' @param file The full path to the AFM .txt file
#' @param maps Which channels from the scan you want formatted as matrices for
#'   imaging, default will return all channels
#' @param opt_params A list of any extra parameters you want. See details
#' @param scan_size The scan size. Drawn from the header automatically, don't
#'   change unless you know what you're doing
#' @param samps_per_line The samples per line. Drawn from the header
#'   automatically, don't change unless you know what you're doing
#' @param afm_lines The number of lines. Drawn from the header automatically,
#'   don't change unless you know what you're doing
#'
#' @return An object of class S3 containing raw scan data, scan size, lines,
#'   samples per line, maps as rasters, and optional parameters
#'
#' @details By default, afm_read_bruker will detect which channels are present
#'   in your data and return them as a matrix with dimensions [afm_lines,
#'   samps_per_line]. If you only want to include some maps, you must specify a
#'   character vector that contains the names of the maps you want. These names
#'   must match exactly the column names in the .txt AFM file you provided. To
#'   specify optional parameters you should make a named list. If the parameters
#'   you want are included in the header, they will be extracted if you provide
#'   a character vector that matches a header hook. ie. If you want the scan
#'   rate specify opt_params = list(scan_rate = "Scan Rate") and the number will
#'   be fetched from the header. If you want to include parameters that are not
#'   in the header, just specify them within the list as character or numerics
#'   ie. opt_params = list(sample = "mouse_cells", rep = 1). If the parameters
#'   you specify do not appear in the header, it will trigger a warning.
#'
#' @examples
#' #read in with all maps and no optional params
#' \donttest{
#' scan_1 <- afm_read_bruker("scan_1.txt")
#' }
#' #read in with only height map and with optional params
#' \donttest{
#' scan2 <- afm_read_bruker("scan_2.txt", maps = "Height_Sensor(nm)", opt_params = list(scan_rate = "Scan Rate", rep = 1)
#' }
#'
#' @export
#' @importFrom assertthat assert_that
#'
afm_read_bruker <- function(file = NULL, maps = "all", opt_params = list(), scan_size = "Scan Size", samps_per_line = "Samps/line", afm_lines = "Lines"){
  assert_that(!is.null(file))

  afm_text <- readr::read_lines(file)
  afm_header <- afm_text[stringr::str_detect(afm_text, "\\\\")]
  afm_data <- afm_text[((2*length(afm_header))+1):length(afm_text)]

  assert_that(length(afm_header) > 0, msg = "No header found, your file must have a header")
  assert_that(length(afm_data) > 0, msg = "No scan data found, your file must have scan data")

  scanSize <- afm_extract_param(afm_header, scan_size)
  assert_that(is.numeric(scanSize), msg = "scan_size is not numeric,
              check that it is present in
              the header and that you correctly
              specified the name of the parameter")
  sampsPerLine <- afm_extract_param(afm_header, samps_per_line)
  assert_that(is.numeric(sampsPerLine), msg = "samps_per_line is not numeric,
              check that it is present in
              the header and that you correctly
              specified the name of the parameter")
  afmLines <- afm_extract_param(afm_header, afm_lines)
  assert_that(is.numeric(afmLines), msg = "afm_lines is not numeric,
              check that it is present in
              the header and that you correctly
              specified the name of the parameter")
  scan_points <- afm_extract_scan_points(afm_data)

  if(maps == "all"){
    all_channels <- colnames(scan_points)
    map_names <- paste(stringr::str_extract(all_channels, pattern = ".+?(?=\\()"), "matrix", sep = "_")
    afm_maps <- purrr::map(.x = all_channels, ~afm_matrix(scan_points, sampsPerLine, afmLines, channel = .x))
    names(afm_maps) <- map_names
  } else{
    all_channels <- colnames(scan_points)
    select_channels <- maps[maps%in%all_channels]
    map_names <- paste(stringr::str_extract(select_channels, pattern = ".+?(?=\\()"), "matrix", sep = "_")
    afm_maps <- purrr::map(.x = select_channels, ~afm_matrix(scan_points, sampsPerLine, afmLines, channel = .x))
    names(afm_maps) <- map_names
  }

  optional_params <- purrr::map(opt_params, ~extract_parameter(afm_header, .x))

  afm_scan(scan_points, scanSize, sampsPerLine, afmLines, maps = afm_maps, optional_params)
}
