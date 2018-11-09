#' A 500nm x 500nm AFM scan
#'
#' This is an AFM scan of the inner surface of the plant cell wall
#' collected on a Bruker Icon AFM with a scan-asyst fluid+ tip. The
#' data was collected in PeakForce QNM mode, and contains channels for
#' height, peakforce, DMT modulus, adhesion, dissipation
#'
#' @format An object of class "afm_data"
#' \describe{
#'   \item{params}{the critical parameters of scan size (nm), lines, and samples per line}
#'   \item{scan_data}{a dataframe with the raw scan data}
#'   \item{opt_params}{optional parameters (empty)}
#'   \item{maps}{the matrices of dimension [lines, samples per line] corresponding to the channels in scan_data}
#'   ...
#' }
"half_um_scan"

#' A 2um x 2um AFM scan
#'
#' This is an AFM scan of the inner surface of the plant cell wall
#' collected on a Bruker Icon AFM with a scan-asyst fluid+ tip. The
#' data was collected in PeakForce QNM mode, and contains channels for
#' height, peakforce, DMT modulus, adhesion, dissipation
#'
#' @format An object of class "afm_data"
#' \describe{
#'   \item{params}{the critical parameters of scan size (nm), lines, and samples per line}
#'   \item{scan_data}{a dataframe with the raw scan data}
#'   \item{opt_params}{optional parameters (empty)}
#'   \item{maps}{the matrices of dimension [lines, samples per line] corresponding to the channels in scan_data}
#'   ...
#' }
"two_um_scan"
