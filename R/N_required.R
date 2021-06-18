#' Number of lanes required(N)
#'
#' This function calculates the number of lanes required(N) from PDDHV and SFi.
#'    It follows KHCM(2013), p.32.
#' @param PDDHV Peak Directional Design Hourly Volume(PDDHV, vph). See \code{\link{PDDHV}}
#' @param SF_i Service Flow Rate(SF_i). See \code{\link{SF_i}}
#' @export N_required \code{ceiling(PDDHV / SF_i)}
#' @examples
#' N_required(PDDHV = 14000, SF_i = 3000)
#' N_required(3000, 540)
N_required <- function(PDDHV = NULL, SF_i = NULL){
  if (PDDHV > 0){
    if (SF_i > 0){res <- ceiling(PDDHV / SF_i)}
    else {res <- '[SF_i] must be positive(vph). Please check that. See [SF_i()].'}
  }
  else {res <- '[PDDHV] must be positive(vph). Please check that. See [PDDHV()].'}
  res
}
