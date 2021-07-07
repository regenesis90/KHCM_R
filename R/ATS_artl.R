#' Average Travel Speed on Arterial Roads
#'
#' The average speed (kph) of all or part of an arterial road.
#'     The total approach delay and travel time per km are required.
#'     It follows <Formula 12-1>< <Formula 12-9> in KHCM(2013) p.535, 544.
#' @param L Section length(km). Extension of all or part of an arterial road.
#' @param t_trv_km Total traveling time per km of all or part of the road (sec/km). See \code{\link{t_trv_km_artl}}
#' @param d_total Total approach delay (seconds) at intersections within the scope of analysis for all or part of the arterial road
#' @keywords ATS average travel speed arterial road
#' @details 3600 means Conversion factor for converting speed units of m/sec to km/hour.
#' @seealso \code{\link{t_trv_km_artl}}
#' @export ATS_artl
#' @examples
#' ATS_artl(L = 1.2, t_trv_km = 50, d_total = 20)
ATS_artl <- function(L = NULL, t_trv_km = NULL, d_total = NULL){
  if (L > 0){
    if (t_trv_km > 0){
      if (d_total > 0){ats <- 3600 * L / (t_trv_km * L + d_total)}
      else {ats <- 'Error : [d_total] must be positive(seconds). Please check that.'}
    }
    else {ats <- 'Error : [t_trv_km] must be positive(sec/km). Please check that.'}
  }
  else{ats <- 'Error : [L] must be positive(km). Please check that.'}
  ats
}
