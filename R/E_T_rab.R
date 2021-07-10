#' Conversion Factor for Passenger Vehicles of Heavy Vehicles at Roundabouts
#'
#' Conversion factor for passenger vehicles of heavy vehicles at roundabouts.
#'     It follows <Table 11-4> in KHCM(2013) p.500.
#' @param lane Roundabout lane. Choose one from : \code{1}, \code{2}
#' @param pcn_hv Heavy Vehicle Ratio(%)
#' @keywords conversion factor passenger vehicle roundabout heavy
#' @seealso \code{\link{f_hv_rab}}, \code{\link{V_i_pce_rab}}
#' @export E_T_rab Conversion factor for passenger cars of heavy vehicles
#' @examples
#' E_T_rab(lane = 2, hv_pcn = 12.3)
E_T_rab <- function(lane = NULL, hv_pcn = NULL){
  if (lane == 1){
    if (hv_pcn >= 0 & hv_pcn <= 5){et <- 2.4}
    else if (hv_pcn > 5 & hv_pcn <= 10){et <- 2.4}
    else if (hv_pcn > 10 & hv_pcn <= 15){et <- 2.4}
    else if (hv_pcn > 15){et <- 2.5}
  }
  else if (lane == 2){
    if (hv_pcn >= 0 & hv_pcn <= 5){et <- 2.4}
    else if (hv_pcn > 5 & hv_pcn <= 10){et <- 2.5}
    else if (hv_pcn > 10 & hv_pcn <= 15){et <- 2.6}
    else if (hv_pcn > 15){et <- 2.7}
  }
  else {et <- 'Error : [lane] must be one of 1, 2. Please check that.'}
  et
}
