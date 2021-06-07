#' Conversion factor for passenger cars of heavy vehicles(E_T_roundabout)
#'
#' It follows <Table 11-4> in KHCM(2013) p.500
#' @param lane Choose one from L \code{1}, \code{2}
#' @param hv_ratio Heavy Vehicle Ratio(%)
#' @export E_T_roundabout Conversion factor for passenger cars of heavy vehicles
#' @examples
E_T_roundabout <- function(lane = NULL, hv_ratio = NULL){
  if (lane == 1){
    if (hv_ratio >= 0 & hv_ratio <= 5){et <- 2.4}
    if (hv_ratio > 5 & hv_ratio <= 10){et <- 2.4}
    if (hv_ratio > 10 & hv_ratio <= 15){et <- 2.4}
    if (hv_ratio > 15){et <- 2.5}
  }
  if (lane == 2){
    if (hv_ratio >= 0 & hv_ratio <= 5){et <- 2.4}
    if (hv_ratio > 5 & hv_ratio <= 10){et <- 2.5}
    if (hv_ratio > 10 & hv_ratio <= 15){et <- 2.6}
    if (hv_ratio > 15){et <- 2.7}
  }
  et
}
