#' Bus Capacity Processed Per Hour on One Bus Stop.
#'
#' Bus capacity processed per hour on one stop(buses/h)
#' It follows <Formula 13-1>, <Formula 13-2> KHCM(2013), p.598, p.599
#' @param flow_type Choose one from: \code{'uninterrupted'}, \code{'interrupted'}
#' @param R Parking surface capacity correction factor. See \code{\link{R_pt}}
#' @param t_c Erasing time(seconds). See \code{\link{t_c_pt}}
#' @param t_D Stop time(seconds). See \code{\link{t_D_pt}}
#' @param g_c_ratio Ratio of valid green time.
#' @seealso \code{\link{R_pt}}, \code{\link{t_c_pt}}, \code{\link{t_D_pt}}
#' @export capa_area_pt
#' @examples
#' capa_area_pt(flow_type = 'interrupted', R = 0.93, t_c = 13.2, t_D = 20, g_c_ratio = 0.3)
capa_area_pt <- function(flow_type = NULL, R = NULL, t_c = NULL, t_D = NULL, g_c_ratio = NULL){
  if (flow_type == 'uninterrupted'){
    if (R > 0 & t_c > 0 & t_D > 0){c <- 3600 * R / (t_c + t_D)}
    else {c <- 'Error : [R], [t_c], [t_D] must be positive. Please check that.'}
  }
  else if (flow_type == 'interrupted'){
    if (R > 0 & t_c > 0 & t_D > 0 & g_c_ratio >= 0 & g_c_ratio <= 1){c <- g_c_ratio * 3600 * R / (t_c + g_c_ratio * t_D)}
    else {c <- 'Error : [R], [t_c], [t_D], [g_c_ratio] must be positive. Please check that.'}
    }
  else {c <- 'Error : [flow_type] must be one of [uninterrupted], [interrupted]. Please check that.'}
  c
}
