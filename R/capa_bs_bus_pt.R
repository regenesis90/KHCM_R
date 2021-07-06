#' Bus Vehicle Capacity Per Hour at Bus Stops
#'
#' Bus vehicle capacity per hour at bus stops in uninterrupted or interrupted flow(Buses/h)
#'     It follows <Formula 13-3>, <Formula 13-4> in KHCM(2013), p.601.
#' @param flow_type Choose one from: \code{'uninterrupted'}, \code{'interrupted'}
#' @param R Parking surface capacity correction factor. See \code{\link{R_pt}}
#' @param t_c Erasing time(seconds). See \code{\link{t_c_pt}}
#' @param t_D Stop time(seconds). See \code{\link{t_D_pt}}
#' @param g_c_ratio Ratio of valid green time.
#' @param N Utilization efficiency coefficient according to the number of stopping area. See \code{\link{ue_bs_pt}}
#' @seealso \code{\link{ue_bs_pt}}, \code{\link{R_pt}}, \code{\link{t_c_pt}}, \code{\link{t_D_pt}}
#' @export capa_bs_bus_pt
#' @examples
#' capa_bs_bus_pt(flow_type = 'interrupted', R = 0.93, t_c = 13.2, t_D = 20, g_c_ratio = 0.3, N = 1.75)
capa_bs_bus_pt <- function(flow_type = NULL, R = NULL, t_c = NULL, t_D = NULL, g_c_ratio = NULL, N = NULL, P = NULL){
  cb <- capa_area_pt(flow_type = flow_type, R = R, t_c = t_c, t_D = t_D, g_c_ratio = g_c_ratio)
  if (is.numeric(cb) == TRUE){
    if (N > 0){capa <- cb * N}
    else {capa <- 'Error : [N] must be positive integer. Please check that.'}
  }
  else {capa <- cb}
  capa
}
