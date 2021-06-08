#' Maximum number of vehicles per hour per stop(c_Bv)
#'
#' It follows <Formula 13-3>, <Formula 13-4> KHCM(2013), p.601
#' @param flow_type Choose one from: \code{'continuous'}, \code{'interrupted'}
#' @param R Parking surface capacity correction factor. See R()
#' @param t_c Erasing time (sec)
#' @param t_D Stop time (seconds) (= door opening/closing time + passenger boarding and disembarking time)
#' @param g_c_ratio
#' @param N See N_bus_using_efficiency(). Utilization efficiency coefficient according to the number of stopping surfaces (or the length of stopping surfaces)
#' @export c_Bv
#' @examples
c_Bv <- function(flow_type = NULL, R = NULL, t_c = NULL, t_D = NULL, g_c_ratio = NULL, N = NULL){
  if (flow_type == 'continuous'){c <- 3600 * R * N / (t_c + t_D)}
  if (flow_type == 'interrupted'){c <- g_c_ratio * 3600 * R * N / (t_c + g_c_ratio * t_D)}
  c
}
