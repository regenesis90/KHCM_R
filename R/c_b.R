#' Maximum number of buses per hour per bus stopping area (c_b, vph)
#'
#' It follows <Formula 13-1>, <Formula 13-2> KHCM(2013), p.598, p.599
#' @param flow_type Choose one from: \code{'continuous'}, \code{'interrupted'}
#' @param R Parking surface capacity correction factor. See R()
#' @param t_c Erasing time (sec)
#' @param t_D Stop time (seconds) (= door opening/closing time + passenger boarding and disembarking time)
#' @param g_c_ratio
#' @export c_b
#' @examples
c_b <- function(flow_type = NULL, R = NULL, t_c = NULL, t_D = NULL, g_c_ratio = NULL){
  if (flow_type == 'continuous'){c <- 3600 * R / (t_c + t_D)}
  if (flow_type == 'interrupted'){c <- g_c_ratio * 3600 * R / (t_c + g_c_ratio * t_D)}
  c
}
