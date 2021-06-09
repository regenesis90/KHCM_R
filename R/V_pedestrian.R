#' Pedestrian traffic flow rate (person/min/m)
#'
#' It follows <Formula 14-1>, <Formula 14-2> in KHCM(2013), p.613
#' @param S Walking speed (m/min)
#' @param D Walking density (person/㎡)
#' @param M Pedestrian occupied space (㎡/person)
#' @export V_pedestrian
#' @examples
#' V_pedestrian(S = 50, D = 3)
#' V_pedestrian(S = 32.3, M = 12)
V_pedestrian <- function(S = NULL, D = NULL, M = NULL){
  if (is.null(D) == FALSE){
    v <- S * D
  }
  else{
    v <- S / M
  }
  v
}
