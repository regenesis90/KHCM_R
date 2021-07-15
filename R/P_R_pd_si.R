#' Turning Traffic Volume Ratio of the Practically Dedicated Right Turn Lane Group i at Signalized Intersection.
#'
#' Turning traffic volume ratio of the practically dedicated right turn lane group i at signalized intersection.
#'    The ratio of right turns in the actual dedicated right turn lane group
#'    This function follows <Formula 8-22> in KHCM(2013), p.242.
#' @param V_R Corrected right turn traffic volume (vph) for RTOR. See \code{\link{V_R_si}}
#' @param V_RF Traffic going straight ahead of the first right turn on the public right turn lane at the signal intersection(vph). See \code{\link{V_RF_si}}
#' @keywords turning traffic volume ratio practically dedicated right turn lane group signalized intersection
#' @seealso \code{\link{V_RF_si}}, \code{\link{V_R_si}}
#' @export P_R_pd_si
#' @examples
#' P_R_pd_si(V_R = 321, V_RF = 283)
P_R_pd_si <- function(V_R = NULL, V_RF = NULL){
  if (V_R >= 0 & V_RF >= 0 & ((V_R + V_RF) > 0)){
    p <- V_R / (V_RF + V_R)
  }
  else {p <- 'Error : [V_R], [V_RF] must be positive(vph). Please check that.'}
  p
}
