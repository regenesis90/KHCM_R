#' Total Traffic Volume of Approach Road in Integrated Lane Group for Going Straight, Left and Right Turns at Signalized Intersection
#'
#' The total traffic volume (vph) of the approach road in the integrated lane group
#'     for going straight, left and right turns at signalized intersection.
#'     It follows <Formula 8-25> in KHCM(2013), p.242.
#' @param V_R Right Turn Traffic Volume(vph). See \code{\link{V_R_si}}
#' @param V_L Left Turn Traffic Volume(vph)
#' @param V_TH Straight-through traffic (vph)
#' @seealso \code{\link{V_R_si}}, \code{\link{P_LT_int_si}}, \code{\link{P_RT_int_si}}
#' @keywords traffic volume land use signalized intersection
#' @export V_T_int_si
#' @examples
#' V_T_int_si(V_R = 103, V_L = 384, V_TH = 2892)
V_T_int_si <- function(V_R = NULL, V_L = NULL, V_TH = NULL){
  if (V_R >= 0 & V_TH >= 0 & V_L >= 0){vt <- V_R + V_L + V_TH}
  else {vt <- 'Error : [V_R], [V_TH], [V_L] must be >= 0(vph). Please check that.'}
  vt
}
