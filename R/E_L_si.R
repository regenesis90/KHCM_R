#' Total Straight-line Conversion Factor of the Left Turn Itself in Signalized Intersection
#'
#' Total straight-line conversion factor of the left turn itself in signalized intersection.
#'     This is the overall straight-forward conversion factor for left-turning lanes.
#'     The effect of the left turn itself according to the number of left turn lanes,
#'     the effect of the curve radius of the left turn trajectory,
#'     and the effect of the U-turn are comprehensively considered.
#'     This function follows <Formula 8-6> in KHCM(2013), p.227.
#' @param E_1 Influence of left turn itself according to the number of left turn lanes. See \code{\link{E_1_si}}
#' @param E_p Influence of curve radius on left turn trajectory. See \code{\link{E_P_si}}
#' @param E_u Influence of u-tern. See \code{\link{E_U_si}}
#' @keywords total straight-line conversion factor left turn signalized intersection
#' @seealso \code{\link{E_1_si}}, \code{\link{E_P_si}}, \code{\link{E_U_si}}
#' @export E_L_si Comprehensive straight-line conversion factor for left-turning lanes
#' @examples
#' E_L_si(E_1 = 10.32, E_p = 39.11, E_u = 32.22)
E_L_si <- function(E_1 = NULL, E_p = NULL, E_u = NULL){
  if (E_1 > 0 & E_p >= 0 & E_u >= 0){el <- E_1 * E_p * E_u}
  else {el <- 'Error : [E_1], [E_p], [E_u] must be positive. Please check that.'}
  el
}
