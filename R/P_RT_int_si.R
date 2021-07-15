#' Right Turn Traffic Volume Ratio of Integrated Lane Group Going Straight + Left + Right at Signalized Intersection
#'
#' Right turn traffic volume ratio of the integrated lane group going straight + left + right at signalized intersection.
#'     It follows <Formula 8-25> in KHCM(2013), p.242.
#' @param V_R Right Turn Traffic Volume(vph). See \code{\link{V_R_si}}
#' @param V_L Left Turn Traffic Volume(vph)
#' @param V_TH Straight-through traffic (vph)
#' @seealso \code{\link{V_R_si}}, \code{\link{V_T_int_si}}
#' @keywords turn traffic volume ratio integrated lane group signalized intersection
#' @export P_RT_int_si
#' @examples
#' P_RT_int_si(V_R = 103, V_L = 384, V_TH = 2892)
P_RT_int_si <- function(V_R = NULL, V_L = NULL, V_TH = NULL){
  V_T <- V_T_int_si(V_R = V_R, V_L = V_L, V_TH = V_TH)
  if (is.numeric(V_T) == TRUE){prt <- V_R / V_T}
  else {prt <- V_T}
  prt
}
