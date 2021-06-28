#' Length of Influenced Upstream of the Signal Intersection
#'
#' Length of the sphere of influence upstream of the signal intersection(m).
#'     This function follows <Formula 7-3> in KHCM(2013), p.174.
#' @param v_d Traffic in the direction of travel(pcph)
#' @param LB Whether the left turn-only lane exist. \code{LB = 1} means exist. \code{LB = 0} means not.
#' @param g_c_ratio Effective green time ratio
#' @param L Direction traffic volume left Turn ratio
#' @param DIS Direction traffic volume straight forward ratio
#' @keywords Influenced upstream signal intersection length
#' @export ESL_2l Length of the sphere of influence upstream of the signal intersection(m).
#' @examples
#' ESL_2l(v_d = 1000, LB = 1, g_c_ratio = 0.23, L = 0.2, DIS = 0.73)
#' ESL_2l(300, 0, 0.11, 0.123, 0.84)
ESL_2l <- function(v_d = NULL, LB = NULL, g_c_ratio = NULL, L = NULL, DIS = NULL){
  if (v_d >= 0){
    if (LB == 1 | LB == 0){
      if (g_c_ratio >= 0 & g_c_ratio <= 1){
        if (L >= 0 & L <= 1){
          if (DIS >= 0 & DIS <= 1){
            res <- 242 + (74 * (v_d/100)) - (102 * LB) - (70 * ((v_d/100) * (g_c_ratio)) + (152 * ((v_d/100) * L * DIS)))
          }
          else {res <- 'Error : [DIS] must be >= 0 and <= 1. Please check that.'}
        }
        else {res <- 'Error : [L] must be >= 0 and <= 1. Please check that.'}
      }
      else {res <- 'Error : [g_c_ratio] must be >= 0 and <= 1. Please check that.'}
    }
    else (res <- 'Error : [LB] must be one of 0, 1. Please check that.')
  }
  else {res <- 'Error : [v_d] must be positive(pcph). Please check that.'}
  res
}
