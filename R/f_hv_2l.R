#' Heavy Vehicle Factors in 2-lane Road
#'
#' This function calculates Heavy Vehicle Factors in 2-lane road.
#'     It follows <Formula 7-4> in KHCM(2013), p.174.
#' @param P_T The ratio of total heavy vehicles.
#' @param E_T Heavy vehicle correction factor based on traffic speed or total delay ratio
#' @export f_hv_2l Heavy Vehicle Factors
#' @seealso \code{\link{E_T_D_2l}}, \code{\link{E_T_ATS_2l}}
#' @examples
#' f_hv_2l(P_T = 0.43, E_T = 5.9)
f_hv_2l <- function(P_T = NULL, E_T = NULL){
  if (P_T >= 0 & P_T <= 1){
    if (E_T >= 0){res <- 1 / (1 + P_T * (E_T - 1))}
    else {res <- 'Error : [E_T] must be positive. See [E_T_D_2l()] or [E_T_ATS_2l()]. Please check that.'}
  }
  else {res <- 'Error : [P_T] must be >= 0, <= 1. Please check that.'}
  res
}
