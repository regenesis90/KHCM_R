#' Right Turn Correction Factor for Shared Right Turn Lanes at Signalized Intersection
#'
#' Right turn correction factor for shared right turn lanes at signalized intersections.
#'     This function follows <Formula 8-31> in KHCM(2013), p.243.
#' @param E_R Forward conversion coefficient for public right turns in a right turn lane at signalized intersection. See \code{\link{E_R_si}}
#' @param P_RT Turn traffic ratio for shared right-turn lane group i. See \code{\link{P_RT_sh_si}}
#' @keywords right turn correction factor shared lane signalized intersection
#' @seealso \code{\link{E_R_si}}, \code{\link{P_RT_sh_si}}
#' @export f_RT_sh_si Right turn correction factor
#' @examples
#' f_RT_sh_si(E_R = 1.32, P_RT = 0.33)
f_RT_sh_si <- function(E_R = NULL, P_RT = NULL){
  if (is.numeric(E_R) == TRUE & P_RT >= 0 & P_RT <= 1){fr <- 1 / (1 + P_RT * (E_R - 1))}
  else {fr <- 'Error : [E_R] must be numeric. [P_RT] must be >= 0 and <= 1. Please check that.'}
  fr
}
