#' Left Turn and Right Turn Correction Factor for Integrated Lane Group at Signalized Intersection
#'
#' Left turn and right turn correction factor for integrated lane group at signalized intersection
#'     This function follows <Formula 8-32> in KHCM(2013), p.243.
#' @param E_L Forward conversion coefficient for shared left turns in a left turn lane at signalized intersection. See \code{\link{E_L_si}}
#' @param E_R Forward conversion coefficient for shared right turns in a right turn lane at signalized intersection. See \code{\link{E_R_si}}
#' @param P_LT Turn traffic ratio for shared left-turn lane group i. See \code{\link{P_LT_sh_si}}
#' @param P_RT Turn traffic ratio for shared right-turn lane group i. See \code{\link{P_RT_sh_si}}
#' @keywords Left right turn correction factor integrated lane signalized intersection
#' @seealso \code{\link{E_L_si}}, \code{\link{E_R_si}}, \code{\link{P_LT_sh_si}}, \code{\link{P_RT_sh_si}}
#' @export f_int_si
#' @examples
#' f_int_si(E_L = 1.32, P_LT = 0.33, E_R = 1.44, P_RT = 0.5)
f_int_si <- function(E_L = NULL, E_R = NULL, P_LT = NULL, P_RT = NULL){
  if (is.numeric(E_L) == TRUE & P_LT >= 0 & P_LT <= 1){
    if (is.numeric(E_R) == TRUE & P_RT >= 0 & P_RT <= 1){
      fint <- 1 / (1 + P_LT * (E_L - 1) + P_RT * (E_R - 1))
    }
    else {fint <- 'Error : [E_R] must be numeric. [P_RT] must be >= 0 and <= 1. Please check that.'}
  }
  else {fint <- 'Error : [E_L] must be numeric. [P_LT] must be >= 0 and <= 1. Please check that.'}
  fint
}
