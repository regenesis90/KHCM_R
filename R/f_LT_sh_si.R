#' Left Turn Correction Factor for Shared Left Turn Lanes at Signalized Intersection
#'
#' Left turn correction factor for shared left turn lanes at signalized intersections.
#'     This function follows <Formula 8-31> in KHCM(2013), p.243.
#' @param E_L Forward conversion coefficient for public Left turns in a left turn lane at signalized intersection. See \code{\link{E_L_si}}
#' @param P_LT Turn traffic ratio for shared Left-turn lane group i. See \code{\link{P_LT_sh_si}}
#' @keywords Left turn correction factor shared lane signalized intersection
#' @seealso \code{\link{E_L_si}}, \code{\link{P_LT_sh_si}}
#' @export f_LT_sh_si Left turn correction factor
#' @examples
#' f_LT_sh_si(E_L = 1.32, P_LT = 0.33)
f_LT_sh_si <- function(E_L = NULL, P_LT = NULL){
  if (is.numeric(E_L) == TRUE & P_LT >= 0 & P_LT <= 1){fl <- 1 / (1 + P_LT * (E_L - 1))}
  else {fl <- 'Error : [E_L] must be numeric. [P_LT] must be >= 0 and <= 1. Please check that.'}
  fl
}
