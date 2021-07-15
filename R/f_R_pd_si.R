#' Right Turn Correction Factor for Practically Dedicated Right Turn Lanes at Signalized Intersection
#'
#' Right turn correction factor for practically dedicated right turn lanes at signalized intersection.
#'     This function follows <Formula 8-29> in KHCM(2013), p.243.
#' @param E_R Total straight-line conversion factor of the right turn itself in signalized intersection. See \code{\link{E_R_si}}
#' @param P_R Turning traffic volume ratio of the practically dedicated right-turn lane group i at signalized intersection. See \code{\link{P_R_pd_si}}
#' @keywords right turn correction factor practically dedicated lane signalized intersection
#' @seealso \code{\link{E_R_si}}, \code{\link{P_R_pd_si}}
#' @export f_R_pd_si right turn correction factor
#' @examples
#' f_R_pd_si(E_R = 2.1, P_R = 0.7)
f_R_pd_si <- function(E_R = NULL, P_R = NULL){
  if (is.numeric(E_R) == TRUE & P_R >= 0 & P_R <= 1){fr <- 1 / (1 + P_R * (E_R - 1))}
  else {fr <- 'Error : [E_R] must be numeric. [P_R] must be >= 0 and <= 1. Please check that.'}
  fr
}
