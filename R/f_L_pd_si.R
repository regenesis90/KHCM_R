#' Left Turn Correction Factor for Practically Dedicated Left Turn Lanes at Signalized Intersection
#'
#' Left turn correction factor for practically dedicated left turn lanes at signalized intersection.
#'     This function follows <Formula 8-28> in KHCM(2013), p.243.
#' @param E_L Total straight-line conversion factor of the left turn itself in signalized intersection. See \code{\link{E_L_si}}
#' @param P_L Turning traffic volume ratio of the practically dedicated left-turn lane group i at signalized intersection. See \code{\link{P_L_pd_si}}
#' @keywords Left turn correction factor practically dedicated lane signalized intersection
#' @seealso \code{\link{E_L_si}}, \code{\link{P_L_pd_si}}
#' @export f_L_pd_si Left turn correction factor
#' @examples
#' f_L_pd_si(E_L = 2.1, P_L = 0.7)
f_L_pd_si <- function(E_L = NULL, P_L = NULL){
  if (is.numeric(E_L) == TRUE & P_L >= 0 & P_L <= 1){fl <- 1 / (1 + P_L * (E_L - 1))}
  else {fl <- 'Error : [E_L] must be numeric. [P_L] must be >= 0 and <= 1. Please check that.'}
  fl
}
