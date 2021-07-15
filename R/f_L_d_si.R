#' Left-turn Correction Factor for Left-turn Lanes Dedicated to Signalized Intersection
#'
#' Left-turn correction factor for left-turn lanes dedicated to signalized intersections
#'     This function follows <Formula 8-27> in KHCM(2013), p.243.
#' @param E_L Total straight-line conversion factor of the left turn itself in signalized intersection. \code{\link{E_L_si}}
#' @keywords left-turn correction factor dedicated lane signalized intersection
#' @seealso \code{\link{E_L_si}}
#' @export f_L_d_si Left turn correction factor
#' @examples
#' f_L_d_si(E_L = 2.1)
f_L_d_si <- function(E_L = NULL){
  if (E_L > 0){f <- 1 / E_L}
  else {f <- 'Error : [E_L] must be positive. Please check that.'}
  f
}
