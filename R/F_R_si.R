#' Right Turn Traffic Volume Correction Factor at Signalized Intersection
#'
#' Right turn traffic volume correction factor at signalized intersection.
#'     This function follows <Table 8-6> in KHCM(2013), p.226.
#' @param legs Leg of signalized intersection. Choose one from : \code{3}, \code{4}
#' @param dowry \code{'yes'}, \code{'no'}
#' @keywords Right Turn Traffic Volume Correction Factor Coefficient
#' @export F_R_si Right turn traffic volume correction factor
#' @seealso \code{\link{V_R_si}}
#' @examples
#' F_R_si(legs = 3)
#' F_R(legs = 4, dowry = 'no')
F_R_si <- function(legs = NULL, dowry = NULL){
  if (legs == 3){f <- 0.5}
  else if (legs == 4){
    if (dowry == 'yes'){f <- 0.4}
    else if (dowry == 'no'){f <- 0.5}
    else {f <- 'Error : [dowry] must be one of [yes], [no]. Please check that.'}
  }
  else {f <- 'Error : [legs] must be one of 3, 4. Please check that.'}
  f
}
