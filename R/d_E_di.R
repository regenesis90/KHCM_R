#' Delay of the Moving Flow E Passing Straight through Intersection 1 and Turning Left at Intersection 2
#'
#' delay of the moving flow E passing straight through intersection 1 and turning left at intersection 2
#'     att the two-point intersection type diamond-shaped interchange (highway connection road-general road junction)
#'     It follows <Formula 9-11> in KHCM(2013) p.434.
#' @param d_EBTH Delay (seconds) when passing straight through intersection 1
#' @param d_EBL Delay (seconds) when turning left at Intersection 2
#' @seealso \code{\link{d_total_di}}
#' @export d_artl delay moving flow diamond interchange
#' @examples
#' d_E_di(d_EBTH = 4.2, d_EBL = 2.1)
d_E_di <- function(d_EBTH = NULL, d_EBL = NULL){
  if (d_EBTH >= 0 & d_EBL >= 0){d <- d_EBTH + d_EBL}
  else {d <- 'Error : [d_EBTH], [d_EBL] must be >= 0(seconds). Please check that.'}
  d
}
