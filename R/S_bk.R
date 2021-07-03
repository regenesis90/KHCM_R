#' Saturated Traffic Flow Rate on Bicycle Roads
#'
#' Saturated traffic flow rate(vph) on bicycle roads.
#'     It follows <Formula 15-16> in KHCM(2013), p.648
#' @param width bicycle road width(m)
#' @keywords bicycle road saturated traffic flow
#' @export S_bk
#' @examples
#' S_bk(width = 1.3)
#' S_bk(2.0)
S_bk <- function(width = NULL){
  if (width >= 1){
    f_w <- f_w_bk(width = width)
    s <- 3000 * f_w
  }
  else {s <- 'Error : [width] must be >= 1(m). Please check that.'}
  s
}
