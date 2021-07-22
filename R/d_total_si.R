#' Average Control Delay per Vehicle at Signalized Intersection I
#'
#' Average control delay per vehicle at signalized intersection I(sec/veh).
#'     It follows <Formula 8-56> in KHCM(2013), p.258.
#' @param d_A Average control delay per vehicle in approach A(sec/veh), See \code{\link{d_A_si}}
#' @param v_A Corrected traffic volume (vph) in approach A(vph)
#' @seealso \code{\link{d_A_si}}
#' @export d_total_si
#' @examples
#' d_total_si(d_A = c(10, 2.4, 3.5, 6.2), v_A = c(230, 432, 581, 398))
d_total_si <- function(d_A = NULL, v_A = NULL){
  da_sum <- 0
  va_sum <- 0
  for (i in 1:length(d_A)){
    da <- d_A[i] * v_A[i]
    va <- v_A[i]
    da_sum <- da_sum + da
    va_sum <- va_sum + va
  }
  da_sum/va_sum
}
