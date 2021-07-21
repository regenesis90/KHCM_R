#' Degree of Saturation of the i-lane Group at Signalized Intersection
#'
#' Degree of saturation of the i-lane group at signalized intersection.
#'     It means the ratio of traffic volume and capacity of lane i.
#'     It follows <Formula 8-41> in KHCM(2013), p.250.
#' @param S_i Saturated traffic flow rate (vph) for the i lane group. See \code{\link{S_i_si}}
#' @param g_i Effective green time for i lane group (seconds)
#' @param C signal period(seconds)
#' @param v_i i traffic volume in lane group (vph)
#' @details The value of X_i generally has a value of 0~1.0, but sometimes it shows a value greater than 1.0 when the arrival traffic exceeds the capacity.
#' @seealso \code{\link{S_i_si}}
#' @export X_i_si capacity of the i-th lane group
#' @examples
#' X_i_si(S_i = 1200, g_i = 45, C = 180, v_i = 122)
X_i_si <- function(S_i = NULL, g_i = NULL, C = NULL, v_i = NULL){
  if (S_i > 0){
    if (g_i > 0 & C > 0){
      if (v_i > 0){xi <- v_i * C / (S_i * g_i)}
      else {xi <- 'Error : [v_i] must be positive(vph). Please check that.'}
    }
    else {xi <- 'Error : [g_i], [C] must be positive(sec). Please check that.'}
  }
  else {xi <- 'Error : [S_i] must be positive(vph). Please check that.'}
  xi
}
