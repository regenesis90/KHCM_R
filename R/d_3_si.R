#' Additional delay when there is an initial waiting vehicle at the signal intersection
#'
#' Additional delay when there is an initial waiting vehicle at the signal intersection.
#'     It means initial queue delay. It is occured due to initial queuing due to oversaturation before analysis.
#'     It follows <Formula 8-51>, <Formula 8-52>, <Formula 8-53> in KHCM(2013), p.255.
#' @param case Choose one from : \code{'case1'}, \code{'case2'}, \code{'case3'}. See \code{\link{d_3_case_si}}
#' @param Q_b Initial number of vehicles on standby at the beginning of the analysis period (T) (veh)
#' @param c The capacity (vph) of the corresponding vehicle group during the analysis period
#' @param v Arrival traffic volume (vph) for the corresponding lane group during the analysis period
#' @param t Analysis period length (hours)
#' @param X The saturation of the lane group
#' @seealso \code{\link{d_3_case_si}}, \code{\link{d_si}}
#' @export d_3_si
#' @examples
#' d_3_si(case = 'case1', Q_b = 322, c = 1800, v = 400, t = 1.2)
#' d_3_si(case = 'case2', Q_b = 322, c = 1200, t = 1.2, X = 0.7)
#' d_3_si(case = 'case3', Q_b = 322, c = 1200)
d_3_si <- function(case = NULL, Q_b = NULL, c = NULL, v = NULL, t = NULL, X = NULL){
  if (case == 'case1'){
    if (Q_b > 0){
      if (c > 0){
        if (t > 0){
          if (v > 0){d <- 1800 * (Q_b**2) / (c * t * (c - v))}
          else {d <- 'Error : [v] must be positive(vph). Please check that.'}
        }
        else {d <- 'Error : [t] must be positive(hours). Please check that.'}
      }
      else {d <- 'Error : [c] must be positive(vph). Please check that.'}
    }
    else {d <- 'Error : [d] must be positive(veh). Please check that.'}
  }
  else if (case == 'case2'){
    if (Q_b > 0){
      if (c > 0){
        if (t > 0){
          if (X > 0){d <- 3600 * Q_b / c - (1800 * t * (1 - X))}
          else {d <- 'Error : [X] must be positive. Please check that.'}
        }
        else {d <- 'Error : [t] must be positive(hours). Please check that.'}
      }
      else {d <- 'Error : [c] must be positive(vph). Please check that.'}
    }
    else {d <- 'Error : [d] must be positive(veh). Please check that.'}
  }
  else if (case == 'case3'){
    if (Q_b > 0){
      if (c > 0){d <- 3600 * Q_b / c}
      else {d <- 'Error : [c] must be positive(vph). Please check that.'}
    }
    else {d <- 'Error : [d] must be positive(veh). Please check that.'}
  }
  else {d <- 'Error : [case] must be one of [case1], [case2], [case3]. Please check that.'}
  d
}
