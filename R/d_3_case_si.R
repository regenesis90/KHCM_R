#' Classification of Cases Where Initial Queue Delay Exists According to the Influence of Initial Waiting Vehicle at Signalized Intersection.
#'
#' Classification of cases where initial queue delay exists according to the influence of the initial waiting vehicle at the signalized intersection.
#'     It follows <Formula 8-51>, <Formula 8-52>, <Formula 8-53> in KHCM(2013), p.252.
#' @param Q_b Initial number of vehicles on standby at the beginning of the analysis period (t) (vehicles)
#' @param c The capacity (vph) of the corresponding vehicle group during the analysis period
#' @param v Arrival traffic volume (vph) for the corresponding lane group during the analysis period
#' @param t Analysis period length (hours)
#' @param X The saturation of the lane group
#' @details
#'     If there is a waiting vehicle before the start of the analysis period,
#'     the vehicles arriving at the beginning of the analysis period form a queue,
#'     and while these waiting vehicles are released,
#'     the vehicles arriving during the analysis period must have an additional delay.
#'     Therefore, if there are no waiting vehicles at the start of the analysis,
#'     this additional delay need not be considered.
#'     Since this additional delay is to calculate the delay of the arriving vehicle during the analysis period,
#'     the delay of the initial waiting vehicle during the analysis period is not considered.
#'     * \code{'case1'} : If there is an initial waiting vehicle and all traffic arriving within the analysis period is processed, and no waiting vehicle remains after the analysis period
#'     * \code{'case2'} : If there is an initial waiting vehicle and there are still waiting vehicles after the analysis period, but the length is shorter than the initial queue
#'     * \code{'case3'} : If there is an initial waiting vehicle and the waiting vehicle still remains after the analysis period has elapsed, but the length is longer than the initial waiting queue
#' @export d_3_case_si \code{'case1'}, \code{'case2'}, \code{'case3'}
#' @examples
#' d_3_case_si(Q_b = 234, c = 1200, v = 212, t = 1, X = 0.4)
d_3_case_si <- function(Q_b = NULL, c = NULL, v = NULL, t = NULL, X = NULL){
  if (X >= 0){
    if (c > 0 & v > 0){
      if (t > 0){
        if (Q_b >= 0){
          K <- (1 - X) * c * t
          if (Q_b < K){case <- 'case1'}
          else if (0 < K & K < Q_b){case <- 'case2'}
          else if (K < 0){case <- 'case3'}
          else {case <- 'Error : [X] == 1? ??'}
        }
        else {case <- 'Error : [Q_b] must be positive(veh). Please check that.'}
      }
      else {case <- 'Error : [t] must be positive(hours). Please check that.'}
    }
    else {case <- 'Error : [c], [v] must be positive(vph). Please check that.'}
  }
  else {case <- 'Error : [X] must be positive. Please check that.'}
  case
}
