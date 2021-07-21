#' Threshold V/c Ratio of Entire Signalized Intersection
#'
#' Threshold V/c ratio of the entire signalized intersection.
#'     It is a good indication of the service level of the intersection as a whole.
#'     It follows <Formula 8-42> in KHCM(2013), p.250.
#' @param C period(sec)
#' @param L Total loss time per cycle (sec)
#' @param y_i Traffic volume ratio for critical lane groups in each prefecture
#' @details
#'     The sum of the traffic ratios of the critical lane groups belonging to each signal appearance is used
#'     to calculate the signal period or the critical V/c ratio for the entire intersection.
#'     This value is an indicator of the congestion level of the entire intersection
#'     under appropriate signal operating conditions.
#' @export X_c_si Critical V/c ratio of the entire intersection
#' @examples
#' X_c_si(C = 182, L = 21, y_i = c(0.2, 0.3, 0.43, 0.2))
X_c_si <- function(C = NULL, L = NULL, y_i = NULL){
  y_sum <- 0
  for (i in 1:length(y_i)){
    y <- y_i[i]
    y_sum <- y_sum + y
  }
  X_c <- C * y_sum / (C - L)
  X_c
}
