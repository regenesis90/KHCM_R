#' Offset Convenience Rate at Signalized Intersection
#'
#' Offset convenience rate at signalized intersection.
#'     It is calculated to calculate the fixed time signal interlocking factor (PF) of the signal intersection.
#'     It follows <Formula 8-54> in KHCM(2013) p.256.
#' @param T_c In the section from the stop line of the upstream intersection to the stop line of the analysis intersection,Time (seconds) obtained from the speed and link length of the section not affected by acceleration, deceleration, and stop
#' @param offset The difference (in seconds) between the upstream intersection and the analysis intersection in the continuous direction green signal start time. Use less value than period
#' @param C Common cycle required for arterial linkage (seconds)
#' @export TVO_si
#' @seealso \code{\link{PF_si}}
#' @examples
#' TVO_si(T_c = 24.2, offset = 10, C = 150)
TVO_si <- function(T_c = NULL,  offset = NULL, C = NULL){
  if (T_c >= 0){
    if (offset >= 0){
      if (C > 0 & offset < C){tvo <- (T_c - offset)/C}
      else {tvo <- 'Error : [C] must be positive(sec) and [offset] < [C]. Please check that.'}
    }
    else {tvo <- 'Error : [offset] must be positive(sec). Please check that.'}
  }
  else {tvo <- 'Error : [T_c] must be positive(sec). Please check that.'}
  tvo
}
