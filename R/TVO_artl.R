#' Offset Convenience Rate(TVO) on Arterial Road
#'
#' Offset convenience rate on arterial roads.
#'     It is a value required to estimate the interlocking coefficient of a signal.
#'     It follows <Formula 12-8> in KHCM(2013) p.540.
#' @param T_c In the section from the stop line of the upstream intersection to the stop line of the analysis intersection.
#'     Time (seconds) obtained from the speed and link length of the section not affected by acceleration, deceleration, and stop
#' @param offset The difference (in seconds) between the upstream intersection and the analysis intersection in the continuous direction green signal start time. Use less value than period
#' @param C Common cycle required for arterial linkage (seconds)
#' @export TVO_artl
#' @examples
#' TVO_artl(T_c = 100, offset = 30, C = 120)
TVO_artl <- function(T_c = NULL, offset = NULL, C = NULL){
  if (T_c > 0 & offset > 0 & C > 0){
    tvo <- (T_c - offset)/C
  }
  else {tvo <- 'Error : [T_c], [offset], [C] must be positive. Please check that.'}
  tvo
}
