#' Offset convenience rate(TVO)
#'
#' It follows <Formula 8-51>, <Formula 8-52>, <Formula 8-53> in KHCM(2013)
#' @param T_c In the section from the stop line of the upstream intersection to the stop line of the analysis intersection,Time (seconds) obtained from the speed and link length of the section not affected by acceleration, deceleration, and stop
#' @param offset The difference (in seconds) between the upstream intersection and the analysis intersection in the continuous direction green signal start time. Use less value than period
#' @param C Common cycle required for arterial linkage (seconds)
#' @export TVO
#' @examples
function(T_c = NULL,  offset = NULL, C = NULL){
  (T_c - offset)/C
}
