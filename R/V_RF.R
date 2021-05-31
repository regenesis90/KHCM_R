#' Number of vehicles going straight ahead of the first right turn in the public right turn lane(V_RF)
#'
#' This function follows <Formula 8-15>, <Formula 8-16> in KHCM(2013)
#' @param V_R *Numeric* Right Turn Traffic Volume(vph)
#' @param V_TH *Numeric* Straight-through traffic (vph)
#' @param C *Numeric* Signal cycle(seconds)
#' @param N *Categorical* Total number of access lanes (excluding dedicated left-turn lanes). \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, \code{6}
#' @keywords
#' @export V_RF Straight-through traffic (vph) ≤VTh/N arriving before the first right turn on a public right-turn lane
#' @examples
#'V_RF(432, 1293, 82, 4)
V_RF <- function(V_R = NULL, V_TH = NULL, C = NULL, N = NULL){
  if (V_R >= 0 & V_TH >= 0 & C >= 0 & (N == 1 | N == 2 | N == 3 | N == 4 | N == 5 | N == 6)){
    if (N == 5){vrf <- 3600 * V_TH / (C * (N - 1) * V_R)}
    else {vrf <- 3600 * V_TH / (C * N * V_R)}
    vrf
  }
}
