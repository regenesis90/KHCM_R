#' Number of vehicles going straight ahead of the first left turn in the public left turn lane(V_LF)
#'
#' This function follows <Formula 8-13>, <Formula 8-14> in KHCM(2013)
#' @param V_L *Numeric* Left Turn Traffic Volume(vph)
#' @param V_TH *Numeric* Straight-through traffic (vph)
#' @param C *Numeric* Signal cycle(seconds)
#' @param N *Categorical* Total number of access lanes (excluding dedicated left-turn lanes). \code{4}, \code{5}, \code{6}
#' @keywords
#' @export V_LF Straight-through traffic (vph) ≤VTh/N arriving before the first left turn on a public left-turn lane
#' @examples
#' V_LF(423, 923, 76, 4)
V_LF <- function(V_L = NULL, V_TH = NULL, C = NULL, N = NULL){
  if (V_L >= 0 & V_TH >= 0 & C >= 0 & (N == 4 | N == 5 | N == 6)){
    if (N == 5){vlf <- 7200 * V_TH / (C * (N - 1) * V_L)}
    else {vlf <- 3600 * V_TH / (C * N * V_L)}
    vlf
  }
}
