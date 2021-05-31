#' Straight-through traffic (vph) using public right-turn lanes(V_STR)
#'
#' This function follows <Formula 8-19>, <Formula 8-20> in KHCM(2013)
#' @param V_R *Numeric* Right Turn Traffic Volume(vph)
#' @param V_L *Numeric* Left Turn Traffic Volume(vph)
#' @param V_TH *Numeric* Straight-through traffic (vph)
#' @param E_R *Numeric* Forward conversion factor for right turn
#' @param E_L *Numeric* Forward conversion factor for left turn
#' @param N *Categorical* Total number of access lanes (excluding dedicated left-turn lanes). \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, \code{6}
#' @keywords
#' @export V_STR Straight-through traffic (vph) using public right-turn lanes
#' @examples
#' V_STR(92, 103, 763, 1.3, 2.123, 5)
V_STR <- function(V_R = NULL, V_L = NULL, V_TH = NULL, E_R = NULL, E_L = NULL, N = NULL){
  if (V_R >= 0 & V_L >= 0 & V_TH >= 0 & E_R >= 0 & E_L >= 0 & is.null(N) == FALSE){
    if (N == 1 | N == 2 | N == 3){vstr <- (1/N) * (V_TH + E_R * V_R * (N - 1))}
    if (N == 4 | N == 5 | N == 6){vstr <- (1/N) * (V_TH + E_L * V_L - E_R * V_R * (N - 1))}
    vstr
  }
}
