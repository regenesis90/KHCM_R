#' Straight-through traffic (vph) using public left-turn lanes(V_STL)
#'
#' This function follows <Formula 8-17>, <Formula 8-18> in KHCM(2013)
#' @param V_R *Numeric* Right Turn Traffic Volume(vph)
#' @param V_L *Numeric* Left Turn Traffic Volume(vph)
#' @param V_TH *Numeric* Straight-through traffic (vph)
#' @param E_R *Numeric* Forward conversion factor for right turn
#' @param E_L *Numeric* Forward conversion factor for left turn
#' @param N *Categorical* Total number of access lanes (excluding dedicated left-turn lanes). \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, \code{6}
#' @keywords
#' @export V_STL Straight-through traffic (vph) using public left-turn lanes
#' @examples
#' V_STL(V_R = 323, V_L = 291, V_TH = 999, E_R = 1.2, E_L = 1.09, N = 4)
V_STL <- function(V_R = NULL, V_L = NULL, V_TH = NULL, E_R = NULL, E_L = NULL, N = NULL){
  if (V_R >= 0 & V_L >= 0 & V_TH >= 0 & E_R >= 0 & E_L >= 0 & is.null(N) == FALSE){
    if (N == 4 | N == 6){vstl <- (1/N) * (V_TH + E_R * V_R - E_L * V_L * (N - 1))}
    if (N == 5){vstl <- (1/N) * (2 * (V_TH + E_R * V_R) - E_L * V_L * (N - 2))}
    vstl
  }
}
