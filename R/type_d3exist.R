#' 3 types of extra lag d3 present(type_d3exist)
#'
#' It follows <Formula 8-44>, <Formula 8-45>, <Formula 8-46> in KHCM(2013)
#' @param Q_b
#' @param X
#' @param C
#' @param T
#' @export type_d3exist
#' @examples
type_d3exist <- function(Q_b = NULL, X = NULL, C = NULL, T = NULL){
  k <- (1 - X) * c * T
  if (Q_b > 0 & Q_b < k){type <- 1}
  if (Q_b > k & k > 0){type <- 2}
  if (Q_b > 0 & k < 0){type <- 3}
  type
}
