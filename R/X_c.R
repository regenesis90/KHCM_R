#' Critical V/c ratio(X_c)
#'
#' It follows <Formula 8-42> in KHCM(2013)
#' @param C period(sec)
#' @param L Total loss time per cycle (sec)
#' @param y_i Traffic volume ratio for critical lane groups in each prefecture
#' @export X_c Critical V/c ratio of the entire intersection
#' @examples
X_c <- function(C = NULL, L = NULL, y_i = NULL){
  y_sum <- 0
  for (i in 1:length(y_i)){
    y <- y_i[i]
    y_sum <- y_sum + y
  }
  X_c <- C * y_sum / (C - L)
  X_c
}
