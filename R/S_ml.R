#' Average Travel Speed of the Entire Section in Multi-lane Road
#'
#' Average travel speed of the entire section in multi-lane road(kph).
#'     It follows <Formula 6-4> in KHCM(2013), p.146.
#' @param L Total section length (km)
#' @param n Number of divided sections
#' @param L_n Ln = Length of section n (km)
#' @param S_n Boundary value of service level i of section n (average travel speed, kph)
#' @export S_i Average travel speed of section n (kph).
#' @seealso \code{\link{S_i_ml}}
#' @examples
#' S_ml(L = 5, L_n = c(1, 1.5, 1.8, 0.7), S_n = c(78, 68.2, 84.5, 50.7))
S_ml <- function(L = NULL, L_n = NULL, S_n = NULL){
  lssum <- 0
  if (L > 0){
    if (length(L_n) == length(S_n)){
      for (i in 1:length(L_n)){
        ls <- L_n[i]/S_n[i]
        lssum <- lssum + ls
      }
      result <- L / lssum
    }
    else {result <- 'Error : length(L_n) == length(S_n). Please check that.'}
  }
  else {result <- 'Error : [L], ust be positive. Please check that.'}
  result
}
