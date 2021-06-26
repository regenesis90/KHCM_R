#' Boundary Value of Service Level i for the Entire Interval
#'
#' Boundary value of service level i for the entire section (average travel speed, kph)
#'     It follows <Formula 6-3> in KHCM(2013), p.146.
#' @param L Total section length (km)
#' @param n Number of divided sections
#' @param L_n Ln = Length of section n (km)
#' @param S_n_i Boundary value of service level i of section n (average travel speed, kph)
#' @export S_i Saturated traffic flow rate of lane group i
#' @seealso \code{\link{S_ml}}
#' @examples
#' S_i_ml(L = 3.2, L_n = c(1, 0.4, 0.5, 1.3), S_n_i = c(80, 74, 62, 67))
S_i_ml <- function(L = NULL, L_n = NULL, S_n_i = NULL){
  lssum <- 0
  if (L > 0){
    if (length(L_n) == length(S_n_i)){
      for (i in 1:length(L_n)){
        ls <- L_n[i]/S_n_i[i]
        lssum <- lssum + ls
      }
      result <- L / lssum
      }
    else {result <- 'Error : length(L_n) == length(S_n_i). Please check that.'}
  }
  else {result <- 'Error : [L], ust be positive. Please check that.'}
  result
}
