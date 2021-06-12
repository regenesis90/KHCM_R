#' Cycling Road Saturated Traffic Flow Rate (vph)
#'
#' It follows <Formula 15-16> in KHCM(2013), p.648
#' @param width bicycle road width(m)
#' @keywords
#' @export S_bike
#' @examples
#' S_bike(width = 1.3)
#' S_bike(2.0)
S_bike <- function(width = NULL){
  f_w_bike <- f_w_bike(width = width)
  s <- 3000 * f_w_bike
  s
}
