#' Bicycle road capacity (vph)
#'
#' It follows <Formula 15-16> in KHCM(2013), p.648
#' @param g Valid green time (sec)
#' @param C Signal period(sec)
#' @param width Bicycle Road width(m)
#' @keywords
#' @export c_bike
#' @examples
#' c_bike(g = 30, C = 130, width = 1.5)
c_bike <- function(g = NULL, C = NULL, width = NULL){
  s <- S_bike(width = width)
  cb <- s * g / C
  cb
}
