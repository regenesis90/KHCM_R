#' Capacity of Bicycle Road
#'
#' Capacity of bicycle road(vph).
#'     It follows <Formula 15-16> in KHCM(2013), p.648
#' @param g Valid green time(sec)
#' @param C Signal period(sec)
#' @param width Bicycle Road width(m)
#' @keywords capacity bicycle road
#' @export capa_bk
#' @examples
#' capa_bk(g = 30, C = 130, width = 1.5)
capa_bk <- function(g = NULL, C = NULL, width = NULL){
  s <- S_bike(width = width)
  if (is.numeric(s) == TRUE){
    if (C > 0){
      if (g > 0){
        cb <- s * g / C
      }
      else {cb <- 'Error : [g] must be positive(sec). Please check that.'}
    }
    else {cb <- 'Error : [C] must be positive(sec). Please check that.'}
  }
  else {cb <- s}
  cb
}
