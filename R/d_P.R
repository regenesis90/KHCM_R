#' Average Pedestrian Delay (seconds)
#'
#' It follows <Formula 14-5> in KHCM(2013), p.623
#' @param C Signal period(seconds)
#' @param g Effective green time for pedestrians (seconds)
#' @keywords
#' @export d_P
#' @examples
d_P <- function(C = NULL, g = NULL){
  (C - g)**2 / (2 * C)
}
