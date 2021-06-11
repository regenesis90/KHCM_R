#' Total number of conflicts in bicycle traffic (times/h)
#'
#' It follows <Formula 15-4> in KHCM(2013), p.644
#' @param F_pass Number of overtaking bicycles in the direction of travel (times/h)
#' @param F_meet Number of bicycles in the opposite direction (times/h)
#' @keywords
#' @export F_total Total number of conflicts in bicycle traffic (times/h)
#' @examples
F_total <- function(F_pass = NULL, F_meet = NULL){
  0.5 * F_meet + F_pass
}
