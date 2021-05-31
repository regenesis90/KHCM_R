#' Heavy Vehicle Factors in Signalized Intersection(f_hv3)
#'
#' It follows <Formula 8-39> in KHCM(2013)
#' @param P Mixing ratio of heavy vehicles to actual traffic volume
#' @export f_hv3 Heavy vehicle Factor
#' @examples
f_hv3 <- function(P = NULL){
  1 / (1 + 0.8 * P)
}
