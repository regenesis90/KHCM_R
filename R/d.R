#' Average control delay per vehicle (d, sec/set)
#'
#' It follows <Formula 8-43> in KHCM(2013)
#' @param d_1
#' @param d_2
#' @param d_3
#' @param PF
#' @export d Average control delay per vehicle
#' @examples
d <- function(d_1 = NULL, d_2 = NULL, d_3 = NULL, PF = NULL){
  d_1 * PF + d_2 + d_3
}
