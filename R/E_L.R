#' Comprehensive straight-line conversion factor for left-turning lanes(E_L)
#'
#' This function follows <Formula 8-6> in KHCM(2013)
#' @param E_1 Influence of left turn itself according to the number of left turn lanes
#' @param E_p Influence of curve radius on left turn trajectory
#' @param E_u Influence of u-tern
#' @keywords
#' @export E_L Comprehensive straight-line conversion factor for left-turning lanes
#' @examples
E_L <- function(E_1 = NULL, E_p = NULL, E_u = NULL){
  E_1 * E_p * E_u
}
