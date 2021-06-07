#' Resistance coefficient against disturbance flow(p_i_nonsignalize_intersection)
#'
#' It follows <Formula 10-3> and <Figure 10-9> in KHCM(2013), p.469
#' @param x Capacity ratio according to demand
#' @keywords
#' @export p_i_nonsignalize_intersection
#' @examples
p_i_nonsignalize_intersection <- function(x = NULL){
  -0.04 * x**2 - 0.64 * x + 1
}
