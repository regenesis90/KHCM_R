#' Ratio of Weaving Traffic Volume
#'
#' The value obtained by dividing the weaving traffic volume(pcph) by the total traffic volume(pcph) of the expressway weaving section.
#'    It follows the definition in KHCM(2013), p.61.
#' @param V_w Weaving traffic volume(pcph)
#' @param V Total traffic volume(pcph).
#' @export VR_expwy_wv \code{V_w/V}
#' @keywords VR ratio weaving traffic volume
#' @seealso \code{\link{W_expwy_wv_nw}}, \code{\link{W_expwy_wv_w}}, \code{\link{appl_expwy_wv}}
#' @examples
#' VR_expwy_wv(V_w = 372, V = 1142)
#' VR_expwy_wv(432, 1449)
VR_expwy_wv <- function(V_w = NULL, V = NULL){
  if (V_w > 0 & V > 0 & V_w <= V){
    V_w/V
  }
  else {'Error : [V_w], [V] must be positive. And always [V_w] <= [V]. Please check that.'}
}
