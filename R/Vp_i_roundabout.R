#' Traffic Volume Converted to Peak Hour in Roundabout(Vp_i_roundabout)
#'
#' This function calculates traffic volume converted to peak hour(vph) by using PHF(peak hour factor).
#' @param V_i *Numeric* i ramp traffic (vph)
#' @param PHF *Numeric* Peak Hour Factor(PHF). It is a value between 0 and 1.
#' @export Vp_i_roundabout i Traffic volume at the peak of the ramp (vph)
#' @examples
#' Vp(1000, 0.97)
#' Vp(V = 1300, PHF = 0.93)
Vp_i_roundabout <- function(V_i = NULL, PHF = NULL){
  V_i / PHF
}
