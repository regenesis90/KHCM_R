#' Traffic Volume Converted to Peak Hour(Vp)
#'
#' This function calculates traffic volume converted to peak hour(vph) by using PHF(peak hour factor).
#' @param V *Numeric* Current or future traffic volume. It must be positive.
#' @param PHF *Numeric* Peak Hour Factor(PHF). It is a value between 0 and 1.
#' @param f_hv *Numeric* Heavy Vehicle Factors(f_hv)
#' @export Vp \code{V/PHF}(vph)
#' @examples
#' Vp(1000, 0.97)
#' Vp(V = 1300, PHF = 0.93)
Vp <- function(V = NULL, PHF = NULL, f_hv = 1){
  if (V >=0 & PHF >=0 & PHF <=1 & f_hv >= 0){
    V/(PHF*f_hv)
  }
}
