#' Traffic Volume Converted to Peak Hour
#'
#' This function calculates traffic volume converted to peak hour(vph) by using PHF(peak hour factor).
#'    It follows <Formula 2-7> in KHCM(2013), p.17, 30.
#' @param v Current or future traffic volume(vph). It must be positive. See \code{\link{DDHV}}.
#' @param PHF Peak Hour Factor(PHF). See \code{\link{PHF_expwy_basic}}.
#' @export V_P_expwy_basic \code{V/PHF}(vph)
#' @examples
#' V_P_expwy_basic(1200, 0.98)
#' V_P_expwy_basic(b = 1300, PHF = 0.93)
V_P_expwy_basic <- function(v = NULL, PHF = NULL){
  if (v > 0){
    if (PHF >= 0 & PHF < 1){res <- v/PHF}
    else {res <- 'Error : [PHF] must be positive, and less than 1. Please check that. See [PHF_expwy_basic()].'}
  }
  else {res <- 'Error : [v] must be positive(vph). Please check that. See [DDHV()].'}
  res
}
