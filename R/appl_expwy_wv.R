#' Applicability of Speed Formula in Expressway Weaving Section
#'
#' This function decides the applicability of speed formula in expressway weaving section.
#'     It follows definitions in KHCM(2013), p.61-62.
#' @param type Type of weaving section. Choose one from: \code{'ramp'}, \code{'frontage'}
#' @param V_w Weaving traffic volume(pcph)
#' @param V Total traffic volume(pcph).
#' @param N Total number of lanes in the weaving section. Choose one from: \code{3}, \code{4}, \code{5}
#' @keywords applicability speed formula exxpressway weaving section
#' @seealso \code{\link{VR_expwy_wv}}, \code{\link{S_expwy_wv}}, \code{\link{D_expwy_wv}}, \code{\link{LOS_expwy_wv_ramp}}, \code{\link{LOS_expwy_wv_fr}}
#' @export appl_expwy_wv \code{'OK'}, \code{'Warning'}
#' @examples
#' appl_expwy_wv(type = 'ramp', V_w = 1839, V = 3200, N = 4)
#' appl_expwy_wv(type = 'frontage', V_w = 882, V = 2738, N = 3)
appl_expwy_wv <- function(type = NULL, V_w = NULL, V = NULL, N = NULL){
  VR <- VR_expwy_wv(V_w = V_w, V = V)
  if (is.numeric(VR) == TRUE){
    if (N == 3){
      if (type == 'ramp'){
        if (VR <= 0.5 & V/N <= 2000 & V_w <= 2800){res <- 'OK'}
        else {res <- 'Warning'}
      }
      else if (type == 'frontage'){
        if (VR <= 0.5 & V_w <= 3000){res <- 'OK'}
        else {res <- 'Warning'}
      }
      else {res <- 'Error : [type] must be one of [ramp] or [frontage]. Please check that.'}
    }
    else if (N == 4){
      if (type == 'ramp'){
        if (VR <= 0.45 & V/N <= 2000 & V_w <= 2800){res <- 'OK'}
        else {res <- 'Warning'}
      }
      else if (type == 'frontage'){
        if (VR <= 0.45 & V_w <= 3000){res <- 'OK'}
        else {res <- 'Warning'}
      }
      else {res <- 'Error : [type] must be one of [ramp] or [frontage]. Please check that.'}
    }
    else if (N == 5){
      if (type == 'ramp'){
        if (VR <= 0.40 & V/N <= 2000 & V_w <= 2800){res <- 'OK'}
        else {res <- 'Warning'}
      }
      else if (type == 'frontage'){
        if (VR <= 0.40 & V_w <= 3000){res <- 'OK'}
        else {res <- 'Warning'}
      }
      else {res <- 'Error : [type] must be one of [ramp] or [frontage]. Please check that.'}
    }
    else {res <- 'Error : [N] must be one of [3], [4], [5]. Please check that.'}
  }
  else {res <- print(VR)}
  res
}
