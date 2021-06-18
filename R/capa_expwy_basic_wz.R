#' Capacity of Expressway Work Zone
#'
#' This function calculates the expressway work zone(construction section) capacity(vph).
#'     It follows <Formula 2-10> in KHCM(2013), p.37.
#' @param c_jw Basic capacity of expressway work zone at design speed j(pcphpl). See \code{\link{capa_expwy_basic_wz_jw}}.
#' @param N The number of freeway lane(one-way lane). It means (# of normal one-way lane) - (# of lanes closed due to construction)
#' @param f_w The lane width and side clearance factor. See \code{\link{f_w_expwy_basic}}.
#' @param f_hv Heavy Vehicle Factors. See \code{\link{f_hv_expwy_basic}}.
#' @keywords capacity workzone work zone construction
#' @seealso \code{\link{f_hv_expwy_basic}},
#' @export capa_expwy_basic_wz Work zone capacity(vph).
#' @examples
#' capa_expwy_basic_wz(c_jw = 1750, N = 4, f_w = 0.91, f_hv = 0.642)
#' capa_expwy_basic_wz(1700, 3, 0.91, 0.588)
capa_expwy_basic_wz <- function(c_jw = NULL, N = NULL, f_w = NULL, f_hv = NULL){
  if (is.numeric(c_jw) == TRUE){
    if (N >= 1){
      if (f_w > 0){
        if (f_hv > 0){c_jw * N * f_w * f_hv}
        else{'Error : [f_hv] must be positive. Please check that. See [f_hv_expwy_basic()].'}
      }
      else{'Error : [f_w] must be positive. Please check that. See [f_w_expwy_basic()].'}
    }
    else {'Error : [N] must be >= 1. Please check that.'}
  }
  else {'Error : [c_jw] must be positive. Please check that. See [capa_expwy_basic_wz_j()]'}
}
