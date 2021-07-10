#' Potential Capacity per x Moving Flow at Unsignalized Intersection
#'
#' Potential capacity per x moving flow at unsignalized intersection(pcph).
#'     It follows <Formula 10-1> in KHCM(2013), p.466
#' @param v_c Conflict flow for flow x(pcph). see \code{\link{V_c_i_nsi}}
#' @param form Intersection form. Choose one from: \code{'1x1'}, \code{'2x1'}
#' @param dir1 Flow direction. Choose one from : \code{'main'}, \code{'sub'}
#' @param dir2 Flow direction. Choose one from: \code{'left'}, \code{'straight'}, \code{'right'}
#' @seealso \code{\link{V_c_i_nsi}}, \code{\link{t_c_x_nsi}}, \code{\link{t_f_x_nsi}}
#' @keywords potential capacity per x moving flow unsignalized Intersection
#' @export capa_p_x_nsi
#' @examples
#' capa_p_x_nsi(v_c = 942, form = '1x1', dir1 = 'sub', dir2 = 'straight')
#' capa_p_x_nsi(v_c = 343, form = '2x1', dir1 = 'main', dir2 = 'left')
capa_p_x_nsi <- function(v_c = NULL, form = NULL, dir1 = NULL, dir2 = NULL){
  if (v_c >= 0){
    t_f <- t_f_x_nsi(form = form, dir1 = dir1, dir2 = dir2)
    t_c <- t_c_x_nsi(form = form, dir1 = dir1, dir2 = dir2)
    if (is.numeric(t_f) == TRUE & is.numeric(t_c) == TRUE){
      c <- v_c * (exp(-1 * v_c * t_c / 3600))/(1 - exp(-1 * v_c * t_f / 3600))
    }
    else {c <- t_f}
  }
  else {c <- 'Error : [v_c] must be positive(pcph). Please check that.'}
  c
}
