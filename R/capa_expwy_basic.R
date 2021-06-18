#' Capacity in Basic Section of Expressway
#'
#' Capacity (vph) of the expressway section that is the j design speed reflecting special circumstances (work zone, weather, day and night)
#'     It follows <Formula 2-11> KHCM(2013), p.34.
#' @param c_j Basic Capacity of Basic Section of Expressway at Design Speed j(pcphpl). See \code{\link{capa_expwy_basic_wz_jw}} or \code{\link{capa_expwy_basic_j}}
#' @param N The number of freeway lane(one-way lane). It means (# of normal one-way lane) - (# of lanes closed due to construction)
#' @param f_w The lane width and side clearance factor. See \code{\link{f_w_expwy_basic}}.
#' @param f_hv Heavy Vehicle Factors. See \code{\link{f_hv_expwy_basic}}.
#' @param f_iw Correction Factor for Capacity Estimation in Case of Bad Weather in Basic Section of Expressway. See \code{\link{f_iw_expwy_basic}}.
#' @param f_dk Nighttime Correction Factor in Basic Section of Expressway. See \code{\link{f_dk}}.
#' @export capa_expwy_basic
#' @keywords capacity expressway workzone basic section
#' @seealso \code{\link{capa_expwy_basic_wz_jw}}, \code{\link{capa_expwy_basic_j}}, \code{\link{f_w_expwy_basic}}, \code{\link{f_hv_expwy_basic}}, \code{\link{f_iw_expwy_basic}}, \code{\link{f_dk}}
#' @examples
#' capa_expwy_basic(c_j = 2000, N = 3, f_w = 0.87, f_hv = 0.488, f_iw = 0.97, f_dk = 1)
capa_expwy_basic <- function(c_j = NULL, N = NULL, f_w = NULL, f_hv = NULL, f_iw = NULL, f_dk = NULL){
  if (c_j > 0){
    if (N >= 1){
      if (f_w > 0){
        if (f_hv > 0){
          if (f_iw > 0){
            if (f_dk > 0){res <- c_j * N * f_w * f_hv * f_iw * f_dk}
            else{res <- 'Error : [f_dk] must be positive. Please check that. See [f_dk()].'}
          }
          else {res <- 'Error : [f_iw] must be positive. Please check that. See [f_iw_expwy_basic()].'}
        }
        else {res <- 'Error : [f_hv] must be positive. Please check that. See [f_hv_expwy_basic()].'}
      }
      else{res <- 'Error : [f_w] must be positive. Please check that. See [f_w_expwy_basic()].'}
    }
    else{res <- 'Error : [N] must be >= 1 and integer. Please check that.'}
  }
  else {res <- 'Error : [c_j] must be positive(pcphpl). Please check that. See [capa_expwy_basic_wz_jw()] or [capa_expwy_basic_j()].'}
  res
}
