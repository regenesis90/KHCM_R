#' Saturated traffic flow rate of lane group i at Signalized Intersection
#'
#' Saturated traffic flow rate of lane group i at signalized intersection.
#'     It follows <Formula 8-38> in KHCM(2013), p.247.
#' @param N_i number of lanes in the i lane group
#' @param f_LT Left turn correction factor for shared left turn lanes at signalized intersections. See \code{\link{f_LT_sh_si}}
#' @param f_RT Right turn correction factor for shared right turn lanes at signalized intersections. See \code{\link{f_RT_sh_si}}
#' @param f_w Lane width correction factor. See \code{\link{f_w_si}}
#' @param f_g Approach slope correction factor. See \code{\link{f_g_si}}
#' @param f_hv Heavy vehicle factor. See \code{\link{f_hv_si}}
#' @seealso \code{\link{f_LT_sh_si}}, \code{\link{f_RT_sh_si}}, \code{\link{f_w_si}}, \code{\link{f_g_si}}, \code{\link{f_hv_si}}
#' @export S_i_si Saturated traffic flow rate of lane group i
#' @examples
#' S_i_si(N_i = 2, f_LT = 2.2, f_RT = 1.1, f_w = 1.0, f_g = 1.2, f_hv = 1.5)
#' S_i_si(N_i = 2, f_w = 1.0, f_g = 1.2, f_hv = 1.5)
#' S_i_si(N_i = 2, f_LT = 1.1, f_w = 1.0, f_g = 1.2, f_hv = 1.5)
#' S_i_si(N_i = 2, f_RT = 1.1, f_w = 1.0, f_g = 1.2, f_hv = 1.5)
S_i_si <- function(N_i = NULL, f_LT = 1, f_RT = 1, f_w = NULL, f_g = NULL, f_hv = NULL){
  if (N_i >= 1){
    if (f_w > 0){
      if (f_hv > 0){
        if (f_g > 0){
          if (f_LT > 0 & f_RT > 0){s <- S_0_si() * N_i * f_w * f_g * f_hv}
          else {s <- 'Error : [f_RT], [f_LT] must be positive. Please check that.'}
        }
        else {s <- 'Error : [f_g] must be positive. Please check that.'}
      }
      else {s <- 'Error : [f_hv] must be positive. Please check that.'}
    }
    else {s <- 'Error : [f_w] must be positive. Please check that.'}
  }
  else {s <- 'Error : [N_i] must be positive integer. Please check that.'}
  s
}

