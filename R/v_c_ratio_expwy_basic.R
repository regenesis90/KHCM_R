#' The Volume-to-Capacity ratio(V/C)
#'
#' The Volume-to-Capacity ratio (V/C) measures the level of congestion on a roadway by dividing the volume of traffic (existing or future) by the capacity of the roadway.
#'     It follows definition in KHCM(2013), p.17.
#' @param v Traffic volume(vph) or traffic volume converted to peak hour(vph). See \code{\link{V_P}}
#' @param c Capacity(vph). See \code{\link{capa_expwy_basic}}
#' @export v_c_ratio \code{Vp/C} The Volume-to-Capacity ratio
#' @keywords V/C v_c_ratio volume to capacity ratio
#' @seealso \code{\link{V_P}}, \code{\link{capa_expwy_basic}}
#' @examples
#' v_c_ratio_expwy_basic(v = 1200, c = 2938)
v_c_ratio_expwy_basic <- function(v = NULL, c = NULL){
  if (v >= 0){
    if (c > 0){res <- v/c}
    else {res <- 'Error : [c] must be positive(vph). See [capa_expwy_basic()].'}
  }
  else {res <- 'Error : [v] must be positive(vph). See [V_P_expwy_basic()].'}
  res
}
