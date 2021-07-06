#' Total Occupancy Time of Bus at the Bus Stop
#'
#' Total occupancy time of the bus(s).
#'     It is equal to the sum of the erase time in the deceleration section,
#'     the stop time (stop time at the bus stop),
#'     and the erase time in the acceleration section.
#'     It follows <Figure 13-2> in KHCM(2013), p.590.
#' @param t_D Bus stop time(s)
#' @param t_c Total clearance time of bus at bus stop(s)
#' @export t_ocp_pt Total occupancy time of bus(s)
#' @seealso \code{\link{t_D_pt}}, \code{\link{t_c_pt}}
#' @examples
#' t_ocp_pt(t_D = 15.5, t_c = 16)
t_ocp_pt <- function(t_D = NULL, t_c = NULL){
  if (t_D >= 0 & t_c >= 0){
    t <- t_D + t_c
  }
  else {t <- 'Error : [t_D], [t_c] must be positive(s). Please check that.'}
  t
}
