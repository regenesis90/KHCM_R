#' Section travel time per km(t_p_km_arterial, seconds/km)
#'
#' It follows <Table 12-6> in KHCM(2013) p.536
#' @param free_speed Choose one from : \code{80}, \code{70}, \code{60}
#' @param roadside_friction_arterial See roadside_friction_arterial()
#' @param L section length(km)
#' @keywords
#' @export t_p_km_arterial
#' @examples
t_p_km_arterial <- function(free_speed = NULL, roadside_friction_arterial = NULL, L = NULL){
  if (free_speed == 80){
    if (roadside_friction_arterial == 'big'){}
    if (roadside_friction_arterial == 'small'){}
  }
  if (free_speed == 70){
    if (roadside_friction_arterial == 'big'){}
    if (roadside_friction_arterial == 'small'){}
  }
  if (free_speed == 60){
    if (roadside_friction_arterial == 'big'){}
    if (roadside_friction_arterial == 'small'){}
  }
}
