#' Effective Pedestrian Sidewalk Width for Planning
#'
#' Calculation of effective sidewalk width for pedestrian roads for the planning and design of pedestrian roads
#'     It follows <Formula 14-10> in KHCM(2013), p.625.
#' @param V Future Demand Pedestrian Traffic Volume (person/min)
#' @param SV_i Service pedestrian traffic flow rate at service level i (person/min/m)
#' @keywords effective pedestrian sidewalk with for planning
#' @seealso \code{\link{V_P_ped}}
#' @export W_E_plan_ped
#' @examples
#' W_E_plan_ped(V = 382, SV_i = 100)
W_E_plan_ped <- function(V = NULL, SV_i = NULL){
  if (V > 0){
    if (SV_i > 0){w <- V / SV_i}
    else {w <- 'Error : [SV_i] must be positive(person/min/m). Please check that.'}
  }
  else {w <- 'Error : [V] must be positive(person/min). Please check that.'}
  w
}
