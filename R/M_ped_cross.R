#' Crosswalk occupied space per pedestrian (m^2/person)
#'
#' It follows <Formula 14-7> in KHCM(2013), p.624
#'     Based on this result, the service level is judged according to the value of the occupied space presented in <Table 14-1>.
#' @param TS Space-time area (m^2/person) at a crosswalk at a traffic light intersection. See TS()
#' @param T_pedestrian_occupancy See T_pedestrian_occupancy()
#' @keywords
#' @export M_ped_cross
#' @examples
M_ped_cross <- function(TS = NULL, T_pedestrian_occupancy = NULL){
  TS / T_pedestrian_occupancy
}
