#' Crosswalk Occupied Space Per Pedestrian at Signal Crosswalk
#'
#' Crosswalk occupied space per pedestrian at signal crosswalk(㎡/person).
#'     It follows <Formula 14-7> in KHCM(2013), p.624
#'     Based on this result, the service level is judged according to the value of the occupied space presented in <Table 14-1>.
#' @param TS Time-space area of pedestrians at signal crosswalks (㎡-person). See \code{\link{TS_cross_ped}}
#' @param TT Total crosswalk occupation time of pedestrians at signal crosswalks(person-second). See \code{\link{T_cross_ped}}
#' @keywords crosswalk signal occupied space pedestrian
#' @seealso \code{\link{TS_cross_ped}}, \code{\link{T_cross_ped}}
#' @export M_cross_ped
#' @examples
#' M_cross_ped(TS = 800, T = 253)
M_cross_ped <- function(TS = NULL, TT = NULL){
  if (TS > 0 & TT > 0){m <- TS / TT}
  else {m <- 'Error : [TS], [TT] must be positive. Please check that.'}
  m
}
