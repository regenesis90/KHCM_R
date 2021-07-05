#' Total Number of Conflicts for Cyclists in Bicycle-Pedestrian Road
#'
#' Total number of conflicts on the bike-pedestrian road(type2) in cyclists view.
#'     It follows <Formula 15-11>, <Formula 15-24> in KHCM(2013), p.646, p.650.
#' @param Q_bike_sm Same direction Bicycle traffic flow rate (vph)
#' @param Q_bike_op Bicycle traffic flow rate in opposite direction (vph)
#' @param Q_ped_sm Same direction pedestrian traffic flow rate (vph)
#' @param Q_ped_op Pedestrian traffic flow rate (person/h) walking in the opposite direction to the bicycle
#' @param U_bike Average bike speed (kph)
#' @param U_ped Average walking speed (kph)
#' @param sd Standard deviation of bicycle speed.
#' @keywords bicycle-pedestrian road total number of conflicts cyclist
#' @seealso \code{\link{F_pass_type2_bb_bk}}, \code{\link{F_pass_type2_bp_bk}}, \code{\link{F_meet_type2_bb_bk}}, \code{\link{F_meet_type2_bp_bk}}
#' @export F_total_type2_b_bk Total number of conflicts on the bike-pedestrian road(type2) in cyclists view(times/h)
#' @examples
F_total_type2_b_bk <- function(Q_bike_sm = NULL, Q_bike_op = NULL, Q_ped_sm = NULL, Q_ped_op = NULL, U_bike = NULL, U_ped = NULL, sd = NULL){
  if (Q_bike_sm >= 0 & Q_bike_op >= 0){
    if (Q_ped_sm >= 0 & Q_ped_op >= 0){
      if (is.null(U_bike) == TRUE & is.null(U_ped) == TRUE & is.null(sd) == TRUE){
        f <- (Q_bike_sm + Q_bike_op) * (1 - 0.75 * (Q_bike_sm/(Q_bike_sm + Q_bike_op))) + 2 * (Q_ped_sm + Q_ped_op)
      }
      else {
        if (U_bike > 0 & U_ped > 0 & sd > 0){
          F_pass_bb <- F_pass_type2_bb_bk(Q_bike_sm = Q_bike_sm, U_bike = U_bike, sd = sd)
          F_pass_bp <- F_pass_type2_bp_bk(Q_ped_sm = Q_ped_sm, U_bike = U_bike, U_ped = U_ped)
          F_meet_bb <- F_meet_type2_bb_bk(Q_bike_op = Q_bike_op)
          F_meet_bp <- F_meet_type2_bp_bk(Q_ped_sm = Q_ped_sm, U_bike = U_bike, U_ped = U_ped)
          f <- F_pass_bb + F_pass_bp + 0.5 * (F_meet_bb + F_meet_bp)
        }
        else {f <- 'Error : [U_bike], [U_ped], [sd] must be positive. Please check that.'}
      }
    }
    else {f <- 'Error : [Q_ped_sm], [Q_ped_op] must be positive(person/h). Please check that.'}
  }
  else {f <- 'Error : [Q_bike_sm], [Q_bike_op] must be positive(vph). Please check that.'}
  f
}
F_total_type2_b_bk(Q_bike_sm = 321, Q_bike_op = 142, Q_ped_sm = 88, Q_ped_op = 191, U_bike = 12.98, U_ped = 5.1, sd = 1.58)
