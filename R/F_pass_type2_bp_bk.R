#' Number of Overtaking Between Bicycle and Pedestrian Walking in the Same Direction in Bicycle-Pedestrian Road
#'
#' Number of times a bicycle overtakes a pedestrian walking in the same direction(times/h) in bicyle-pedestrian road(type2).
#'     * It follows <Formula 15-6> in KHCM(2013), p.645.
#' @param Q_ped_sm Pedestrian traffic flow rate in the same direction(person/h)
#' @param U_bike Average bike speed (kph)
#' @param U_ped Average walking speed of pedestrian(kph)
#' @keywords number overtaking bicycle pedestrian bicycle-pedestrian road
#' @seealso \code{\link{F_pass_type2_bb_bk}}, \code{\link{F_pass_type2_pb_bk}}
#' @export F_pass_type2_bp_bk Number of times a bicycle overtakes a pedestrian walking in the same direction (times/h)
#' @examples
#' F_pass_type2_bp_bk(Q_ped_sm = 94, U_bike = 10.32, U_ped = 5.42)
F_pass_type2_bp_bk <- function(Q_ped_sm = NULL, U_bike = NULL, U_ped = NULL){
  if (Q_ped_sm > 0){
    if (U_bike > 0 & U_ped > 0){
      f <- Q_ped_sm * (U_bike / U_ped - 1)
    }
    else {f <- 'Error : [U_bike], [U_ped] must be positive(kph). Please check that.'}
  }
  else {f <- 'Error : [Q_ped_sm] must be positive(person/h). Please check that.'}
  f
}
