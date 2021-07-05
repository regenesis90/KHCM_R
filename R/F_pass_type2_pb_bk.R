#' Number of Overtaking in Pedestrian's View Between Bicycle and Pedestrian Walking in the Same Direction in Bicycle-Pedestrian Road
#'
#' Number of times a pedestrian is overtaken by a bicycle traveling in the same direction (times/h) in bicyle-pedestrian road(type2).
#'     * It follows <Formula 15-7> in KHCM(2013), p.645.
#' @param Q_bike_sm Bicycle traffic flow rate in the same direction(vph)
#' @param U_bike Average bike speed (kph)
#' @param U_ped Average walking speed of pedestrian(kph)
#' @keywords number overtaking bicycle pedestrian bicycle-pedestrian road
#' @seealso \code{\link{F_pass_type2_bb_bk}}, \code{\link{F_pass_type2_pb_bk}}
#' @export F_pass_type2_pb_bk Number of times a bicycle overtakes a pedestrian walking in the same direction (times/h)
#' @examples
#' F_pass_type2_pb_bk(Q_bike_sm = 258, U_bike = 10.32, U_ped = 5.42)
F_pass_type2_pb_bk <- function(Q_bike_sm = NULL, U_bike = NULL, U_ped = NULL){
  if (Q_bike_sm > 0){
    if (U_bike > 0 & U_ped > 0){
      f <- Q_bike_sm * (1 - U_ped / U_bike)
    }
    else {f <- 'Error : [U_bike], [U_ped] must be positive(kph). Please check that.'}
  }
  else {f <- 'Error : [Q_bike_sm] must be positive(person/h). Please check that.'}
  f
}
