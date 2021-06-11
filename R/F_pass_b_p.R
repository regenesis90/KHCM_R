#' Number of times a bicycle overtakes a pedestrian walking in the same direction (times/h)
#'
#' It follows <Formula 15-6> in KHCM(2013), p.645
#' @param Q_ped_sm Pedestrian traffic flow rate in the same direction
#' @param U_bike Average bike speed (kph)
#' @param U_ped Average walking speed (kph)
#' @keywords
#' @export F_pass_b_p Number of times a bicycle overtakes a pedestrian walking in the same direction (times/h)
#' @examples
F_pass_b_p <- function(Q_ped_sm = NULL, U_bike = NULL, U_ped = NULL){
  Q_ped_sm * (U_bike / U_ped - 1)
}
