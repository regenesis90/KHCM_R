#' Loss Time Per Hour due to Entry and Exit of the Backside Road at the Signalized Intersection
#'
#' Loss time per hour due to entry and exit of the backside road at the signalized intersection.
#'     Interference time (seconds) of vehicles entering and exiting at signal intersections.
#'     This function follows <Formula 8-7> in KHCM(2013), p.230.
#' @param V_en Traffic entering the arterial road (vph)
#' @param V_ex Traffic leaving the arterial road (vph)
#' @keywords loss time entry exit backside road signalized intersection
#' @seealso \code{\link{L_H_si}}
#' @details
#'     * There are often entrances and exits on the back road near the stop line at the signal intersection.
#'     * Vehicles entering and exiting through these roads impede the flow of main road traffic.
#'     * Therefore, as the traffic volume increases, the saturation traffic flow rate of the main line decreases.
#'     * This interruption time is an incremental time compared to the saturation head time of the right turn,
#'     and it is considered that there is no effect on entry and exit routes of 60m or more from the stop line.
#' @export L_dw_si Loss time per hour due to in and out of the back road (seconds)
#' @examples
#' L_dw_si(V_en = 800, V_ex = 732)
L_dw_si <- function(V_en = NULL, V_ex = NULL){
  if (V_en >= 0 & V_ex >= 0){l <- 0.9 * V_en + 1.4 * V_ex}
  else {l <- 'Error : [V_en], [V_ex] must be positive(vph). Please check that.'}
  l
}
