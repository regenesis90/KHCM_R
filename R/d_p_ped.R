#' Average Pedestrian Delay
#'
#' Average pedestrian delay(seconds).
#'     It is used when analyzing the service level at signal crosswalks.
#'     It follows <Formula 14-5> in KHCM(2013), p.623.
#' @param C Signal period(seconds)
#' @param g Effective green time for pedestrians (seconds)
#' @keywords average pedestrian delay
#' @seealso \code{\link{LOS_cross_ped}}
#' @export d_p_ped Average pedestrian delay(seconds)
#' @examples
#' d_p_ped(C = 120, g = 30)
d_p_ped <- function(C = NULL, g = NULL){
  if (C > 0){
    if (g > 0){
      d <- (C - g)**2 / (2 * C)
    }
    else {d <- 'Error : [g] must be positive(seconds). Please check that.'}
  }
  else {d <- 'Error : [C] must be positive(seconds). Please check that'}
  d
}
