#' Incremental Delay Indicating Effect of Random Arrival and Oversaturation when there is No Initial Waiting Vehicle at Signalized Intersection.
#'
#' Incremental delay indicating the effect of random arrival and oversaturation
#'     within the analysis period when there is no initial waiting vehicle at signalized intersection.
#'     It follows <Formula 8-50> in KHCM(2013), p.255.
#' @param c The capacity (vph) of the corresponding vehicle group during the analysis period
#' @param t Analysis period length (hours)
#' @param X The saturation of the lane group
#' @seealso \code{\link{d_si}}
#' @details
#'     Incremental delay includes random delay due to non-uniform arrival and overflow delay due to several cycle failures within the analysis period.
#'     Therefore, there is no remaining queue at the beginning and end of the analysis period.
#'     The incremental delay of a lane group largely depends on the saturation (X) of the lane group,
#'     the length of the analysis period (T), and the capacity (c) of the lane group.
#' @export d_2_si
#' @examples
#' d_2_si(c = 1800, t = 1.2, X = 0.556)
d_2_si <- function(c = NULL, t = NULL, X = NULL){
  if (c > 0){
    if (t > 0){
      if (X > 0){d <- 900 * t * ((X - 1) + ((X - 1)**2 + (4 * X / (c * t)))**(1/2))}
      else {d <- 'Error : [X] must be positive. Please check that.'}
    }
    else {d <- 'Error : [t] must be positive(hours). Please check that.'}
  }
  else {d <- 'Error : [c] must be positive(vph). Please check that.'}
  d
}
