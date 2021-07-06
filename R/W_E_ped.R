#' Effective Pedestrian Sidewalk Width
#'
#' Effective sidewalk width on pedestrian roads.
#'     The actual sidewalk width is subtracted from the sidewalk width obstructed by the facility.
#'     It follows <Formula 14-3> in KHCM(2013), p.620.
#' @param W_T Actual sidewalk width(m)
#' @param W_O The width of the sidewalk obstructed by the facility(m). See \code{\link{W_O_ped}}
#' @keywords Effective Sidewalk Width pedestrian
#' @seealso \code{\link{W_O_ped}}
#' @export W_E_ped
#' @examples
#' W_E_ped(3.5, 1.1)
W_E_ped <- function(W_T = NULL, W_O = NULL){
  if (W_T > 0){
    if (W_O >= 0){
      w <- W_T - W_O
    }
    else {w <- 'Error : [W_O] must be >= 0(m). Please check that.'}
  }
  else {w <- 'Error : [W_T] must be positive(m). Please check that.'}
  w
}
