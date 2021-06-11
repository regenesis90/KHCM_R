#' Space-time area (m^2/person) at a crosswalk at a traffic light intersection
#'
#' It follows <Formula 14-7> in KHCM(2013), p.624
#' @param L Crosswalk length (m)
#' @param S_p Average speed of pedestrians (m/s)
#' @param WALK_FDW Effective pedestrian green time at crosswalks (seconds)
#' @param W_E Effective crosswalk width(m). See W_E()
#' @keywords
#' @export TS
#' @examples
TS <- function(L = NULL, W_E = NULL, S_p = NULL, WALK_FDW = NULL){
  L * W_E * (WALK_FDW - (L / (2 * S_p)))
}
