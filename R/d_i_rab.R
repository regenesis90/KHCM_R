#' Average Delay per Vehicle at Roundabout
#'
#' Average delay per vehicle at roundabout(sec/veh).
#'     It is necessary to judge the roundabout operation efficiency.
#'     It follows <Formula 11-9> in KHCM(2013), p.503.
#' @param v Traffic volume(vph)
#' @param c Capacity(vph). See \code{\link{capa_rab}}
#' @param t Analysis time(1t = 15 minutes = 0.25h)
#' @keywords average delay roundabout
#' @seealso \code{\link{d_total_rab}}, \code{\link{capa_rab}}
#' @export d_i_rab Average control delay per vehicle
#' @examples
#' d_i_rab(v = 1234, c = 1800, t = 3)
d_i_rab <- function(v = NULL, c = NULL, t = NULL){
  if (v >= 0 & c > 0){
    if (t > 0){
      if (v/c >= 1){
        d <- 3600/c + 900 * t * ((v/c - 1) + ((v/c - 1)**2 + (3600/c) * (v/c) / (450 * t))**(1/2)) + 5 * v/c
      }
      else {
        d <- 3600/c + 900 * t * ((v/c - 1) + ((v/c - 1)**2 + (3600/c) * (v/c) / (450 * t))**(1/2)) + 5 * 1
      }
    }
    else {d <- 'Error : [t] must be positive. Please check that.'}
  }
  else {d <- 'Error : [v], [c] must be positive(vph). Please check that.'}
  d
}
