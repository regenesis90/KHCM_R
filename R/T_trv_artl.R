#' Travel Time of Analysis Section on Arterial Road
#'
#' Travel time (seconds) in the analysis section of the exclusive median bus lane or general lane
#'     It follows <Formula 12-12> in KHCM(2013) p.552.
#' @param t_trv_km Travel time per km(sec/km). See \code{\link{t_trv_km_artl}}, \code{\link{t_trv_km_bus_artl}}
#' @param l Length of analysis section (km)
#' @param d Total access delay (seconds) at all intersections within the analysis target range as a part of the general lane
#' @keywords travel time arterial road
#' @seealso \code{\link{t_trv_km_artl}}, \code{\link{t_trv_km_bus_artl}}
#' @export T_trv_artl
#' @examples
#' T_trv_artl(t_trv_km = 136, l = 0.6, d = 54.3)
T_trv_artl <- function(t_trv_km = NULL, l = NULL, d = NULL){
  if (t_trv_km > 0){
    if (l > 0){
      if (d > 0){t <- t_trv_km * l + d}
      else {t <- 'Error : [d] must be positive(seconds). Please check that.'}
    }
    else {t <- 'Error : [l] must be positive(km). Please check that.'}
  }
  else {t <- 'Error : [t_trv_km] must be positive(sec/km). Please check that.'}
  t
}
