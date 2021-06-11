#' Effective sidewalk width
#'
#' It follows <Formula 14-3>, <Formula 14-10> in KHCM(2013), p.620, p.625
#' @param W_T Actual Sidewalk Width(m)
#' @param W_O The width of the sidewalk obstructed by the facility(m)
#' @param V Future Demand Pedestrian Traffic Volume (person/min)
#' @param SV_i Service pedestrian traffic flow rate at service level i (person/min/m)
#' @keywords
#' @export W_E
#' @examples
W_E <- function(W_T = NULL, W_O = NULL, V = NULL, SV_i = NULL){
  if (is.null(W_T) == FALSE & is.null(W_O) == FALSE){w <- W_T - W_O}
  else{
    if (is.null(V) == FALSE & is.null(SV_i) == FALSE){w <- V / SV_i}
  }
  w
}
