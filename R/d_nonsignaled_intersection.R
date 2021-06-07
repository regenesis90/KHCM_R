#' Operation delay at non-signaled intersections(d_nonsignaled_intersection, seconds/unit)
#'
#' This function follows <Formula 10-5> in KHCM(2013) p.470
#' @param V_x Traffic flow rate for moving flow x (vph)
#' @param c_m_x Capacity of flow x (vph)
#' @param T Analysis time period (hours) (T = 0.25 means 15 minutes analysis time)
#' @keywords
#' @export d_nonsignaled_intersection
#' @examples
d_nonsignaled_intersection <- function(V_x = NULL, c_m_x = NULL, T = NULL){
  3600/c_m_x + 900 * T * ((V_x/c_m_x) - 1 + (((V_x/c_m_x) - 1)**2 + (3600/c_m_x)*(V_x/c_m_x)/(450 * T))**(1/2)) + 5
}
