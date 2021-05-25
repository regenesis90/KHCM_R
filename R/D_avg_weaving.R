#' Average Density in Freeway Weaving Section(D_avg_weaving, pcpkmpl)
#'
#' This function calculates the average density in freeway weaving section.(pcpkmpl)
#' @param V *Numeric* Total traffic volume of the weaving section(pcph). Input values should be transformed to peak-hour's(including heavy vehicle's influence)
#' @param V_w *Numeric* Weaving traffic volume(pcph) Input values should be transformed to peak-hour's(including heavy vehicle's influence)
#' @param V_nw *Numeric* Non-weaving traffic volume(pcph) Input values should be transformed to peak-hour's(including heavy vehicle's influence)
#' @param S_w *Numeric* Average Speed of Weaving Traffic Flow in Freeway Weaving Section(S_w, kph)
#' @param S_nw *Numeric* Average Speed of Non-Weaving Traffic Flow in Freeway Weaving Section(S_nw, kph)
#' @param N *Numeric* Total number of lanes in the weaving section
#' @export D_avg_weaving Average speed in freeway weaving section.(pcpkmpl)
#' @examples
#' D_avg_weaving(V = 1000, V_w = 300, V_nw = 700, S_w = 80, S_nw = 75, N = 4)
#' D_avg_weaving(1100, 350, 750, 85, 80, 6)
D_avg_weaving <- function(V = NULL, V_w = NULL, V_nw = NULL, S_w = NULL, S_nw = NULL, N = NULL){
  if (V > 0 & V_w > 0 & V_nw > 0 & S_w > 0 & S_nw > 0 & N >= 2){
    S_avg <- S_avg_weaving(V = V, V_w = V_w, V_nw = V_nw, S_w = S_w, S_nw = S_nw)
    (V/N)/S_avg
  }
}
