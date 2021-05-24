#' Average Speed of Non-Weaving Traffic Flow in Freeway Weaving Section(S_nw, kph)
#'
#' This function calculates the average speed of non-weaving traffic flow in freeway weaving section.(kph)
#' @param S_D *Numeric* Design speed of the main lane(kph)
#' @param V *Numeric* Total traffic volume (pcph) of the weaving section
#' @param V_w *Numeric* Weaving traffic volume(pcph)
#' @param N *Numeric* Total number of lanes in the weaving section
#' @param L *Numeric* Length of the weaving section(m)
#' @export S_nw Average speed of non-weaving traffic flow in freeway weaving section(kph)
#' @examples
#' S_nw(S_D = 100, V = 1000, V_w = 300, N = 1, L = 250)
#' S_nw(120, 1200, 210, 1, 300)
S_nw <- function(S_D = NULL, V = NULL, V_w = NULL, N = NULL, L = NULL){
  if (S_D >= 0 & V > 0 & V_w > 0 & N >= 1 & L > 0){
    W_nw <- W_nw(V = V, V_w = V_w, N = N, L = L)
    30 + (((S_D + 10) - 30)/(1 + W_nw))
  }
}
