#' Average Speed of Weaving Traffic Flow in Freeway Weaving Section(S_w, kph)
#'
#' This function calculates the average speed of weaving traffic flow in freeway weaving section.(kph)
#' @param S_D *Numeric* Design speed of the main lane(kph)
#' @param V *Numeric* Total traffic volume (pcph) of the weaving section
#' @param V_w *Numeric* Weaving traffic volume(pcph)
#' @param N *Numeric* Total number of lanes in the weaving section
#' @param N_main *Numeric* A Number of main lane in the weaving section
#' @param L *Numeric* Length of the weaving section(m)
#' @export S_w Average Speed of Weaving Traffic Flow in Freeway Weaving Section(kph)
#' @examples
#' S_w(S_D = 100, V = 1000, V_w = 300, N = 4, N_main = 3 L = 250)
#' S_w(120, 1200, 210, 3, 2, 300)
S_w <- function(S_D = NULL, V = NULL, V_w = NULL, N = NULL, L = NULL){
  if (S_D >= 0 & V > 0 & V_w > 0 & N >= 1 & L > 0){
    VR <- V_w/V
    if ((N == 3 & N_main == 2)|(N == 4 & N_main == 3)|(N == 5 & N_main == 4)){}
    W_w <- W_w(V = V, V_w = V_w, N = N, L = L)
    30 + (((S_D + 10) - 30)/(1 + W_w))
  }
}
