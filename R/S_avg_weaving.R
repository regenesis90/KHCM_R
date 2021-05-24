#' Average Speed in Freeway Weaving Section(S_avg_weaving, kph)
#'
#' This function calculates the average speed in freeway weaving section.(kph)
#' @param V *Numeric* Total traffic volume of the weaving section(pcph)
#' @param V_w *Numeric* Weaving traffic volume(pcph)
#' @param V_nw *Numeric* Non-weaving traffic volume(pcph)
#' @param S_w *Numeric* Average Speed of Weaving Traffic Flow in Freeway Weaving Section(S_w, kph)
#' @param S_nw *Numeric* Average Speed of Non-Weaving Traffic Flow in Freeway Weaving Section(S_nw, kph)
#' @export S_avg_weaving Average speed in freeway weaving section.(kph)
#' @examples
#' S_avg_weaving(V = 1000, V_w = 300, V_nw = 700, S_w = 80, S_nw = 75)
#' S_avg_weaving(1100, 350, 750, 85, 80)
S_avg_weaving <- function(V = NULL, V_w = NULL, V_nw = NULL, S_w = NULL, S_nw = NULL){
  if (V > 0 & V_w > 0 & V_nw > 0 & S_w > 0 & S_nw > 0){
    V/((V_w/S_w) + (V_nw/S_nw))
  }
}
