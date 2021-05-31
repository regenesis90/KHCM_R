#' Number of possible unprotected left turns per gap by opposite traffic volume(P_signalized_intersection)
#'
#' This function follows <Table 8-8> in KHCM(2013)
#' @param V_o *Numeric* Opposite traffic volume(vph)
#' @keywords
#' @export P_signalized_intersection Number of possible unprotected left turns per gap by opposite traffic volume
#' @examples
#' P_signalized_intersection(V_o = 1792)
#' P_signalized_intersection(993)
P_signalized_intersection <- function(V_o = NULL){
  if (V_o >= 0 & V_o < 100){}
  if (V_o == 100){p <- 14.1}
  if (V_o > 100 & V_o < 200){p <- 14.1 - 7.75 * (V_o - 100) * 0.01}
  if (V_o == 200){p <- 6.35}
  if (V_o > 200 & V_o < 400){p <- 6.35 - 2.78 * (V_o - 200) * 0.005}
  if (V_o == 400){p <- 2.57}
  if (V_o > 400 & V_o < 600){p <- 2.57 - 1.18 * (V_o - 400) * 0.005}
  if (V_o == 600){p <- 1.39}
  if (V_o > 600 & V_o < 800){p <- 1.39 - 0.55 * (V_o - 600) * 0.005}
  if (V_o == 800){p <- 0.84}
  if (V_o > 800 & V_o < 1000){p <- 0.84 - 0.30 * (V_o - 800) * 0.005}
  if (V_o == 1000){p <- 0.54}
  if (V_o > 1000 & V_o < 1200){p <- 0.54 - 0.17 * (V_o - 1000) * 00.005}
  if (V_o == 1200){p <- 0.37}
  if (V_o > 1200 & V_o < 1400){p <- 0.37 - 0.12 * (V_o - 1200) * 0.005}
  if (V_o == 1400){p <- 0.25}
  if (V_o > 1400 & V_o < 1600){p <- 0.25 - 0.07 * (V_o - 1400) * 0.005}
  if (V_o == 1600){p <- 0.18}
  if (V_o > 1600 & V_o < 1800){p <- 0.18 - 0.05 * (V_o - 1600) * 0.005}
  if (V_o == 1800){p <- 0.13}
  if (V_o >1800){p <- 0.18 - 0.05 * (V_o - 1600) * 0.01}
  p
}
