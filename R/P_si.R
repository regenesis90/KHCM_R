#' Number of Possible Unprotected Left Turns Per Gap by Opposite Straight Traffic Volume at Signalized Intersection
#'
#' Number of possible unprotected left turns per gap by opposite straight traffic volume.(vph)
#'     This function follows <Table 8-8> in KHCM(2013), p.228.
#' @param V_O Opposite traffic volume(vph)
#' @keywords possible unprotected left turn opposite straight traffic volume signalized intersection
#' @seealso \code{\link{E_1_si}}
#' @export P_si Number of possible unprotected left turns per gap by opposite traffic volume
#' @examples
#' P_si(V_O = 1792)
P_si <- function(V_O = NULL){
  if (V_O >= 0 & V_O < 100){}
  else if (V_O == 100){p <- 14.1}
  else if (V_O > 100 & V_O < 200){p <- 14.1 - 7.75 * (V_O - 100) * 0.01}
  else if (V_O == 200){p <- 6.35}
  else if (V_O > 200 & V_O < 400){p <- 6.35 - 2.78 * (V_O - 200) * 0.005}
  else if (V_O == 400){p <- 2.57}
  else if (V_O > 400 & V_O < 600){p <- 2.57 - 1.18 * (V_O - 400) * 0.005}
  else if (V_O == 600){p <- 1.39}
  else if (V_O > 600 & V_O < 800){p <- 1.39 - 0.55 * (V_O - 600) * 0.005}
  else if (V_O == 800){p <- 0.84}
  else if (V_O > 800 & V_O < 1000){p <- 0.84 - 0.30 * (V_O - 800) * 0.005}
  else if (V_O == 1000){p <- 0.54}
  else if (V_O > 1000 & V_O < 1200){p <- 0.54 - 0.17 * (V_O - 1000) * 00.005}
  else if (V_O == 1200){p <- 0.37}
  else if (V_O > 1200 & V_O < 1400){p <- 0.37 - 0.12 * (V_O - 1200) * 0.005}
  else if (V_O == 1400){p <- 0.25}
  else if (V_O > 1400 & V_O < 1600){p <- 0.25 - 0.07 * (V_O - 1400) * 0.005}
  else if (V_O == 1600){p <- 0.18}
  else if (V_O > 1600 & V_O < 1800){p <- 0.18 - 0.05 * (V_O - 1600) * 0.005}
  else if (V_O == 1800){p <- 0.13}
  else if (V_O >1800){p <- 0.18 - 0.05 * (V_O - 1600) * 0.01}
  else {p <- 'Error : [V_O] must be positive(vph). Please check that.'}
  p
}
