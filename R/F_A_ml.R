#' Speed Correction Factor from Number of inflow & outflow Points
#'
#' Number of outflow points velocity correction factor (kph).
#'     It represents the degree of the maximum speed reduction (kph) according to the number of inflow and outflow points.
#'     The number of one-way entry and exit points installed within a unit section of a multi-lane road also affects the speed of traffic.
#'     It follows <Table 6-5> in KHCM(2013), p.142.
#' @param n_pnt Number of inflow and outflow points.(ea/km)
#' @keywords inflow outflow point reduced speed
#' @details This number of inflow and outflow points includes:
#'     - Inflow and outflow at a level that allows access to the main line only by turning right
#'     - Left turn access road with low traffic (50 cars/day) without traffic lights
#' @export F_A_ml Speed Correction Factor from Number of inflow and outflow points(F_A, kph)
#' @examples
#' F_A_ml(n_pnt = 10)
#' F_A_ml(8)
F_A_ml <- function(n_pnt = 0){
  if (n_pnt == 0){result <- 0}
  else if (n_pnt > 0 & n_pnt <= 4){result <- 1}
  else if (n_pnt > 4 & n_pnt <= 8){result <- 2}
  else if (n_pnt > 8 & n_pnt <= 12){result <- 3}
  else if (n_pnt > 12 & n_pnt <= 16){result <- 4}
  else if (n_pnt > 16 & n_pnt <= 20){result <- 6}
  else if (n_pnt > 20){result <- 8}
  else {result <- 'Error : [n_pnt] must be >= 0. Please check that.'}
  result
}
