#' Speed Correction Factor from Number of inlet and outlet points(F_A, kph)
#'
#' It follows <Table 6-5> in KHCM(2013).
#' @param n_inlet_outlet Number of inlet and outlet points.(ea/km)
#' @keywords inlet outlet point reduced speed
#' @export F_A Speed Correction Factor from Number of inlet and outlet points(F_A, kph)
#' @examples
#' F(n_inlet_outlet = 10)
#' F_A(8)
F_A <- function(n_inlet_outlet = 0){
  if (n_inlet_outlet == 0){result <- 0}
  if (n_inlet_outlet > 0 & n_inlet_outlet <= 4){result <- 1}
  if (n_inlet_outlet > 4 & n_inlet_outlet <= 8){result <- 2}
  if (n_inlet_outlet > 8 & n_inlet_outlet <= 12){result <- 3}
  if (n_inlet_outlet > 12 & n_inlet_outlet <= 16){result <- 4}
  if (n_inlet_outlet > 16 & n_inlet_outlet <= 20){result <- 6}
  if (n_inlet_outlet > 20){result <- 8}
  result
}
