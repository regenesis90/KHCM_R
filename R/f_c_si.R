#' Percentage of Crosswalk Signal Time for which a Right Turn is Not Available at Signalized Intersection
#'
#' Percentage of crosswalk signal times that are not available to vehicles turning right at a signalized intersection.
#'     This function follows <Table 8-13> in KHCM(2013), p.233.
#' @param N_ped Number of pedestrians crossing (in both directions, person/hour)
#' @keywords crosswalk signal time right turn signalized intersection
#' @seealso \code{\link{E_R1_si}}
#' @export f_c Percentage of crosswalk signal time for which a right turn is not available (f_c)
#' @examples
#' f_c_si(N_ped = 2883)
f_c_si <- function(N_ped = NULL){
  if (N_ped >= 0 & N_ped <= 500){f <- 0.3}
  else if (N_ped > 500 & N_ped <= 1000){f <- 0.6}
  else if (N_ped > 1000 & N_ped <= 2000){f <- 0.8}
  else if (N_ped > 2000 & N_ped <= 3000){f <- 0.9}
  else if (N_ped > 3000){f <- 1.0}
  else {f <- 'Error : [N_ped] must be positive integer(person/hour). Please check that.'}
  f
}
