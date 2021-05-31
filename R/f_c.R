#' Percentage of crosswalk signal time for which a right turn is not available (f_c)
#'
#' This function follows <Table 8-13> in KHCM(2013)
#' @param N_ped Number of pedestrians crossing (in both directions, person/hour)
#' @keywords
#' @export f_c Percentage of crosswalk signal time for which a right turn is not available (f_c)
#' @examples
#' f_c(2883)
f_c <- function(N_ped = NULL){
  if (N_ped >= 0 & N_ped <= 500){f <- 0.3}
  if (N_ped > 500 & N_ped <= 1000){f <- 0.6}
  if (N_ped > 1000 & N_ped <= 2000){f <- 0.8}
  if (N_ped > 2000 & N_ped <= 3000){f <- 0.9}
  if (N_ped > 3000){f <- 1.0}
  f
}
