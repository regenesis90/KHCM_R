#' In-vehicle service level of seat-type bus(LOS_bus_seat)
#'
#' It follows <Table 13-3> in KHCM(2013), p.593.
#'     It based on the vehicle area of 23.30㎡, standing area of 3.77㎡, 45 seats.
#' @param n_psg Number of passenger
#' @param psg_by_seat Number of occupants per seat
#' @param area_psg Area occupied per passenger
#' @export LOS_seat_bus \code{'A'}, \code{'B'}, \code{'C'}, \code{'D'}, \code{'E'}, \code{'F'}
#' @examples
#' LOS_bus_seat(n_psg = 30)
#' LOS_bus_seat(psg_by_seat = 0.98)
#' LOS_bus_seat(area_psg = 0.33)
LOS_bus_seat <- function(n_psg = NULL, psg_by_seat = NULL, area_psg = NULL){
  if (is.null(n_psg) == FALSE){
    if (n_psg >= 0 & n_psg <= 22){los <- 'A'}
    if (n_psg > 22 & n_psg <= 34){los <- 'B'}
    if (n_psg > 34 & n_psg <= 45){los <- 'C'}
    if (n_psg > 45 & n_psg <= 57){los <- 'D'}
    if (n_psg > 57 & n_psg <= 70){los <- 'E'}
    if (n_psg > 70){los <- 'F'}
  }
  else{
    if (is.null(psg_by_seat) == FALSE){
      if (psg_by_seat >= 0 & psg_by_seat <= 0.50){los <- 'A'}
      if (psg_by_seat > 0.5 & psg_by_seat <= 0.75){los <- 'B'}
      if (psg_by_seat > 0.75 & psg_by_seat <= 1.00){los <- 'C'}
      if (psg_by_seat >= 1.00 & psg_by_seat <= 1.20){los <- 'D'}
      if (psg_by_seat >= 1.20 & psg_by_seat <= 1.37){los <- 'E'}
      if (psg_by_seat > 1.37){los <- 'F'}
    }
    else{
      if (area_psg > 1.05){los <- 'A'}
      if (area_psg > 0.68 & area_psg <= 1.05){los <- 'B'}
      if (area_psg > 0.51 & area_psg <= 0.68){los <- 'C'}
      if (area_psg > 0.40 & area_psg <= 0.51){los <- 'D'}
      if (area_psg > 0.33 & area_psg <= 0.40){los <- 'E'}
      if (area_psg <= 0.33){los <- 'F'}
    }
  }
  los
}
