#' In-vehicle service level of standing-type bus(LOS_standing_bus)
#'
#' It follows <Table 13-4> in KHCM(2013), p.594.
#'     It based on following standard.
#'     * City Bus : vehicle area of 26.37㎡, standing area of 7.5㎡, 31 seats.
#'     * Circular Bus : vehicle area of 16.05㎡, standing area of 3.48㎡, 24 seats.
#' @param type Choose one from: \code{'city'}, \code{'circular'}
#' @param n_psg Number of passenger
#' @param psg_by_seat Number of occupants per seat
#' @param area_psg Area occupied per passenger
#' @export LOS_bus_standing \code{'A'}, \code{'B'}, \code{'C'}, \code{'D'}, \code{'E'}, \code{'F'}
#' @examples
#' LOS_bus_standing('city', 30)
#' LOS_bus_standing(type = 'circular', psg_by_seat = 1.334)
#' LOS_bus_standing(type = 'city', area_psg = 2.31)
LOS_bus_standing <- function(type = NULL, n_psg = NULL, psg_by_seat = NULL, area_psg = NULL){
  if (type == 'city'){
    if (is.null(n_psg) == FALSE){
      if (n_psg >= 0 & n_psg <= 15){los <- 'A'}
      if (n_psg > 15 & n_psg <= 31){los <- 'B'}
      if (n_psg > 31 & n_psg <= 40){los <- 'C'}
      if (n_psg > 40 & n_psg <= 50){los <- 'D'}
      if (n_psg > 50 & n_psg <= 62){los <- 'E'}
      if (n_psg > 62){los <- 'F'}
    }
    else{
      if (is.null(psg_by_seat) == FALSE){
        if (psg_by_seat >= 0 & psg_by_seat <= 0.50){los <- 'A'}
        if (psg_by_seat > 0.50 & psg_by_seat <= 1.00){los <- 'B'}
        if (psg_by_seat > 1.00 & psg_by_seat <= 1.30){los <- 'C'}
        if (psg_by_seat >= 1.30 & psg_by_seat <= 1.60){los <- 'D'}
        if (psg_by_seat >= 1.60 & psg_by_seat <= 2.00){los <- 'E'}
        if (psg_by_seat > 2.00){los <- 'F'}
      }
      else{
        if (area_psg > 1.70){los <- 'A'}
        if (area_psg > 0.84 & area_psg <= 1.70){los <- 'B'}
        if (area_psg > 0.65 & area_psg <= 0.84){los <- 'C'}
        if (area_psg > 0.52 & area_psg <= 0.65){los <- 'D'}
        if (area_psg > 0.43 & area_psg <= 0.52){los <- 'E'}
        if (area_psg <= 0.43){los <- 'F'}
      }
    }
  }
  if (type == 'circular'){
    if (is.null(n_psg) == FALSE){
      if (n_psg >= 0 & n_psg <= 12){los <- 'A'}
      if (n_psg > 12 & n_psg <= 24){los <- 'B'}
      if (n_psg > 24 & n_psg <= 31){los <- 'C'}
      if (n_psg > 31 & n_psg <= 38){los <- 'D'}
      if (n_psg > 38 & n_psg <= 48){los <- 'E'}
      if (n_psg > 48){los <- 'F'}
    }
    else{
      if (is.null(psg_by_seat) == FALSE){
        if (psg_by_seat >= 0 & psg_by_seat <= 0.50){los <- 'A'}
        if (psg_by_seat > 0.50 & psg_by_seat <= 1.00){los <- 'B'}
        if (psg_by_seat > 1.00 & psg_by_seat <= 1.30){los <- 'C'}
        if (psg_by_seat >= 1.30 & psg_by_seat <= 1.60){los <- 'D'}
        if (psg_by_seat >= 1.60 & psg_by_seat <= 2.00){los <- 'E'}
        if (psg_by_seat > 2.00){los <- 'F'}
      }
      else{
        if (area_psg > 1.33){los <- 'A'}
        if (area_psg > 0.66 & area_psg <= 1.33){los <- 'B'}
        if (area_psg > 0.52 & area_psg <= 0.66){los <- 'C'}
        if (area_psg > 0.41 & area_psg <= 0.52){los <- 'D'}
        if (area_psg > 0.33 & area_psg <= 0.41){los <- 'E'}
        if (area_psg <= 0.33){los <- 'F'}
      }
    }
  }
  los
}
