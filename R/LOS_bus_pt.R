#' In-vehicle Service Level of Bus
#'
#' In-vehicle level of service(LOS) for seated or standing buses.
#'     - 0.5/seat is service level A, one passenger per seat is service level C.
#'     - In the case of standing space, the space standard of service level E of the waiting area among pedestrian facilities was applied to the space including a part of the seat space available to standing passengers.
#'     - It follows <Table 13-3>, <Table 13-4> in KHCM(2013), p.593-594.
#' @param type Bus type. Choose one from : \code{'seat'}, \code{'standing'}
#' @param type2 If \code{type == 'standing'}, then Choose type2 from : \code{'city'}, \code{'circulation'}
#' @param n_psg Number of passenger
#' @param area Area occupied per passenger(㎡/person)
#' @seealso
#' @details
#'     - Seated bus : Based on vehicle area of 23.30㎡, standing area of 3.77㎡, 45 seats
#'     - Standing Bus :
#'         - City bus : Based on vehicle area of 26.37㎡, standing area of 7.5㎡, 31 seats
#'         - Circulation bus : Based on vehicle area of 16.05㎡, standing area of 3.48㎡, 24 seats
#' @export LOS_bus_pt \code{'A'}, \code{'B'}, \code{'C'}, \code{'D'}, \code{'E'}, \code{'F'}
#' @examples
#' LOS_bus_pt(type = 'seat', n_psg = 30)
#' LOS_bus_pt(type = 'standing', type2 = 'city', area = 0.33)
LOS_bus_pt <- function(type = NULL, type2 = NULL, n_psg = NULL, area = NULL){
  if (type == 'seat'){
    if (is.null(n_psg) == FALSE){
      if (n_psg >= 0 & n_psg <= 22){los <- 'A'}
      else if (n_psg > 22 & n_psg <= 34){los <- 'B'}
      else if (n_psg > 34 & n_psg <= 45){los <- 'C'}
      else if (n_psg > 45 & n_psg <= 57){los <- 'D'}
      else if (n_psg > 57 & n_psg <= 70){los <- 'E'}
      else if (n_psg > 70){los <- 'F'}
      else {los <- 'Error : [n_psg] must be positive(persons). Please check that.'}
    }
    else{
      if (area > 1.05){los <- 'A'}
      else if (area > 0.68 & area <= 1.05){los <- 'B'}
      else if (area > 0.51 & area <= 0.68){los <- 'C'}
      else if (area > 0.40 & area <= 0.51){los <- 'D'}
      else if (area > 0.33 & area <= 0.40){los <- 'E'}
      else if (area <= 0.33 & area > 0){los <- 'F'}
      else {los <- 'Error : [area] must be positive(㎡/person). Please check that.'}
    }
  }
  else if (type == 'standing'){
    if (type2 == 'city'){
      if (is.null(n_psg) == FALSE){
        if (n_psg >= 0 & n_psg <= 15){los <- 'A'}
        else if (n_psg > 15 & n_psg <= 31){los <- 'B'}
        else if (n_psg > 31 & n_psg <= 40){los <- 'C'}
        else if (n_psg > 40 & n_psg <= 50){los <- 'D'}
        else if (n_psg > 50 & n_psg <= 62){los <- 'E'}
        else if (n_psg > 62){los <- 'F'}
        else {los <- 'Error : [n_psg] must be positive(persons). Please check that.'}
      }
      else{
        if (area > 1.70){los <- 'A'}
        else if (area > 0.84 & area <= 1.70){los <- 'B'}
        else if (area > 0.65 & area <= 0.84){los <- 'C'}
        else if (area > 0.52 & area <= 0.65){los <- 'D'}
        else if (area > 0.43 & area <= 0.52){los <- 'E'}
        else if (area <= 0.43 & area > 0){los <- 'F'}
        else {los <- 'Error : [area] must be positive(㎡/person). Please check that.'}
      }
    }
    else if (type2 == 'circular'){
      if (is.null(n_psg) == FALSE){
        if (n_psg >= 0 & n_psg <= 12){los <- 'A'}
        else if (n_psg > 12 & n_psg <= 24){los <- 'B'}
        else if (n_psg > 24 & n_psg <= 31){los <- 'C'}
        else if (n_psg > 31 & n_psg <= 38){los <- 'D'}
        else if (n_psg > 38 & n_psg <= 48){los <- 'E'}
        else if (n_psg > 48){los <- 'F'}
        else {los <- 'Error : [n_psg] must be positive(persons). Please check that.'}
      }
      else{
        if (area > 1.33){los <- 'A'}
        else if (area > 0.66 & area <= 1.33){los <- 'B'}
        else if (area > 0.52 & area <= 0.66){los <- 'C'}
        else if (area > 0.41 & area <= 0.52){los <- 'D'}
        else if (area > 0.33 & area <= 0.41){los <- 'E'}
        else if (area <= 0.33 & area > 0){los <- 'F'}
        else {los <- 'Error : [area] must be positive(㎡/person). Please check that.'}
      }
    }
    else {los <- 'Error : [type] must be one of [city] or [circular]. Please check that.'}
  }
  los
}
