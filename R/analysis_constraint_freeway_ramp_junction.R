#' Analysis Constraint of Ramp Freeway Junction
#'
#' It follows <Table 4-2> in KHCM(2013). The suggested capacity value indicates whether or not to proceed with the service level analysis procedure of the connection. Used to judge. If at least one of the points where the capacity value presented in the table exceeds the capacity value, the service level analysis procedure is not performed. When the dose values at the points indicated in the presented table are not exceeded, proceed with the analysis procedure. When all values are not exceeded, the analysis procedure is performed.
#' @param free_speed *Numeric* Free speed in connecting road(kph)
#' @param N *Categorical* The number of one-way lane of connecting road. Choose one from : \code{1}, \code{2}
#' @param C *Numeric* Capacity of the connecting road(pcph)
#' @export Analysis_availability_freeway_ramp_junction \code{'Possible'}, \code{'Impossible'}
#' @examples
#' Analysis_availability_freeway_ramp_junction(free_speed = 80, N = 2, C = 4000)
#' Analysis_availability_freeway_ramp_junction(50, 1, 3000)
Analysis_availability_freeway_ramp_junction <- function(free_speed = NULL, N = NULL, C = NULL){
  if (N == 1){
    if (free_speed > 70){
      if (C > 0 & C <= 2000){result <- 'Possible'}
      else {result <- 'Impossible'}
      }
    if (free_speed <= 70 & free_speed > 60){
      if (C > 0 & C <= 1900){result <- 'Possible'}
      else {result <- 'Impossible'}
    }
    if (free_speed <= 60 & free_speed > 50){
      if (C > 0 & C <= 1800){result <- 'Possible'}
      else {result <- 'Impossible'}
    }
    if (free_speed <= 50 & free_speed > 40){
      if (C > 0 & C <= 1700){result <- 'Possible'}
      else {result <- 'Impossible'}
    }
    if (free_speed <= 40){
      if (C > 0 & C <= 1600){result <- 'Possible'}
      else {result <- 'Impossible'}
    }
  }
  if (N == 2){
    if (free_speed > 70){
      if (C > 0 & C <= 4000){result <- 'Possible'}
      else {result <- 'Impossible'}
    }
    if (free_speed <= 70 & free_speed > 60){
      if (C > 0 & C <= 3800){result <- 'Possible'}
      else {result <- 'Impossible'}
    }
    if (free_speed <= 60 & free_speed > 50){
      if (C > 0 & C <= 3600){result <- 'Possible'}
      else {result <- 'Impossible'}
    }
    if (free_speed <= 50 & free_speed > 40){
      if (C > 0 & C <= 3400){result <- 'Possible'}
      else {result <- 'Impossible'}
    }
    if (free_speed <= 40){
      if (C > 0 & C <= 3200){result <- 'Possible'}
      else {result <- 'Impossible'}
    }
  }
  result
}
