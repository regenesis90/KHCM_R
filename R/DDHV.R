#' Directional Design Hourly Volume(DDHV)
#'
#' This function calculate Directional Design Hourly Volume(DDHV).
#' @param AADT *Numeric* Average Annual Daily Traffic(pc/day)
#' @param region *Categorical* Choose one from : \code{'city'}, \code{'rural'}
#' @param road *Categorical* Choose one from : \code{'general'}, \code{'highway'}
#' @param lane *Numeric* The number of lane(round-trip). It should \code{2} or \code{4} or more.
#' @keywords DDHV DHV AADT Directional Design Hourly Factor
#' @export DDHV \code{AADT * K * D}. It means Directional Design Hourly Volume(DDHV, pc/h/bidirectional)
#' @examples
#' DDHV(AADT = 1500, region = 'city', road = 'general', lane = 2)
#' DDHV(3000, 'city', 'general', 8)
#' DDHV(1700, 'city', 'general', 6)
DDHV <- function(AADT = NULL, region = NULL, road = NULL, lane = NULL){
  if (AADT >= 0 & lane >= 2){
    if(region =='city'){
      D <- 0.6
      if(road == 'general'){
        if(lane == 2){K <- 0.12}
        if(lane >= 4){K <- 0.1}
      }
      if(road == 'highway'){K <- 0.1}
    }
    if(region == 'rural'){
      D <- 0.65
      if(road == 'general'){
        if(lane == 2){K <- 0.16}
        if(lane >= 4){K <- 0.12}
      }
      if(road == 'highway'){K <- 0.14}
    }
    AADT * K * D
  }
}
