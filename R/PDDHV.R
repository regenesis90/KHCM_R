#' Peak Directional Design Hourly Volume(PDDHV, vph)
#'
#' This function calculates the Peak Directional Design Hourly Volume(PDDHV, vph) for a given road and traffic condition.
#' @param AADT *Numeric* Average Annual Daily Traffic(pc/day, vph)
#' @param region *Categorical* Choose one from : \code{'city'}, \code{'rural'}
#' @param road *Categorical* Choose one from : \code{'general'}, \code{'highway'}
#' @param lane *Numeric* The number of lane. It should \code{2} or \code{4} or more.
#' @param PHF
#' @keywords PDDHF DDHV DHV AADT Directional Design Hourly Factor PHF
#' @export DDHV \code{AADT * K * D}. It means Directional Design Hourly Volume(DDHV, pc/h/bidirectional)
#' @examples
#' PDDHV(AADT = 1000, region = 'city', road = 'general', lane = 6, PHF = 0.98)
#' PDDHV(1500, 'rural', 'highway', 8, 0.95)
PDDHV <- function(AADT = NULL, region = NULL, road = NULL, lane = NULL, PHF = NULL){
  if (AADT >= 0 & PHF >= 0){
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
    AADT * K * D / PHF
  }
}
