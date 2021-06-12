#' Design Hour Volume(DHV)
#'
#' Design Hourly Volume (DHV) is one of the hourly traffic volumes for design.
#'     It is obtained by multiplying the Average Annual Daily Traffic (AADT) of the planned target year by the Design Hourly Factor (K).
#'     It follows a formula in KHCM(2013) p.6
#' @param AADT *Numeric* Average Annual Daily Traffic(pc/day, vph)
#' @param region *Categorical* Region type classification. Choose one from : \code{'urban'}, \code{'local'}, \code{'tourist_area'}
#' @param road *Categorical* Road type. Choose one from : \code{'general'}(General national road), \code{'expressway'}(Expressway with more than 4 lanes)
#' @param lane *Numeric* The number of round-trip lanes.. It should \code{2} or \code{4} or more.
#' @param output *Categorical* Type of result value. \code{'max'}, \code{'min'}, \code{'mean'}
#' @keywords DHV AADT Design Hour Factor
#' @export DHV \code{AADT * K}. It means Design Hourly Volume(DHV, veh/h/bidirectional)
#' @examples
#' DHV(AADT = 2000, region = 'urban', road = 'expressway', lane = 8, output = 'max')
#' DHV(3000, 'urban', 'general', 6, 'mean')
DHV <- function(AADT = NULL, region = NULL, road = NULL, lane = NULL, output = NULL){
  k <- K(region = region, road = road, lane = lane, output = output)
  if (is.numeric(k) == TRUE){
    if (is.numeric(AADT) == TRUE){
      dhv <- AADT * k
    }
    else{dhv <- 'Error : AADT is not numeric type. Please check AADT.'}
  }
  else{dhv <- 'Error : Please check the argument to get K.'}
  dhv
}
