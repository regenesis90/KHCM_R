#' Directional Design Hourly Volume(DDHV)
#'
#' DDHV is reflected by multiplying the Directional Distribution Factor(D)
#'     by the Design Hourly Volume (DHV) when there is a clear difference in the distribution of traffic volume by direction, such as during peak hours.
#'     It follows a formula in KHCM(2013), p.7
#' @param AADT *Numeric* Average Annual Daily Traffic(pc/day, vph)
#' @param region *Categorical* Region type classification. Choose one from : \code{'urban'}, \code{'local'}
#' @param road *Categorical* Road type. Choose one from : \code{'general'}(General national road), \code{'expressway'}(Expressway with more than 4 lanes)
#' @param lane *Numeric* The number of round-trip lanes.. It should \code{2} or \code{4} or more.
#' @param output_d *Categorical* Type of D_coe() result value. \code{'max'}, \code{'min'}, \code{'mean'}
#' @param output_k *Categorical* Type of K() result value. \code{'max'}, \code{'min'}, \code{'mean'}
#' @keywords DDHV DHV AADT Directional Design Hourly Factor
#' @export DDHV \code{AADT * K * D}. It means Directional Design Hourly Volume(DDHV, veh/h/direction)
#' @examples
#' DDHV(AADT = 1200, region = 'urban', road = 'expressway', lane = 6, output_d = 'mean', output_k = 'mean')
#' DDHV(3000, 'local', 'general', 4, 'mean', 'mean')
DDHV <- function(AADT = NULL, region = NULL, road = NULL, lane = NULL, output_d = NULL, output_k = NULL){
  k <- K(region = region, road = road, lane = lane, output = output_k)
  if (is.numeric(k) == TRUE){
    if (is.numeric(AADT) == TRUE){
      dhv <- AADT * k
      d <- D_coe(region = region, output = output_d)
      if (is.numeric(d) == TRUE){
        result <- dhv * d
      }
      else{result <- 'Error : D_coe is not numeric type. Please check the arguments.'}
    }
    else{result <- 'Error : AADT is not numeric type. Please check AADT.'}
  }
  else{result <- 'Error : Please check the argument to get K.'}
  result
}
