#' Total Delay Rate in Section i of Section Type2 in 2-lane Road
#'
#' Total delay rate (%) in section i of section type 2 of the two-lane road
#'    It follows <Formula 7-10> in KHCM(2013), p.175.
#' @param FFS Free speed(kph)
#' @param L_2_i Section i length (km) of section type 2 (signal intersection influence zone)
#' @param d Average control delay per vehicle in 2-lane Road(seconds/veh). See \code{\link{d_2l}}
#' @keywords TDR Total Delay Rate
#' @export TDR_2_i_2l Total delay rate (%) in section i of section type 2 of the two-lane road
#' @seealso \code{\link{TDR_2l}}, \code{\link{TDR_thr_2l}}, \code{\link{TDR_2_i_2l}}, \code{\link{d_2l}}
#' @examples
#' TDR_2_i_2l(FFS = 87, L_2_i = 3.2, d = 3.2)
TDR_2_i_2l <- function(FFS = NULL, L_2_i = NULL, d = NULL){
  if (FFS >= 0){
    if (L_2_i >= 0){
      if (d >= 0){
        tdr <- d / ((3.6 * L_2_i)/FFS)
      }
      else {tdr <- 'Error : [d] must be positive(seconds/veh). Please check that.'}
    }
    else {tdr <- 'Error : [L_2_i] must be positive(m). Please check that.'}
  }
  else {tdr <- 'Error : [FFS] must be positive(kph). Please check that.'}
  tdr
}
