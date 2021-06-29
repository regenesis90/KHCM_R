#' Average Travel Speed in Type2 2-lane Road, i Section
#'
#' Average travel speed (km/h) in section i of section type 2 on a two-lane road.
#'     This function follows <Formula 7-9> in KHCM(2013), p.175.
#' @param FFS_up Upstream free speed(kph)
#' @param L_2_i Section i length (km) of section type 2 (signal intersection influence zone)
#' @param d Average control delay per vehicle in 2-lane Road(seconds/veh). See \code{\link{d_2l}}
#' @keywords Average Travel Speed ATS 2-lane Road
#' @export ATS_2_i_2l Average travel speed in type2 2-lane road, i section(kph)
#' @seealso \code{\link{ATS_1_i_2l}}, \code{\link{ATS_2l}}, \code{\link{d_2l}}
#' @examples
#' ATS_2_i_2l(FFS_up = 70, L_2_i = 2.4, d = 19.2)
ATS_2_i_2l <- function(FFS_up = NULL, L_2_i = NULL, d = NULL){
  if (FFS_up >= 0){
    if (L_2_i >= 0){
      if (d >= 0){
        ats <- 3.6 * L_2_i / ((d + 3.6 * L_2_i)/FFS_up)
      }
      else {ats <- 'Error : [d] must be positive(seconds/veh). Please check that.'}
    }
    else {ats <- 'Error : [L_2_i] must be positive(m). Please check that.'}
  }
  else {ats <- 'Error : [FFS_up] must be positive(kph). Please check that.'}
  ats
}
