#' Average control delay per vehicle in 2-lane Road
#'
#' Average control delay per vehicle on a two-lane road (seconds/vehicle).
#'     It follows <Formula 7-8> in KHCM(2013) p.175.
#' @param d_1 Uniform control delay (sec/vehicle)
#' @param d_2 Incremental lag indicative of intentionality and supersaturation. When there is no vehicle remaining at the end of the cycle immediately preceding the analysis period (seconds/vehicle)
#' @param d_3 Additional delay (sec/vehicle) received by vehicles arriving in the analysis period due to the remaining waiting vehicles before the analysis period.
#' @param PF Interlocking correction coefficient by signal interlocking
#' @export d_2l Average control delay per vehicle(seconds/vehicle).
#' @examples
#' d_2l(d_1 = 3.29, d_2 = 3.42, d_3 = 1.2, PF = 0.8)
d_2l <- function(d_1 = NULL, d_2 = NULL, d_3 = NULL, PF = NULL){
  if (d_1 >= 0 & d_2 >= 0 & d_3 >= 0 & PF >= 0){res <- d_1 * PF + d_2 + d_3}
  else {res <- 'Error : [d_1], [d_2], [d_3], [PF] must be positive. Please check that.'}
  res
}
