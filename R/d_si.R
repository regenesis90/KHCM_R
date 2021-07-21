#' Average Control Delay Per Vehicle on Signalized Intersection
#'
#' Average control delay per vehicle on signalized intersection (sec/veh).
#'     It follows <Formula 8-43> in KHCM(2013) p.251.
#' @param d_1 Interlocking corrected uniform control delay(sec/veh). See \code{\link{d_1_nq_si}}, \code{\link{d_1_yq_si}}
#' @param d_2 Incremental lag indicative of random arrivals and supersaturation. See \code{\link{d_2_nq_si}}, \code{\link{d_2_yq_si}}
#' @param d_3 Additional delay (sec/veh). See \code{\link{d_3_case_si}}, \code{\link{d_3_yq_si}}
#' @param PF Interlocking coefficient. See \code{\link{PF_si}}
#' @seealso \code{\link{d_1_nq_si}}, \code{\link{d_1_yq_si}}, \code{\link{d_2_nq_si}}, \code{\link{d_2_yq_si}}, \code{\link{d_3_case_si}}, \code{\link{d_3_yq_si}}, \code{\link{PF_si}}
#' @export d_si Average control delay per vehicle
#' @examples
#' d_si(d_1 = 32.1, d_2 = 11.1, d_3 = 9.20, PF = 1.43)
d_si <- function(d_1 = NULL, d_2 = NULL, d_3 = NULL, PF = NULL){
  if (d_1 > 0 & d_2 > 0 & d_3 > 0){
    if (PF > 0){d <- d_1 * PF + d_2 + d_3}
    else {d <- 'Error : [PF] must be positive. Please check that.'}
  }
  else {d <- 'Error : [d_1], [d_2], [d_3] must be positive. Please check that.'}
  d
}
