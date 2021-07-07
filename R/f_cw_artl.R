#' Pedestrian Crossing Signal Correction Factor on Arterial Road
#'
#' Pedestrian crossing signal correction factor at signal intersection on arterial road.
#'     - If there is a pedestrian crossing signal in the middle of the arterial road link,
#'     the interlocking corrected uniform delay value(See \code{\link{d1_artl}}) should be multiplied by the pedestrian crossing signal correction factor at the signal intersection.
#'     - This function follows <Table 12-7> in KHCM(2013) p.539
#' @param interlock Whether to link(to interlock). Choose one from: \code{'yes'}, \code{'no'}
#' @param n_cross Number of crosswalk road in target section
#' @keywords pedestrian crossing signal correction factor arterial road
#' @seealso \code{\link{d1_artl}}, \code{\link{d_artl}}
#' @details
#'     - The value of this coefficient is different depending on the number of crossing signals on a single road between signal intersections and whether they are interlocked.
#'     - The number of crosswalks means the number of crosswalks with crosswalk signals installed in the analysis section.
#'     - When the crossing signal and the intersection signal are not interlocked with each other,
#'     the interlocking between the signal intersections has no practical effect, so the interlocking coefficient should be applied to 1.0 as in the case of non-interlocking.
#' @export f_cw_artl
#' @examples
#' f_cw_artl('yes', 4)
f_cw_artl <- function(interlock = NULL, n_cross = NULL){
  if (interlock == 'yes'){
    if (n_cross == 0){f <- 1.0}
    else if (n_cross == 1){f <- 1.0}
    else if (n_cross >= 2){f <- 1.1}
    else {f <- 'Error : [n_cross] must be positive integer. Please check that.'}
  }
  else if (interlock == 'no'){
    if (n_cross == 0){f <- 1.0}
    else if (n_cross == 1){f <- 1.1}
    else if (n_cross >= 2){f <- 1.2}
    else {f <- 'Error : [n_cross] must be positive integer. Please check that.'}
  }
  else {f <- 'Error : [interlock] must be one of [yes], [no]. Please check that.'}
  f
}
