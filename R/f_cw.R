#' Pedestrian crossing signal correction factor between signal intersections(f_cw)
#'
#' This function follows <Table 12-7> in KHCM(2013) p.539
#' @param interlock Choose one from: \code{'yes'}, \code{'no'}
#' @param n_crossing Number of crosswalk road in target section
#' @keywords
#' @export f_cw
#' @examples
#' f_cw('yes', 4)
f_cw <- function(interlock = NULL, n_crossing = NULL){
  if (interlock == 'yes'){
    if (n_crossing == 0){f <- 1.0}
    if (n_crossing == 1){f <- 1.0}
    if (n_crossing >= 2){f <- 1.1}
  }
  if (interlock == 'no'){
    if (n_crossing == 0){f <- 1.0}
    if (n_crossing == 1){f <- 1.1}
    if (n_crossing >= 2){f <- 1.2}
  }
  f
}
