#' Critical interval of two-way stop intersection(t_c_x)
#'
#' TIt follows <Table 10-1> in KHCM(2013), p.466
#' @param form Choose one from: \code{'1x1'}, \code{'2x1'}
#' @param direction Choose one from: \code{'main_left'}, \code{'sub_left'}, \code{'sub_straight'}, \code{'sub_right'}
#' @keywords
#' @export t_c_x
#' @examples
#' t_c_x(form = '2x1', direction = 'sub_straight')
#' t_c_x('1x1', 'main_left')
t_c_x <- function(form = NULL, direction = NULL){
  if (form == '1x1'){
    if (direction == 'main_left'){tcx <- 4.2}
    if (direction == 'sub_left'){tcx <- 4.6}
    if (direction == 'sub_straight'){tcx <- 4.5}
    if (direction == 'sub_right'){tcx <- 3.7}
  }
  if (form == '2x1'){
    if (direction == 'main_left'){tcx <- 4.9}
    if (direction == 'sub_left'){tcx <- 5.2}
    if (direction == 'sub_straight'){tcx <- 5.4}
    if (direction == 'sub_right'){tcx <- 4.4}
  }
  tcx
}
