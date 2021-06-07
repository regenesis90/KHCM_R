#' Follow-up time of two-way stop intersection(t_f_x)
#'
#' TIt follows <Table 10-1> in KHCM(2013), p.466
#' @param form Choose one from: \code{'1x1'}, \code{'2x1'}
#' @param direction Choose one from: \code{'main_left'}, \code{'sub_left'}, \code{'sub_straight'}, \code{'sub_right'}
#' @keywords
#' @export t_c_x
#' @examples
#' t_f_x(form = '2x1', direction = 'sub_straight')
#' t_f_x('1x1', 'main_left')
t_f_x <- function(form = NULL, direction = NULL){
  if (form == '1x1'){
    if (direction == 'main_left'){tfx <- 2.5}
    if (direction == 'sub_left'){tfx <- 3.0}
    if (direction == 'sub_straight'){tfx <- 2.7}
    if (direction == 'sub_right'){tfx <- 2.8}
  }
  if (form == '2x1'){
    if (direction == 'main_left'){tfx <- 2.5}
    if (direction == 'sub_left'){tfx <- 3.0}
    if (direction == 'sub_straight'){tfx <- 2.7}
    if (direction == 'sub_right'){tfx <- 2.8}
  }
  tfx
}
