#' Lane Width Correction Factor at Signalized Intersection
#'
#' Lane width correction factor at signalized intersection.
#'     Effect of reducing saturation traffic flow rate by lane width.
#'     If the lane widths within a lane group are different, the average value of these is used.
#'     It follows <Table 8-15> in KHCM(2013), p.248.
#' @param width Lane width(m/lane).
#' @export f_w_si
#' @details
#'     The saturation traffic flow rate of the moving flow is affected by the lane width.
#'     That is, if the width of the lane is narrow, the traffic flow rate is reduced
#'     because the traffic flow is hindered by the next lane or the flow next to it,
#'     or a feeling of psychological atrophy is felt.
#'     Conversely, if the lane width is too wide and used as two lanes at a stop line,
#'     the passing rate of vehicles increases, but conflicts between vehicles increase and safety problems may occur.
#' @seealso \code{\link{S_i_si}}
#' @examples
#' f_w_si(2.89)
#' f_w_si(width = 3.23)
f_w_si <- function(width = NULL){
  if (width > 0 & width <= 2.6){f <- 0.88}
  else if (width > 2.6 & width <= 2.9){f <- 0.94}
  else if (width >= 3.0){f <- 1.00}
  else {f <- 'Error : [width] must be positive(m). Please check that.'}
  f
}
