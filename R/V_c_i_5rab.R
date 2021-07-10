#' Conflict Traffic Volume of i-Ramp at Five-way Roundabout
#'
#' Conflict traffic volume (pcph) of approach i at 5-way roundabout
#'     It follows <Table 11-7> in KHCM(2013) p.493, 505.
#' @param v_i1 Traffic from ramp i to other ramp 1(pcph)
#' @param v_i2 Traffic from ramp i to other ramp 2(pcph)
#' @param v_i3 Traffic from ramp i to other ramp 3(pcph)
#' @param v_i4 Traffic from ramp i to other ramp 4(pcph)
#' @param v_u Sum of U-turn traffic excluding U-turn traffic on ramp i(pcph)
#' @keywords conflict traffic volume five-way roundabout
#' @seealso \code{\link{V_c_NB_rab}}, \code{\link{V_c_i_3rab}}
#' @export V_c_i_5rab Conflicting traffic volume (vph)
#' @examples
#' V_c_i_5rab(v_i1 = 132, v_i2 = 494, v_i3 = 90, v_i4 = 343, v_u = 100)
V_c_i_5rab <- function(v_i1 = NULL, v_i2 = NULL, v_i3 = NULL, v_i4 = NULL, v_u = NULL){
  if (v_i1 >= 0 & v_i2 >= 0 & v_i3 >= 0 & v_i4 >= 0 & v_u >= 0){v <- v_i1 + v_i2 + v_i3 + v_i4 + v_u}
  else {v <- 'Error : [v_i1], [v_i2], [v_i3], [v_i4], [v_u] must be positive(pcph). Please check that.'}
  v
}
