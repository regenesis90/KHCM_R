#' Conflict Traffic Volume of i-Ramp at Three-way Roundabout
#'
#' Conflict traffic volume (pcph) of approach i at three-way roundabout
#'     It follows <Table 11-7> in KHCM(2013) p.493, 505.
#' @param v_ij Traffic from ramp i to ramp j(pcph)
#' @param v_u Sum of U-turn traffic excluding U-turn traffic on ramp i(pcph)
#' @keywords conflict traffic volume three-way roundabout
#' @seealso \code{\link{V_c_NB_rab}}, \code{\link{V_c_i_5rab}}
#' @export V_c_i_3rab Conflicting traffic volume (vph) of northbound traffic flow
#' @examples
#' V_c_i_3rab(v_ij = 123, v_u = 44)
V_c_i_3rab <- function(v_ij = NULL, v_u = NULL){
  if (v_ij >= 0 & v_u >= 0){v <- v_ij + v_u}
  else {v <- 'Error : [v_ij], [v_u] must be positive(pcph). Please check that.'}
  v
}
