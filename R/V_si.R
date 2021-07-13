#' Traffic Volume Corrected for Lane Use at Signalized Intersection
#'
#' Traffic volume (vph) corrected for lane use at signal intersections.
#'     It is corrected by multiplying the corrected peak traffic volume by the lane use factor.
#'     It follows <Formula 8-2> in KHCM(2013), p.224-225.
#' @param V_P Peak hour traffic flow rate (vph) at signal intersection. See \code{\link{V_P_si}}
#' @param v_avg Average traffic volume by lane.(vphpl)
#' @param design_level Road design level. Choose one from : \code{C}, \code{D}, \code{E}
#' @param N Number of exclusive lanes going straight.
#' @param PHF Peak Hour Factor(PHF).
#' @details
#'     * When moving flows or moving flows included in the same lane group use multiple lanes,
#'     the same level of use is not shown for each lane.
#'     For a more detailed analysis during operation analysis,
#'     it is desirable to use the lane utilization coefficient and correct the difference in the utilization rate by lane within the lane group.
#'     However, when the V/c ratio of the lane group exceeds 0.9,
#'     there is often little or no difference in the usage rate by lane.
#'     * Without classifying the lane groups,
#'     it is not only possible to know the number of lanes in a lane group,
#'     but also because the use rate of public roundabouts for going straight is relatively low.
#'     Therefore, the average traffic volume for each straight lane of straight traffic is calculated.
#' @seealso \code{\link{V_P_si}}, \code{\link{F_U_si}}
#' @keywords traffic volume land use signalized intersection
#' @export V_si
#' @examples
#' V_si(V_P = 1200, v_avg = 400, N = 4)
#' V_si(V_P = 888, design_level = 'D', N = 3)
V_si <- function(V_P = NULL, v_avg = NULL, design_level = NULL, N = NULL){
  if (V_P > 0){
    F_U <- F_U_si(v_avg = v_avg, design_level = design_level, N = N)
    if (is.numeric(F_U) == TRUE){v <- V_P * F_U}
    else {v <- F_U}
  }
  else {v <- 'Error : [V_P] must be positive(vph). Please check that.'}
  v
}
