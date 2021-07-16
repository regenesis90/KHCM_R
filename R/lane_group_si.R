#' Criteria for Classifying Lanes at Signalized Intersection
#'
#' Criteria for classifying lanes at signalized intersection.
#'    It follows <Table 8-14> in KHCM(2013), p.240.
#' @param case Classification by signal operation and left turn lane at signalized intersections. Choose one from: \code{'case1'}, \code{'case2'}, \code{'case3'}, \code{'case4'}, \code{'case5'}, \code{'case6'}. See \code{\link{case_si}}
#' @param N Number of lanes(Except for dedicated left-turn lanes)
#' @param V_STL Straight-through traffic volume(vph) using shared left-turn lanes at signalized intersection. See \code{\link{V_STL_si}}
#' @param V_STR Straight-through traffic volume(vph) using shared right-turn lanes at signalized intersection. See \code{\link{V_STR_si}}
#' @param V_LF Traffic going straight ahead of the first left turn on the shared left turn lane at the signal intersection(vph). See \code{\link{V_LF_si}}
#' @param V_RF Traffic going straight ahead of the first right turn on the shared right turn lane at the signal intersection(vph). See \code{\link{V_RF_si}}
#' @seealso \code{\link{case_si}}, \code{\link{V_STR_si}}, \code{\link{V_LF_si}}, \code{\link{V_RF_si}}
#' @keywords Criteria classify lane signalized intersection
#' @details
#'     - Signal intersection capacity analysis is carried out separately by approach road and lane group, and the lane group varies according to the traffic distribution of the moving flow. That is, the moving flows proceeding at different times form separate lane groups.
#'     - Also, in the case of different types of movement proceeding at the same time, if the flow ratio (V/S) or V/c ratio is different, they are classified as separate lane groups.
#'     - Conversely, since the left-turning or right-turning lane is shared by going straight, the left-turning or right-turning flow forms the same lane group with the straight-forward and is analyzed in an integrated way when the equilibrium state with the straight lane is expressed in terms of the traffic ratio.
#' @export lane_group_si \code{''}
#' @examples
#' lane_group_si(case = 'case4', N = 2, V_STL = 304, V_STR = 292, V_LF = 334, V_RF = 90)
lane_group_si <- function(case = NULL, N = NULL, V_STL = NULL, V_STR = NULL, V_LF = NULL, V_RF = NULL){
  if (case == 'case1' | case == 'case2' | case == 'case3'){res <- 'Dedicated left lane is separated lane.'}
  else {
    if (N == 1){res <- 'Integrated Lane(Except dedicated left lane)'}
    else {
      if (V_STL < V_LF){
        if (V_STR < V_RF){res <- 'Left : Practically dedicated, Right : Practically dedicated'}
        else if (V_STR > V_RF){res <- 'Left : Practically dedicated, Right : Combined straight and right turn'}
        else {res <- 'Left : Practically dedicated, Right : ??'}
        }
      else if (V_STL > V_LF){
        if (V_STR < V_RF){res <- 'Left : Combined straight and left turn, Right : Practically dedicated'}
        else if (V_STR > V_RF){res <- 'Left : Combined straight and left turn, Right : Combined straight and right turn'}
        else {res <- 'Left : Combined straight and left turn, Right : ??'}
      }
      else {
        if (V_STR < V_RF){res <- 'Left : ??, Right : Practically dedicated'}
        else if (V_STR > V_RF){res <- 'Left : ??, Right : Combined straight and right turn'}
        else {res <- 'Left : ??, Right : ??'}
      }
    }
  }
  res
}
