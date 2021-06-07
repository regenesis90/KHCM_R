#' Conflict Traffic Flow(V_ci)
#'
#' It follows <Figure 10-5> in KHCM(2013), p.465
#' @param type Choose one from : \code{'right_sub'}, \code{'left_main'}, \code{'straight_sub'}, \code{'left_sub'}
#' @param main_right_turn_lane \code{'yes'}, \code{'no'}
#' @param sub_road_wide \code{'yes'}, \code{'no'}. Where the radius of the right turn on the sub-road is wide, or where this flow is controlled by stopping / yielding
#' @param main_multi_load \code{'yes'}, \code{'no'}
#' @param V_r
#' @param V_t
#' @param V_ra
#' @param V_ta
#' @param V_la
#' @param V_rb
#' @param V_tb
#' @param V_lb
#' @param V_o
#' @param V_or
#' @export V_ci Conflict Traffic Flow
#' @examples
V_ci <- function(type = NULL, main_right_turn_lane = NULL, sub_road_wide = NULL, main_multi_load = NULL, V_r = NULL, V_t = NULL, V_ra = NULL, V_ta = NULL, V_la = NULL, V_rb = NULL, V_tb = NULL, V_lb = NULL, V_o = NULL, V_or = NULL){
  if (type == 'right_sub'){
    if (main_right_turn_lane == 'yes'){vci <- V_t}
    if (main_right_turn_lane == 'no'){vci <- 1/2*(V_r) + V_t}
  }
  if (type == 'left_main'){
    if (sub_road_wide == 'yes'){vci <- V_t}
    if (sub_road_wide == 'no'){vci <- V_r + V_t}
  }
  if (type == 'straight_sub'){
    if (main_multi_road == 'yes'){ #V_rb 배제
      if (main_right_turn_lane == 'yes'){vci <- V_ta + V_la + V_tb + V_lb}
      if (main_right_turn_lane == 'no'){vci <- 1/2*V_ra + V_ta + V_la + V_tb + V_lb}
    }
    if (main_multi_road == 'no'){
      if (main_right_turn_lane == 'yes'){vci <- V_ta + V_la + V_rb + V_tb + V_lb}
      if (main_right_turn_lane == 'no'){vci <- 1/2 * V_ra + V_ta + V_la + V_rb + V_tb + V_lb}
    }
  }
  if (type == 'left_sub'){
    if (main_multi_road == 'yes'){ #V_rb 배제
      if (main_right_turn_lane == 'yes'){vci <- V_ta + V_la + V_tb + V_lb + V_o + V_or}
      if (main_right_turn_lane == 'no'){vci <- 1/2*V_ra + V_ta + V_la + V_tb + V_lb + V_o + V_or}
    }
    if (main_multi_road == 'no'){
      if (main_right_turn_lane == 'yes'){vci <- V_ta + V_la + V_rb + V_tb + V_lb + V_o + V_or}
      if (main_right_turn_lane == 'no'){vci <- 1/2 * V_ra + V_ta + V_la + V_rb + V_tb + V_lb + V_o + V_or}
    }
  }
  v_ci
}
