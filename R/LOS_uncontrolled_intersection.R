#' Level of Service(LOS) in uncontrolled intersection(LOS_uncontrolled_intersection)
#'
#' This function follows <Table 10-3>, <Table 10-4>, <Formula 10-6> in KHCM(2013) p.473
#' @param r main_road_traffic_ratio (%)
#' @param V Intersection total traffic (vph)
#' @keywords
#' @export LOS_uncontrolled_intersection
#' @examples
LOS_uncontrolled_intersection <- function(r = NULL, V = NULL){
  if (r >= 0 & r < 60){
    a <- 0.1508
    y <- a * V
    if (V >= 0 & V <= 320 & y <= 60){los <- 'A'}
    if (V > 320 & V <= 640 & y <= 120){los <- 'B'}
    if (V > 640 & V <= 960 & y <= 180){los <- 'C'}
    if (V > 960 & V <= 1280 & y <= 240){los <- 'D'}
    if (V > 1280 & V <= 1600 & y <= 300){los <- 'E'}
    if (V > 1600 & y > 300){los <- 'F'}
  }
  if (r >= 60 & r < 70){
    a <- 0.1487
    y <- a * V
    if (V >= 0 & V <= 360 & y <= 60){los <- 'A'}
    if (V > 360 & V <= 720 & y <= 120){los <- 'B'}
    if (V > 720 & V <= 1080 & y <= 180){los <- 'C'}
    if (V > 1080 & V <= 1440 & y <= 240){los <- 'D'}
    if (V > 1440 & V <= 1800 & y <= 300){los <- 'E'}
    if (V > 1800 & y > 300){los <- 'F'}
  }
  if (r >= 70){
    a <- 0.1426
    y <- a * V
    if (V >= 0 & V <= 400 & y <= 60){los <- 'A'}
    if (V > 400 & V <= 800 & y <= 120){los <- 'B'}
    if (V > 800 & V <= 1200 & y <= 180){los <- 'C'}
    if (V > 1200 & V <= 1600 & y <= 240){los <- 'D'}
    if (V > 1600 & V <= 2000 & y <= 300){los <- 'E'}
    if (V > 2000 & y > 300){los <- 'F'}
  }
  los
}
