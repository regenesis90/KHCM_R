#' Peak Hour Factor in 2-lane Road (PHF_2lane_road)
#'
#' This function calculates the peak hour factor(PHF) in 2-lane-road in given traffic volume.
#'     It is desirable to measure the PHF in the field, and this table is a general value,
#'     so it should be used in a limited way.
#' @param V *Numeric* Traffic Volume(pcph/bidirectional)
#' @export PHF_2lane_road Peak hour factor(PHF) in 2-lane-road in given traffic volume.
#' @examples
#' PHF_2lane_road(V = 3020)
#' PHF_2lane_road(2000)
PHF_2lane_road <- function(V = NULL){
  if (V > 0 & V <= 200){PHF <- 0.80}
  if (V > 200 & V <= 400){PHF <- 0.83}
  if (V > 400 & V <= 600){PHF <- 0.86}
  if (V > 600 & V <= 800){PHF <- 0.88}
  if (V > 800 & V <= 1000){PHF <- 0.90}
  if (V > 1000 & V <= 1200){PHF <- 0.91}
  if (V > 1200 & V <= 1400){PHF <- 0.92}
  if (V > 1400 & V <= 1600){PHF <- 0.93}
  if (V > 1600 & V <= 1800){PHF <- 0.94}
  if (V > 1800 & V <= 2000){PHF <- 0.95}
  if (V > 2000 & V <= 2200){PHF <- 0.95}
  if (V > 2200 & V <= 2400){PHF <- 0.96}
  if (V > 2400){PHF <- 0.96}
  PHF
}
