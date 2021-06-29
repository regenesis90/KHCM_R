#' Peak Hour Factor in 2-lane Road
#'
#' This function calculates the peak hour factor(PHF) in 2-lane-road in given traffic volume.
#'     It is desirable to measure the PHF in the field, and this table is a general value,
#'     so it should be used in a limited way.
#'     It follows <Table 7-3> in KHCM(2013), p.177.
#' @param v Traffic volume(pcph/bidirectional)
#' @export PHF_2l Peak hour factor(PHF) in 2-lane-road in given traffic volume.
#' @examples
#' PHF_2l(v) = 3020)
#' PHF_2l(2000)
PHF_2l <- function(v = NULL){
  if (v > 0 & v <= 200){PHF <- 0.80}
  else if (v > 200 & v <= 400){PHF <- 0.83}
  else if (v > 400 & v <= 600){PHF <- 0.86}
  else if (v > 600 & v <= 800){PHF <- 0.88}
  else if (v > 800 & v <= 1000){PHF <- 0.90}
  else if (v > 1000 & v <= 1200){PHF <- 0.91}
  else if (v > 1200 & v <= 1400){PHF <- 0.92}
  else if (v > 1400 & v <= 1600){PHF <- 0.93}
  else if (v > 1600 & v <= 1800){PHF <- 0.94}
  else if (v > 1800 & v <= 2000){PHF <- 0.95}
  else if (v > 2000 & v <= 2200){PHF <- 0.95}
  else if (v > 2200 & v <= 2400){PHF <- 0.96}
  else if (v > 2400){PHF <- 0.96}
  else {PHF <- 'Error : [v] must be positive(pcph). Please check that.'}
  PHF
}
