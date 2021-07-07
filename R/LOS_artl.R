#' Level of Service(LOS) of Arterial Road
#'
#' Service level by average speed (kph) of arterial roads.
#'     The service level is stipulated based on the average speed of a part of the arterial road or the entire road.
#'     It follows <Table 12-1>, <Table 12-10> in KHCM(2013), p.529, p.547.
#' @param type Arterial road type. Choose one from: \code{'type1'}, \code{'type2'}, \code{'type3'}. See \code{\link{type_artl}}
#' @param ATS Average travel speed(kph)
#' @keywords level of service LOS arterial road ATS average travel speed
#' @export LOS_artl Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}, \code{FF}, \code{FFF}
#' @examples
#' LOS_artl(type = 'type1', ATS = 38)
#' LOS_artl(type = 'type3', ATS = 59)
LOS_artl <- function(type = NULL, ATS = NULL){
  if (type == 'type1'){
    if (ATS >= 67 & ATS <= 85){los <- 'A'}
    else if (ATS >= 51 & ATS < 67){los <- 'B'}
    else if (ATS >= 37 & ATS < 51){los <- 'C'}
    else if (ATS >= 28 & ATS < 37){los <- 'D'}
    else if (ATS >= 21 & ATS < 28){los <- 'E'}
    else if (ATS >= 10 & ATS < 21){los <- 'F'}
    else if (ATS >= 6 & ATS < 10){los <- 'FF'}
    else if (ATS < 6 & ATS >= 0){los <- 'FFF'}
    else {los <- 'Error : [ATS] must be >= 0 and <= 85. Please check that.'}
  }
  else if (type == 'type2'){
    if (ATS >= 60 & ATS <= 75){los <- 'A'}
    else if (ATS >= 46 & ATS < 60){los <- 'B'}
    else if (ATS >= 33 & ATS < 46){los <- 'C'}
    else if (ATS >= 25 & ATS < 33){los <- 'D'}
    else if (ATS >= 18 & ATS < 25){los <- 'E'}
    else if (ATS >= 10 & ATS < 18){los <- 'F'}
    else if (ATS >= 6 & ATS < 10){los <- 'FF'}
    else if (ATS < 6 & ATS >= 0){los <- 'FFF'}
    else {los <- 'Error : [ATS] must be >= 0 and <= 75. Please check that.'}
  }
  else if (type == 'type3'){
    if (ATS >= 49 & ATS <= 65){los <- 'A'}
    else if (ATS >= 39 & ATS < 49){los <- 'B'}
    else if (ATS >= 29 & ATS < 39){los <- 'C'}
    else if (ATS >= 20 & ATS < 29){los <- 'D'}
    else if (ATS >= 12 & ATS < 20){los <- 'E'}
    else if (ATS >= 8 & ATS < 12){los <- 'F'}
    else if (ATS >= 5 & ATS < 8){los <- 'FF'}
    else if (ATS < 5 & ATS >= 0){los <- 'FFF'}
    else {los <- 'Error : [ATS] must be >= 0 and <= 65. Please check that.'}
  }
  else {los <- 'Error : [type] must be one of [type1], [type2], [type3]. Please check that.'}
  los
}
