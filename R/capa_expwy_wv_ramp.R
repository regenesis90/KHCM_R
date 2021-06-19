#' The Capacity of the Weaving Section, Ramp Weave
#'
#' This function decides the capacity of the weaving section, ramp weave(pcph).
#'     It follows <Table 3-2> in KHCM(2013), p.63-64.
#' @param design_speed Design speed of the main lane(kph). Choose one from: \code{80}, \code{100}, \code{120}
#' @param VR Designed ratio of Weaving traffic flow/Total traffic flow in expressway weaving section(pcph). Choose one from: \code{0.10}, \code{0.20}, \code{0.30}, \code{0.40}
#' @param N Total number of lanes in the weaving section. Choose one from: \code{3}, \code{4}, \code{5}
#' @param L Length of the weaving section(m).
#' @keywords capacity weaving section freeway main line link
#' @export capa_expwy_wv_ramp Capacity of the weaving section, ramp weave(pcph)
#' @examples
#' capa_expwy_wv_ramp(design_speed = 120, VR = 0.20, L = 550, N = 4)
capa_expwy_wv_ramp <- function(design_speed = NULL, VR = NULL, L = NULL, N = NULL){
  if (design_speed >= 100){
    if (VR == 0.10){
      if (N == 3){
        if (L == 150){cap <- 5100}
        else if (L > 150 & L < 300){cap <- 5100 + (100/150) * (L - 150)}
        else if (L == 300){cap <- 5200}
        else if (L > 300 & L < 450){cap <- 5200 + (200/150) * (L - 300)}
        else if (L == 450){cap <- 5400}
        else if (L > 450 & L < 600){cap <- 5400 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 5500}
        else if (L > 600){cap <- 5500 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 4){
        if (L == 150){cap <- 6900}
        else if (L > 150 & L < 300){cap <- 6900 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 7100}
        else if (L > 300 & L < 450){cap <- 7100 + (200/150) * (L - 300)}
        else if (L == 450){cap <- 7300}
        else if (L > 450 & L < 600){cap <- 7300 + (200/150) * (L - 450)}
        else if (L == 600){cap <- 7500}
        else if (L > 600){cap <- 7500 + (200/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 5){
        if (L == 150){cap <- 8600}
        else if (L > 150 & L < 300){cap <- 8600 + (300/150) * (L - 150)}
        else if (L == 300){cap <- 8900}
        else if (L > 300 & L < 450){cap <- 8900 + (300/150) * (L - 300)}
        else if (L == 450){cap <- 9200}
        else if (L > 450 & L < 600){cap <- 9200 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 9300}
        else if (L > 600){cap <- 9300 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else {cap <- 'Error : [N] must be one of [3], [4], [5]. Please check that.'}
    }
    else if (VR == 0.20){
      if (N == 3){
        if (L == 150){cap <- 5000}
        else if (L > 150 & L < 300){cap <- 5000 + (100/150) * (L - 150)}
        else if (L == 300){cap <- 5100}
        else if (L > 300 & L < 450){cap <- 5100 + (200/150) * (L - 300)}
        else if (L == 450){cap <- 5300}
        else if (L > 450 & L < 600){cap <- 5300 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 5400}
        else if (L > 600){cap <- 5400 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 4){
        if (L == 150){cap <- 6800}
        else if (L > 150 & L < 300){cap <- 6800 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 7000}
        else if (L > 300 & L < 450){cap <- 7000 + (200/150) * (L - 300)}
        else if (L == 450){cap <- 7200}
        else if (L > 450 & L < 600){cap <- 7200 + (200/150) * (L - 450)}
        else if (L == 600){cap <- 7400}
        else if (L > 600){cap <- 7400 + (200/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 5){
        if (L == 150){cap <- 8400}
        else if (L > 150 & L < 300){cap <- 8400 + (300/150) * (L - 150)}
        else if (L == 300){cap <- 8700}
        else if (L > 300 & L < 450){cap <- 8700 + (300/150) * (L - 300)}
        else if (L == 450){cap <- 9000}
        else if (L > 450 & L < 600){cap <- 9000 + (200/150) * (L - 450)}
        else if (L == 600){cap <- 9200}
        else if (L > 600){cap <- 9200 + (200/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else {cap <- 'Error : [N] must be one of [3], [4], [5]. Please check that.'}
    }
    else if (VR == 0.30){
      if (N == 3){
        if (L == 150){cap <- 4900}
        else if (L > 150 & L < 300){cap <- 4900 + (100/150) * (L - 150)}
        else if (L == 300){cap <- 5000}
        else if (L > 300 & L < 450){cap <- 5000 + (200/150) * (L - 300)}
        else if (L == 450){cap <- 5200}
        else if (L > 450 & L < 600){cap <- 5200 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 5300}
        else if (L > 600){cap <- 5300 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 4){
        if (L == 150){cap <- 6600}
        else if (L > 150 & L < 300){cap <- 6600 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 6800}
        else if (L > 300 & L < 450){cap <- 6800 + (200/150) * (L - 300)}
        else if (L == 450){cap <- 7100}
        else if (L > 450 & L < 600){cap <- 7100 + (200/150) * (L - 450)}
        else if (L == 600){cap <- 7300}
        else if (L > 600){cap <- 7300 + (200/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 5){
        if (L == 150){cap <- 8200}
        else if (L > 150 & L < 300){cap <- 8200 + (400/150) * (L - 150)}
        else if (L == 300){cap <- 8600}
        else if (L > 300 & L < 450){cap <- 8600 + (300/150) * (L - 300)}
        else if (L == 450){cap <- 8900}
        else if (L > 450 & L < 600){cap <- 8900 + (200/150) * (L - 450)}
        else if (L == 600){cap <- 9100}
        else if (L > 600){cap <- 9100 + (200/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else {cap <- 'Error : [N] must be one of [3], [4], [5]. Please check that.'}
    }
    else if (VR == 0.40){
      if (N == 3){
        if (L == 150){cap <- 4800}
        else if (L > 150 & L < 300){cap <- 4800 + (100/150) * (L - 150)}
        else if (L == 300){cap <- 4900}
        else if (L > 300 & L < 450){cap <- 4900 + (200/150) * (L - 300)}
        else if (L == 450){cap <- 5100}
        else if (L > 450 & L < 600){cap <- 5100 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 5200}
        else if (L > 600){cap <- 5200 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 4){
        if (L == 150){cap <- 6500}
        else if (L > 150 & L < 300){cap <- 6500 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 6700}
        else if (L > 300 & L < 450){cap <- 6700 + (300/150) * (L - 300)}
        else if (L == 450){cap <- 7000}
        else if (L > 450 & L < 600){cap <- 7000 + (200/150) * (L - 450)}
        else if (L == 600){cap <- 7200}
        else if (L > 600){cap <- 7200 + (200/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 5){
        if (L == 150){cap <- 8100}
        else if (L > 150 & L < 300){cap <- 8100 + (300/150) * (L - 150)}
        else if (L == 300){cap <- 8400}
        else if (L > 300 & L < 450){cap <- 8400 + (400/150) * (L - 300)}
        else if (L == 450){cap <- 8800}
        else if (L > 450 & L < 600){cap <- 8800 + (400/150) * (L - 450)}
        else if (L == 600){cap <- 9000}
        else if (L > 600){cap <- 9000 + (200/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else {cap <- 'Error : [N] must be one of [3], [4], [5]. Please check that.'}
    }
    else {cap <- 'Error : [VR] must be one of [0.10], [0.20], [0.30], [0.40]. Please check that.'}
  }
  else if (design_speed == 80){
    if (VR == 0.10){
      if (N == 3){
        if (L == 150){cap <- 4600}
        else if (L > 150 & L < 300){cap <- 4600 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 4800}
        else if (L > 300 & L < 450){cap <- 4800 + (100/150) * (L - 300)}
        else if (L == 450){cap <- 4900}
        else if (L > 450 & L < 600){cap <- 4900 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 5000}
        else if (L > 600){cap <- 5000 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 4){
        if (L == 150){cap <- 6200}
        else if (L > 150 & L < 300){cap <- 6200 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 6400}
        else if (L > 300 & L < 450){cap <- 6400 + (200/150) * (L - 300)}
        else if (L == 450){cap <- 6600}
        else if (L > 450 & L < 600){cap <- 6600 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 6700}
        else if (L > 600){cap <- 6700 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 5){
        if (L == 150){cap <- 7800}
        else if (L > 150 & L < 300){cap <- 7800 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 8000}
        else if (L > 300 & L < 450){cap <- 8000 + (300/150) * (L - 300)}
        else if (L == 450){cap <- 8300}
        else if (L > 450 & L < 600){cap <- 8300 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 8400}
        else if (L > 600){cap <- 8400 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else {cap <- 'Error : [N] must be one of [3], [4], [5]. Please check that.'}
    }
    else if (VR == 0.20){
      if (N == 3){
        if (L == 150){cap <- 4500}
        else if (L > 150 & L < 300){cap <- 4500 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 4700}
        else if (L > 300 & L < 450){cap <- 4700 + (100/150) * (L - 300)}
        else if (L == 450){cap <- 4800}
        else if (L > 450 & L < 600){cap <- 4800 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 4900}
        else if (L > 600){cap <- 4900 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 4){
        if (L == 150){cap <- 6100}
        else if (L > 150 & L < 300){cap <- 6100 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 6300}
        else if (L > 300 & L < 450){cap <- 6300 + (200/150) * (L - 300)}
        else if (L == 450){cap <- 6500}
        else if (L > 450 & L < 600){cap <- 6500 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 6600}
        else if (L > 600){cap <- 6600 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 5){
        if (L == 150){cap <- 7700}
        else if (L > 150 & L < 300){cap <- 7700 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 7900}
        else if (L > 300 & L < 450){cap <- 7900 + (300/150) * (L - 300)}
        else if (L == 450){cap <- 8200}
        else if (L > 450 & L < 600){cap <- 8200 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 8300}
        else if (L > 600){cap <- 8300 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else {cap <- 'Error : [N] must be one of [3], [4], [5]. Please check that.'}
    }
    else if (VR == 0.30){
      if (N == 3){
        if (L == 150){cap <- 4400}
        else if (L > 150 & L < 300){cap <- 4400 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 4600}
        else if (L > 300 & L < 450){cap <- 4600 + (100/150) * (L - 300)}
        else if (L == 450){cap <- 4700}
        else if (L > 450 & L < 600){cap <- 4700 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 4800}
        else if (L > 600){cap <- 4800 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 4){
        if (L == 150){cap <- 5900}
        else if (L > 150 & L < 300){cap <- 5900 + (300/150) * (L - 150)}
        else if (L == 300){cap <- 6200}
        else if (L > 300 & L < 450){cap <- 6200 + (200/150) * (L - 300)}
        else if (L == 450){cap <- 6400}
        else if (L > 450 & L < 600){cap <- 6400 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 6500}
        else if (L > 600){cap <- 6500 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 5){
        if (L == 150){cap <- 7600}
        else if (L > 150 & L < 300){cap <- 7600 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 7800}
        else if (L > 300 & L < 450){cap <- 7800 + (300/150) * (L - 300)}
        else if (L == 450){cap <- 8100}
        else if (L > 450 & L < 600){cap <- 8100 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 8200}
        else if (L > 600){cap <- 8200 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else {cap <- 'Error : [N] must be one of [3], [4], [5]. Please check that.'}
    }
    else if (VR == 0.40){
      if (N == 3){
        if (L == 150){cap <- 4300}
        else if (L > 150 & L < 300){cap <- 4300 + (200/150) * (L - 150)}
        else if (L == 300){cap <- 4500}
        else if (L > 300 & L < 450){cap <- 4500 + (100/150) * (L - 300)}
        else if (L == 450){cap <- 4600}
        else if (L > 450 & L < 600){cap <- 4600 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 4700}
        else if (L > 600){cap <- 4700 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 4){
        if (L == 150){cap <- 5700}
        else if (L > 150 & L < 300){cap <- 5700 + (400/150) * (L - 150)}
        else if (L == 300){cap <- 6100}
        else if (L > 300 & L < 450){cap <- 6100 + (200/150) * (L - 300)}
        else if (L == 450){cap <- 6300}
        else if (L > 450 & L < 600){cap <- 6300 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 6400}
        else if (L > 600){cap <- 6400 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else if (N == 5){
        if (L == 150){cap <- 7300}
        else if (L > 150 & L < 300){cap <- 7300 + (300/150) * (L - 150)}
        else if (L == 300){cap <- 7600}
        else if (L > 300 & L < 450){cap <- 7600 + (400/150) * (L - 300)}
        else if (L == 450){cap <- 8000}
        else if (L > 450 & L < 600){cap <- 8000 + (100/150) * (L - 450)}
        else if (L == 600){cap <- 8100}
        else if (L > 600){cap <- 8100 + (100/150) * (L - 600)}
        else{cap <- 'Error : [L] must be >= 150. Please check that.'}
      }
      else {cap <- 'Error : [N] must be one of [3], [4], [5]. Please check that.'}
    }
    else {cap <- 'Error : [VR] must be one of [0.10], [0.20], [0.30], [0.40]. Please check that.'}
  }
  else {cap <- 'Error : [design_speed] must be one of [80], [100], [120](kph). Please check that.'}
  cap
}
