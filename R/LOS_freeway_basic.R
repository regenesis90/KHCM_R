#' Level of Service(LOS) in Basic Freeway Section
#'
#' This function decides Level of Service(LOS) in the basic freeway section.
#' @param density *Numeric* The density of the road(pcpkmpl)
#' @param design_speed *Categorical* Choose one from : \code{120}, \code{100}, \code{80}
#' @param volume *Numeric* Traffic Volume(pcphpl)
#' @param v_c_ratio *Numeric* It should be more than zero.
#' @keywords LOS Level of Service Density V/C ratio
#' @export LOS Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}
#' @examples
#' DDHV(AADT = 1500, region = 'city', road = 'general', lane = 2)
#' LOS_freeway_basic(density = 30)
#' LOS_freeway_basic(design_speed = 120, v_c_ratio = 0.5)
#' LOS_freeway_basic(design_speed = 80, volume = 1000)
LOS_freeway_basic <- function(density = NULL, design_speed = NULL, volume = NULL, v_c_ratio = NULL){
  if (is.null(density) == FALSE & density >= 0){
    if (density >= 0 & density <= 6){
      LOS <- 'A'
    }
    if (density > 6 & density <= 10){
      LOS <- 'B'
    }
    if (density > 10 & density <= 14){
      LOS <- 'C'
    }
    if (density > 14 & density <= 19){
      LOS <- 'D'
    }
    if (density > 19 & density <= 28){
      LOS <- 'E'
    }
    if (density > 28){
      LOS <- 'F'
    }
  }
  if (is.null(density) == TRUE){
    if (is.null(volume) == FALSE & volume >= 0){
      if (design_speed == 120){
        if(volume >= 0 & volume <= 700){
          LOS <- 'A'
        }
        if(volume > 700 & volume <= 1150){
          LOS <- 'B'
        }
        if(volume > 1150 & volume <= 1500){
          LOS <- 'C'
        }
        if(volume > 1500 & volume <= 1900){
          LOS <- 'D'
        }
        if(volume > 1900 & volume <= 2300){
          LOS <- 'E'
        }
      }
      if (design_speed == 100){
        if(volume >= 0 & volume <= 600){
          LOS <- 'A'
        }
        if(volume > 600 & volume <= 1000){
          LOS <- 'B'
        }
        if(volume > 1000 & volume <= 1350){
          LOS <- 'C'
        }
        if(volume > 1350 & volume <= 1750){
          LOS <- 'D'
        }
        if(volume > 1750 & volume <= 2200){
          LOS <- 'E'
        }
      }
      if (design_speed == 80){
        if(volume >= 0 & volume <= 500){
          LOS <- 'A'
        }
        if(volume > 500 & volume <= 800){
          LOS <- 'B'
        }
        if(volume > 800 & volume <= 1150){
          LOS <- 'C'
        }
        if(volume > 1150 & volume <= 1500){
          LOS <- 'D'
        }
        if(volume > 1500 & volume <= 2000){
          LOS <- 'E'
        }
      }
    }
    if (is.null(volume) == TRUE){
      if (design_speed == 120){
        if(v_c_ratio >= 0 & v_c_ratio <= 0.30){
          LOS <- 'A'
        }
        if(v_c_ratio > 0.30 & v_c_ratio <= 0.50){
          LOS <- 'B'
        }
        if(v_c_ratio > 0.50 & v_c_ratio <= 0.65){
          LOS <- 'C'
        }
        if(v_c_ratio > 0.65 & v_c_ratio <= 0.83){
          LOS <- 'D'
        }
        if(v_c_ratio > 0.83 & v_c_ratio <= 1.00){
          LOS <- 'E'
        }
      }
      if (design_speed == 100){
        if(v_c_ratio >= 0 & v_c_ratio <= 0.27){
          LOS <- 'A'
        }
        if(v_c_ratio > 0.27 & v_c_ratio <= 0.45){
          LOS <- 'B'
        }
        if(v_c_ratio > 0.45 & v_c_ratio <= 0.61){
          LOS <- 'C'
        }
        if(v_c_ratio > 0.61 & v_c_ratio <= 0.80){
          LOS <- 'D'
        }
        if(v_c_ratio > 0.80 & v_c_ratio <= 1.00){
          LOS <- 'E'
        }
      }
      if (design_speed == 80){
        if(v_c_ratio >= 0 & v_c_ratio <= 0.25){
          LOS <- 'A'
        }
        if(v_c_ratio > 0.25 & v_c_ratio <= 0.40){
          LOS <- 'B'
        }
        if(v_c_ratio > 0.40 & v_c_ratio <= 0.58){
          LOS <- 'C'
        }
        if(v_c_ratio > 0.58 & v_c_ratio <= 0.75){
          LOS <- 'D'
        }
        if(v_c_ratio > 0.75 & v_c_ratio <= 1.00){
          LOS <- 'E'
        }
      }
  }
  }
  LOS
}
