#'  Correction Factor According to Distribution by Direction and Ratio of Overtaking Prohibited Section(in TDR)
#'
#' It follows <Table 7-12> in KHCM(2013), p.181-182.
#' @param v_d One-way traffic volume(vph). Choose one from : \code{200}, \code{400}, \code{600}, \code{800}, \code{1000}, \code{1200}, \code{1400}, \code{1600>=}
#' @param v_o Opposite traffic volume(vph). Choose one from : \code{200}, \code{400}, \code{600}, \code{800}, \code{1000}, \code{1200}, \code{1400}, \code{1600>=}
#' @param P_TO Proportion of no overtaking section. Choose one from : \code{0.2}, \code{0.4}, \code{0.6}, \code{0.8}, \code{1.0}
#' @export f_np_ATS_2l Correction Factor According to Distribution by Direction and Ratio of Overtaking Prohibited Section(ATS, kph)
#' @examples
#' f_np_ATS_2l(v_d = 800, v_o = 600, P_TO = 0.4)
#' f_np_ATS_2l(1000, 1200, 0.2)
f_np_ATS_2l <- function(v_d = NULL, v_o = NULL, P_TO = NULL){
  if (P_TO == 0.2){
    if (v_d == 100){
      if (v_o == 200){f <- 3.9}
      else if (v_o == 400){f <- 3.4}
      else if (v_o == 600){f <- 2.8}
      else if (v_o == 800){f <- 2.2}
      else if (v_o == 1000){f <- 1.6}
      else if (v_o == 1200){f <- 1.0}
      else if (v_o == 1400){f <- 0.4}
      else if (v_o == 1600){f <- 0.1}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 200){
      if (v_o == 200){f <- 4.0}
      else if (v_o == 400){f <- 3.4}
      else if (v_o == 600){f <- 2.9}
      else if (v_o == 800){f <- 2.3}
      else if (v_o == 1000){f <- 1.7}
      else if (v_o == 1200){f <- 1.1}
      else if (v_o == 1400){f <- 0.5}
      else if (v_o == 1600){f <- 0.1}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 400){
      if (v_o == 200){f <- 4.2}
      else if (v_o == 400){f <- 3.6}
      else if (v_o == 600){f <- 3.0}
      else if (v_o == 800){f <- 2.4}
      else if (v_o == 1000){f <- 1.8}
      else if (v_o == 1200){f <- 1.2}
      else if (v_o == 1400){f <- 0.7}
      else if (v_o == 1600){f <- 0.1}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 600){
      if (v_o == 200){f <- 4.3}
      else if (v_o == 400){f <- 3.7}
      else if (v_o == 600){f <- 3.2}
      else if (v_o == 800){f <- 2.6}
      else if (v_o == 1000){f <- 2.0}
      else if (v_o == 1200){f <- 1.4}
      else if (v_o == 1400){f <- 0.8}
      else if (v_o == 1600){f <- 0.2}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 800){
      if (v_o == 200){f <- 4.5}
      else if (v_o == 400){f <- 3.9}
      else if (v_o == 600){f <- 3.3}
      else if (v_o == 800){f <- 2.7}
      else if (v_o == 1000){f <- 2.1}
      else if (v_o == 1200){f <- 1.5}
      else if (v_o == 1400){f <- 1.0}
      else if (v_o == 1600){f <- 0.4}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1000){
      if (v_o == 200){f <- 4.6}
      else if (v_o == 400){f <- 4.0}
      else if (v_o == 600){f <- 3.5}
      else if (v_o == 800){f <- 2.9}
      else if (v_o == 1000){f <- 2.3}
      else if (v_o == 1200){f <- 1.7}
      else if (v_o == 1400){f <- 1.1}
      else if (v_o == 1600){f <- 0.5}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1200){
      if (v_o == 200){f <- 4.8}
      else if (v_o == 400){f <- 4.2}
      else if (v_o == 600){f <- 3.6}
      else if (v_o == 800){f <- 3.0}
      else if (v_o == 1000){f <- 2.4}
      else if (v_o == 1200){f <- 1.9}
      else if (v_o == 1400){f <- 1.3}
      else if (v_o == 1600){f <- 0.7}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1400){
      if (v_o == 200){f <- 4.9}
      else if (v_o == 400){f <- 4.4}
      else if (v_o == 600){f <- 3.8}
      else if (v_o == 800){f <- 3.2}
      else if (v_o == 1000){f <- 2.6}
      else if (v_o == 1200){f <- 2.0}
      else if (v_o == 1400){f <- 1.4}
      else if (v_o == 1600){f <- 0.8}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d >= 1600){
      if (v_o == 200){f <- 5.1}
      else if (v_o == 400){f <- 4.5}
      else if (v_o == 600){f <- 3.9}
      else if (v_o == 800){f <- 3.3}
      else if (v_o == 1000){f <- 2.7}
      else if (v_o == 1200){f <- 2.2}
      else if (v_o == 1400){f <- 1.6}
      else if (v_o == 1600){f <- 1.0}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else {f <- 'Error : [v_d] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
  }
  else if (P_TO == 0.4){
    if (v_d == 100){
      if (v_o == 200){f <- 4.3}
      else if (v_o == 400){f <- 3.7}
      else if (v_o == 600){f <- 3.1}
      else if (v_o == 800){f <- 2.5}
      else if (v_o == 1000){f <- 1.9}
      else if (v_o == 1200){f <- 1.3}
      else if (v_o == 1400){f <- 0.7}
      else if (v_o == 1600){f <- 0.1}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 200){
      if (v_o == 200){f <- 4.3}
      else if (v_o == 400){f <- 3.7}
      else if (v_o == 600){f <- 3.2}
      else if (v_o == 800){f <- 2.6}
      else if (v_o == 1000){f <- 2.0}
      else if (v_o == 1200){f <- 1.4}
      else if (v_o == 1400){f <- 0.8}
      else if (v_o == 1600){f <- 0.2}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 400){
      if (v_o == 200){f <- 4.5}
      else if (v_o == 400){f <- 3.9}
      else if (v_o == 600){f <- 3.3}
      else if (v_o == 800){f <- 2.7}
      else if (v_o == 1000){f <- 2.1}
      else if (v_o == 1200){f <- 1.5}
      else if (v_o == 1400){f <- 1.0}
      else if (v_o == 1600){f <- 0.4}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 600){
      if (v_o == 200){f <- 4.6}
      else if (v_o == 400){f <- 4.0}
      else if (v_o == 600){f <- 3.5}
      else if (v_o == 800){f <- 2.9}
      else if (v_o == 1000){f <- 2.3}
      else if (v_o == 1200){f <- 1.7}
      else if (v_o == 1400){f <- 1.1}
      else if (v_o == 1600){f <- 0.5}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 800){
      if (v_o == 200){f <- 4.8}
      else if (v_o == 400){f <- 4.2}
      else if (v_o == 600){f <- 3.6}
      else if (v_o == 800){f <- 3.0}
      else if (v_o == 1000){f <- 2.4}
      else if (v_o == 1200){f <- 1.9}
      else if (v_o == 1400){f <- 1.3}
      else if (v_o == 1600){f <- 0.7}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1000){
      if (v_o == 200){f <- 4.9}
      else if (v_o == 400){f <- 4.4}
      else if (v_o == 600){f <- 3.8}
      else if (v_o == 800){f <- 3.2}
      else if (v_o == 1000){f <- 2.6}
      else if (v_o == 1200){f <- 2.0}
      else if (v_o == 1400){f <- 1.4}
      else if (v_o == 1600){f <- 0.8}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1200){
      if (v_o == 200){f <- 5.1}
      else if (v_o == 400){f <- 4.5}
      else if (v_o == 600){f <- 3.9}
      else if (v_o == 800){f <- 3.3}
      else if (v_o == 1000){f <- 2.7}
      else if (v_o == 1200){f <- 2.2}
      else if (v_o == 1400){f <- 1.6}
      else if (v_o == 1600){f <- 1.0}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1400){
      if (v_o == 200){f <- 5.2}
      else if (v_o == 400){f <- 4.7}
      else if (v_o == 600){f <- 4.1}
      else if (v_o == 800){f <- 3.5}
      else if (v_o == 1000){f <- 2.9}
      else if (v_o == 1200){f <- 2.3}
      else if (v_o == 1400){f <- 1.7}
      else if (v_o == 1600){f <- 1.1}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d >= 1600){
      if (v_o == 200){f <- 5.4}
      else if (v_o == 400){f <- 4.8}
      else if (v_o == 600){f <- 4.2}
      else if (v_o == 800){f <- 3.6}
      else if (v_o == 1000){f <- 3.0}
      else if (v_o == 1200){f <- 2.5}
      else if (v_o == 1400){f <- 1.9}
      else if (v_o == 1600){f <- 1.3}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else {f <- 'Error : [v_d] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
  }
  else if (P_TO == 0.6){
    if (v_d == 100){
      if (v_o == 200){f <- 4.6}
      else if (v_o == 400){f <- 4.0}
      else if (v_o == 600){f <- 3.4}
      else if (v_o == 800){f <- 2.8}
      else if (v_o == 1000){f <- 2.2}
      else if (v_o == 1200){f <- 1.6}
      else if (v_o == 1400){f <- 1.0}
      else if (v_o == 1600){f <- 0.5}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 200){
      if (v_o == 200){f <- 4.6}
      else if (v_o == 400){f <- 4.1}
      else if (v_o == 600){f <- 3.5}
      else if (v_o == 800){f <- 2.9}
      else if (v_o == 1000){f <- 2.3}
      else if (v_o == 1200){f <- 1.7}
      else if (v_o == 1400){f <- 1.1}
      else if (v_o == 1600){f <- 0.5}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 400){
      if (v_o == 200){f <- 4.8}
      else if (v_o == 400){f <- 4.2}
      else if (v_o == 600){f <- 3.6}
      else if (v_o == 800){f <- 3.0}
      else if (v_o == 1000){f <- 2.4}
      else if (v_o == 1200){f <- 1.9}
      else if (v_o == 1400){f <- 1.3}
      else if (v_o == 1600){f <- 0.7}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 600){
      if (v_o == 200){f <- 4.9}
      else if (v_o == 400){f <- 4.4}
      else if (v_o == 600){f <- 3.8}
      else if (v_o == 800){f <- 3.2}
      else if (v_o == 1000){f <- 2.6}
      else if (v_o == 1200){f <- 2.0}
      else if (v_o == 1400){f <- 1.4}
      else if (v_o == 1600){f <- 0.8}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 800){
      if (v_o == 200){f <- 5.1}
      else if (v_o == 400){f <- 4.5}
      else if (v_o == 600){f <- 3.9}
      else if (v_o == 800){f <- 3.3}
      else if (v_o == 1000){f <- 2.7}
      else if (v_o == 1200){f <- 2.2}
      else if (v_o == 1400){f <- 1.6}
      else if (v_o == 1600){f <- 1.0}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1000){
      if (v_o == 200){f <- 5.2}
      else if (v_o == 400){f <- 4.7}
      else if (v_o == 600){f <- 4.1}
      else if (v_o == 800){f <- 3.5}
      else if (v_o == 1000){f <- 2.9}
      else if (v_o == 1200){f <- 2.3}
      else if (v_o == 1400){f <- 1.7}
      else if (v_o == 1600){f <- 1.1}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1200){
      if (v_o == 200){f <- 5.4}
      else if (v_o == 400){f <- 4.8}
      else if (v_o == 600){f <- 4.2}
      else if (v_o == 800){f <- 3.6}
      else if (v_o == 1000){f <- 3.1}
      else if (v_o == 1200){f <- 2.5}
      else if (v_o == 1400){f <- 1.9}
      else if (v_o == 1600){f <- 1.3}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1400){
      if (v_o == 200){f <- 5.6}
      else if (v_o == 400){f <- 5.0}
      else if (v_o == 600){f <- 4.4}
      else if (v_o == 800){f <- 3.8}
      else if (v_o == 1000){f <- 3.2}
      else if (v_o == 1200){f <- 2.6}
      else if (v_o == 1400){f <- 2.0}
      else if (v_o == 1600){f <- 1.4}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d >= 1600){
      if (v_o == 200){f <- 5.7}
      else if (v_o == 400){f <- 5.1}
      else if (v_o == 600){f <- 4.5}
      else if (v_o == 800){f <- 3.9}
      else if (v_o == 1000){f <- 3.4}
      else if (v_o == 1200){f <- 2.8}
      else if (v_o == 1400){f <- 2.2}
      else if (v_o == 1600){f <- 1.6}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else {f <- 'Error : [v_d] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
  }
  else if (P_TO == 0.8){
    if (v_d == 100){
      if (v_o == 200){f <- 4.9}
      else if (v_o == 400){f <- 4.3}
      else if (v_o == 600){f <- 3.7}
      else if (v_o == 800){f <- 3.1}
      else if (v_o == 1000){f <- 2.5}
      else if (v_o == 1200){f <- 1.9}
      else if (v_o == 1400){f <- 1.3}
      else if (v_o == 1600){f <- 0.8}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 200){
      if (v_o == 200){f <- 4.9}
      else if (v_o == 400){f <- 4.4}
      else if (v_o == 600){f <- 3.8}
      else if (v_o == 800){f <- 3.2}
      else if (v_o == 1000){f <- 2.6}
      else if (v_o == 1200){f <- 2.0}
      else if (v_o == 1400){f <- 1.4}
      else if (v_o == 1600){f <- 0.8}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 400){
      if (v_o == 200){f <- 5.1}
      else if (v_o == 400){f <- 4.5}
      else if (v_o == 600){f <- 3.9}
      else if (v_o == 800){f <- 3.3}
      else if (v_o == 1000){f <- 2.7}
      else if (v_o == 1200){f <- 2.2}
      else if (v_o == 1400){f <- 1.6}
      else if (v_o == 1600){f <- 1.0}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 600){
      if (v_o == 200){f <- 5.2}
      else if (v_o == 400){f <- 4.7}
      else if (v_o == 600){f <- 4.1}
      else if (v_o == 800){f <- 3.5}
      else if (v_o == 1000){f <- 2.9}
      else if (v_o == 1200){f <- 2.3}
      else if (v_o == 1400){f <- 1.7}
      else if (v_o == 1600){f <- 1.1}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 800){
      if (v_o == 200){f <- 5.4}
      else if (v_o == 400){f <- 4.8}
      else if (v_o == 600){f <- 4.2}
      else if (v_o == 800){f <- 3.6}
      else if (v_o == 1000){f <- 3.1}
      else if (v_o == 1200){f <- 2.5}
      else if (v_o == 1400){f <- 1.9}
      else if (v_o == 1600){f <- 1.3}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1000){
      if (v_o == 200){f <- 5.6}
      else if (v_o == 400){f <- 5.0}
      else if (v_o == 600){f <- 4.4}
      else if (v_o == 800){f <- 3.8}
      else if (v_o == 1000){f <- 3.2}
      else if (v_o == 1200){f <- 2.6}
      else if (v_o == 1400){f <- 2.0}
      else if (v_o == 1600){f <- 1.4}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1200){
      if (v_o == 200){f <- 5.7}
      else if (v_o == 400){f <- 5.1}
      else if (v_o == 600){f <- 4.5}
      else if (v_o == 800){f <- 3.9}
      else if (v_o == 1000){f <- 3.4}
      else if (v_o == 1200){f <- 2.8}
      else if (v_o == 1400){f <- 2.2}
      else if (v_o == 1600){f <- 1.6}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1400){
      if (v_o == 200){f <- 5.9}
      else if (v_o == 400){f <- 5.3}
      else if (v_o == 600){f <- 4.7}
      else if (v_o == 800){f <- 4.1}
      else if (v_o == 1000){f <- 3.5}
      else if (v_o == 1200){f <- 2.9}
      else if (v_o == 1400){f <- 2.3}
      else if (v_o == 1600){f <- 1.7}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d >= 1600){
      if (v_o == 200){f <- 6.0}
      else if (v_o == 400){f <- 5.4}
      else if (v_o == 600){f <- 4.8}
      else if (v_o == 800){f <- 4.3}
      else if (v_o == 1000){f <- 3.7}
      else if (v_o == 1200){f <- 3.1}
      else if (v_o == 1400){f <- 2.5}
      else if (v_o == 1600){f <- 1.9}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else {f <- 'Error : [v_d] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
  }
  else if (P_TO == 1.0){
    if (v_d == 100){
      if (v_o == 200){f <- 5.2}
      else if (v_o == 400){f <- 4.6}
      else if (v_o == 600){f <- 4.0}
      else if (v_o == 800){f <- 3.4}
      else if (v_o == 1000){f <- 2.8}
      else if (v_o == 1200){f <- 2.2}
      else if (v_o == 1400){f <- 1.7}
      else if (v_o == 1600){f <- 1.1}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 200){
      if (v_o == 200){f <- 5.3}
      else if (v_o == 400){f <- 4.7}
      else if (v_o == 600){f <- 4.1}
      else if (v_o == 800){f <- 3.5}
      else if (v_o == 1000){f <- 2.9}
      else if (v_o == 1200){f <- 2.3}
      else if (v_o == 1400){f <- 1.7}
      else if (v_o == 1600){f <- 1.1}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 400){
      if (v_o == 200){f <- 5.4}
      else if (v_o == 400){f <- 4.8}
      else if (v_o == 600){f <- 4.2}
      else if (v_o == 800){f <- 3.6}
      else if (v_o == 1000){f <- 3.1}
      else if (v_o == 1200){f <- 2.5}
      else if (v_o == 1400){f <- 1.9}
      else if (v_o == 1600){f <- 1.3}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 600){
      if (v_o == 200){f <- 5.6}
      else if (v_o == 400){f <- 5.0}
      else if (v_o == 600){f <- 4.4}
      else if (v_o == 800){f <- 3.8}
      else if (v_o == 1000){f <- 3.2}
      else if (v_o == 1200){f <- 2.6}
      else if (v_o == 1400){f <- 2.0}
      else if (v_o == 1600){f <- 1.4}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 800){
      if (v_o == 200){f <- 5.7}
      else if (v_o == 400){f <- 5.1}
      else if (v_o == 600){f <- 4.5}
      else if (v_o == 800){f <- 3.9}
      else if (v_o == 1000){f <- 3.4}
      else if (v_o == 1200){f <- 2.8}
      else if (v_o == 1400){f <- 2.2}
      else if (v_o == 1600){f <- 1.6}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1000){
      if (v_o == 200){f <- 5.9}
      else if (v_o == 400){f <- 5.3}
      else if (v_o == 600){f <- 4.7}
      else if (v_o == 800){f <- 4.1}
      else if (v_o == 1000){f <- 3.5}
      else if (v_o == 1200){f <- 2.9}
      else if (v_o == 1400){f <- 2.3}
      else if (v_o == 1600){f <- 1.8}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1200){
      if (v_o == 200){f <- 6.0}
      else if (v_o == 400){f <- 5.4}
      else if (v_o == 600){f <- 4.8}
      else if (v_o == 800){f <- 4.3}
      else if (v_o == 1000){f <- 3.7}
      else if (v_o == 1200){f <- 3.1}
      else if (v_o == 1400){f <- 2.5}
      else if (v_o == 1600){f <- 1.9}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d == 1400){
      if (v_o == 200){f <- 6.2}
      else if (v_o == 400){f <- 5.6}
      else if (v_o == 600){f <- 5.0}
      else if (v_o == 800){f <- 4.4}
      else if (v_o == 1000){f <- 3.8}
      else if (v_o == 1200){f <- 3.2}
      else if (v_o == 1400){f <- 2.6}
      else if (v_o == 1600){f <- 2.1}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else if (v_d >= 1600){
      if (v_o == 200){f <- 6.3}
      else if (v_o == 400){f <- 5.7}
      else if (v_o == 600){f <- 5.1}
      else if (v_o == 800){f <- 4.6}
      else if (v_o == 1000){f <- 4.0}
      else if (v_o == 1200){f <- 3.4}
      else if (v_o == 1400){f <- 2.8}
      else if (v_o == 1600){f <- 2.2}
      else {f <- 'Error : [v_o] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
    }
    else {f <- 'Error : [v_d] must be one of 200, 400, 600, 800, 1000, 1200, 1400, or 1600 <= (vph). Please check that.'}
  }
  else {f <- 'Error : [P_TO] must be one of 0.2, 0.4, 0.6, 0.8, 1.0. Please check that.'}
  f
}
