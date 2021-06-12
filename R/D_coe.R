#' Directional Distribution Factor(D_coe)
#'
#' Directional Distribution Factor (D_coe) is defined as the ratio of the middle-direction traffic volume to the two-way traffic volume, and is related to the distribution of traffic volume by direction.
#'     D is necessary because there are cases where the distribution of traffic volume by direction is clearly different, such as during peak hours.
#'     If a road is designed without considering the direction with high traffic volume (medium direction), there is a risk of constant traffic congestion in the direction with high traffic volume (medium direction). should reflect
#'     In general, the value of urban roads is closer to 0.5 than that of rural roads. This is because there is a lot of traffic for various purposes other than commuting traffic. The peak time used for design or operation analysis shows a fluctuation range of 0.55 to 0.70.
#'     D is derived according to the project by using regular traffic volume survey data for road sections with similar traffic demand fluctuation characteristics around the design target road. However, if you cannot go through this process, you can use this function.
#'     It follows <Table 1-4> in KHCM(2013), p.7
#' @param region *Categorical* Region type classification. Choose one from : \code{'urban'}, \code{'local'}
#' @param output *Categorical* Type of result value. \code{'max'}, \code{'min'}, \code{'mean'}
#' @keywords Directional Distribution Factor
#' @export D_coe Directional Distribution Factor
#' @examples
#' D_coe(region = 'urban', output = 'mean')
#' D_coe('local', 'max')
D_coe <- function(region = NULL, output = NULL){
  if (region == 'urban'){
    if (output == 'mean'){D <- 0.60}
    else if (output == 'min'){D <- 0.55}
    else if (output == 'max'){D <- 0.65}
    else{D <- 'Error : The "output" value must be one of "mean", "min", "max".'}
  }
  else if (region == 'local'){
    if (output == 'mean'){D <- 0.65}
    else if (output == 'min'){D <- 0.60}
    else if (output == 'max'){D <- 0.70}
    else{D <- 'Error : The "output" value must be one of "mean", "min", "max".'}
  }
  else{D <- 'Error : The "region" value must be one of "urban" or "local".'}
  D
}
