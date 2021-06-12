#' Design Hourly Factor
#'
#' A coefficient that determines how much traffic volume is selected as the Design Hourly Volume (DHV) for the planned target year among the one-hour traffic volume distribution on the road.
#'     The Design Hourly Factor (K) changes according to the characteristics of the traffic demand change of the road.
#'     The larger the design time coefficient, the greater the fluctuation in traffic demand.
#'     As the average annual daily traffic (AADT) increases, the Design Hourly Factor (K) of the corresponding road section decreases.
#'     In general, the design time coefficient is largest for roads in tourist areas, and tends to decrease in the order of local roads - urban roads.
#'     Therefore, K should be determined in consideration of the characteristics of traffic demand fluctuations according to the characteristics of the regions through which roads pass and connect when dividing regions.
#'     It follows <Table 1-3> in KHCM(2013), p.6.
#'
#' @param region *Categorical* Region type classification. Choose one from : \code{'urban'}, \code{'local'}, \code{'tourist_area'}
#' @param road *Categorical* Road type. Choose one from : \code{'general'}(General national road), \code{'expressway'}(Expressway with more than 4 lanes)
#' @param lane *Numeric* The number of round-trip lanes.. It should \code{2} or \code{4} or more.
#' @param output *Categorical* Type of result value. \code{'max'}, \code{'min'}, \code{'mean'}
#' @keywords Design Hourly Factor K
#' @export K
#' @examples
#' K(region = 'urban', road = 'general', lane = 2, output = 'mean')
#' K('local', 'expressway', 4, 'max')
K <- function(region = NULL, road = NULL, lane = NULL, output = NULL){
  if(region == 'urban'){
    if(road == 'general'){
      if(lane == 2){
        if (output == 'mean'){k <- 0.12}
        else if (output == 'min'){k <- 0.10}
        else if (output == 'max'){k <- 0.14}
        else{k <- 'Error : The output value must be one of "mean", "min", "max".'}
      }
      else if(lane >= 4){
        if (output == 'mean'){k <- 0.10}
        else if (output == 'min'){k <- 0.07}
        else if (output == 'max'){k <- 0.12}
        else{k <- 'Error : The output value must be one of "mean", "min", "max".'}
      }
      else(k <- 'Error : The "Lane" value must be 2 or more than 4.')
    }
    else if(road == 'expressway'){
      if (lane >= 4){
        if (output == 'mean'){k <- 0.10}
        else if (output == 'min'){k <- 0.07}
        else if (output == 'max'){k <- 0.13}
        else{k <- 'Error : The output value must be one of "mean", "min", "max".'}
      }
      else{k <- 'Error : The lane value must be more than 4'}
    }
    else{k <- 'Error : The road value must be one of "general" or "expressway".'}
  }
  else if(region == 'local'){
    if(road == 'general'){
      if(lane == 2){
        if (output == 'mean'){k <- 0.16}
        else if (output == 'min'){k <- 0.13}
        else if (output == 'max'){k <- 0.20}
        else{k <- 'Error : The output value must be one of "mean", "min", "max".'}
      }
      if(lane >= 4){
        if (output == 'mean'){k <- 0.12}
        else if (output == 'min'){k <- 0.09}
        else if (output == 'max'){k <- 0.15}
        else{k <- 'Error : The output value must be one of "mean", "min", "max".'}
      }
      else{k <- 'Error : The lane value must be 2 or more than 4.'}
    }
    if(road == 'expressway'){
      if (lane >= 4){
        if (output == 'mean'){k <- 0.14}
        else if (output == 'min'){k <- 0.09}
        else if (output == 'max'){k <- 0.19}
        else{k <- 'Error : The lane value must be more than 4'}
      }
      else{k <- 'Error : The lane value must be more than 4'}
    }
    else{k <- 'Error : The road value must be one of "general" or "expressway".'}
  }
  else if(region == 'tourist_area'){
    if(road == 'general'){
      if(lane == 2){
        if (output == 'mean'){k <- 0.23}
        else if (output == 'min'){k <- 0.18}
        else if (output == 'max'){k <- 0.28}
        else{'Error : The output value must be one of "mean", "min", "max".'}
      }
      else if(lane >= 4){
        if (output == 'mean'){k <- 0.14}
        else if (output == 'min'){k <- 0.12}
        else if (output == 'max'){k <- 0.17}
        else{'Error : The output value must be one of "mean", "min", "max".'}
      }
      else{'Error : The lane value must be 2 or more than 4.'}
    }
    if(road == 'expressway'){
      if (lane >= 4){
        if (output == 'mean'){k <- 0.14}
        else if (output == 'min'){k <- 0.09}
        else if (output == 'max'){k <- 0.19}
        else{'Error : The output value must be one of "mean", "min", "max".'}
      }
      else{'Error : The lane value must be more than 4'}
    }
    else{'Error : The road value must be one of "general" or "expressway".'}
  }
  else{k <- 'Error : The region value must be one of "urban", "local", or "tourist_area".'}
  k
}

