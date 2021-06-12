#' Number of conflicts on bike paths
#'
#' It follows <Formula 15-18> ~ <Formula 15-26> in KHCM(2013), p.649~651
#' @param type *Categorical* \code{'bike_only'}, \code{'bike_ped_comb'}, \code{'road_basic'}
#' @param way *Categorical* \code{'one_way'}, \code{'two_way'}
#' @param Q_bike Total, or Bidirectional Bicycle Traffic (vph)
#' @param Q_ped Total or Bi-directional pedestrian traffic (person/h)
#' @param U_bike Average bike speed (kph)
#' @param U_ped Average walking speed (kph)
#' @param sd Standard Deviation of Bike Speed (kph)
#' @param p_b Proportion of bicycle traffic in the direction of travel in total (bidirectional) bicycle traffic
#' @param p_p Proportion of pedestrian traffic in the forward direction in total (bidirectional) pedestrian traffic
#' @param output \code{'bike'}, \code{'pedestrian'}
#' @keywords
#' @export F_bike
#' @examples
#' F_bike(type = 'bike_only', way = 'two_way', Q_bike = 450, p_b = 0.3)
F_bike <- function(type = NULL, way = NULL, Q_bike = NULL, Q_ped = NULL, U_bike = NULL, U_ped = NULL, sd = NULL, p_b = NULL, p_p = NULL, output = NULL){
  if (type == 'bike_only'){
    if (is.null(U_bike) ==FALSE & is.null(sd) == FALSE){
      if (way == 'one_way'){f <- 2 * Q_bike * sd / (U_bike * (pi)**(1/2))}
      if (way == 'two_way'){f <- Q_bike * (1 + ((2 * sd / (U_bike * (pi)**(1/2))) - 1) * p_b)}
    }
    else{
      if (way == 'one_way'){f <- 0.25 * Q_bike}
      if (way == 'two_way'){f <- Q_bike * (1 - 0.75 * p_b)}
    }
  }
  if (type == 'bike_ped_comb'){
    if (is.null(U_bike) == FALSE & is.null(U_ped) == FALSE & is.null(sd) == FALSE){
      if (output == 'bike'){f <- Q_bike * ((2 * sd / (U_bike * (pi)**2)) * p_b + 1) + Q_ped * (0.5 * p_p * (U_bike/U_ped) - 1.5 * p_p + 0.5 * (1 + U_bike/U_ped))}
      if (output == 'pedestrian'){f <- Q_bike * (p_b * (1 - (U_ped/U_bike)) + 0.5 * (1 - p_b) * (1 + (U_ped/U_bike)))}
    }
    else{
      if (output == 'bike'){f <- Q_bike * (1 - 0.75 * p_b) + 2 * Q_ped}
      if (output == 'pedestrian'){f <- 2/3 * Q_bike}
    }
  }
  if (type == 'road_basic'){
    if (is.null(U_bike) == FALSE & is.null(sd) == FALSE){
      f <- 2 * Q_bike * sd / (U_bike * (pi)**(1/2))
    }
    else{
      f <- 0.25 * Q_bike
    }
  }
  f
}
