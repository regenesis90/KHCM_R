#' The Inlet or Outlet Capacity of the Freeway Ramp Junction(pcph)
#'
#' This function decides inlet or outlet capacity of the freeway ramp junction(pcph). It follows <Table 4-1> in KHCM(2013).
#' @param free_speed *Numeric* It means free speed of main lane.(kph)
#' @param V *Numeric* Main line traffic volume of branch and confluence section(pcph)
#' @param N *Numeric* Total number of lanes(one-way). It must be 2 or more.
#' @param inlet_outlet *Categirocal* Choose output. \code{inlet}, \code{outlet}
#' @keywords capacity freeway ramp junction
#' @export influence_capacity_freeway_ramp_junction The capacity of the freeway ramp junction(pcph).
#' @examples
#' influence_capacity_freeway_ramp_junction(free_speed = 120, V = 5000, N = 5, inlet_outlet = 'outlet')
#' influence_capacity_freeway_ramp_junction(100, 4000, 3, 'inlet')
influence_capacity_freeway_ramp_junction <- function(free_speed = NULL, V = NULL, N = NULL, inlet_outlet = NULL){
  if (free_speed > 110 & free_speed <= 120){
    if (N == 2){
      if (V >= 0 & V <= 4600){
        if (inlet_outlet == 'outlet'){cap <- 4400}
        if (inlet_outlet == 'inlet'){cap <- 4600}
      }
      else {cap <- 'Impossible Analysis'}
    }
    if (N == 3){
      if (V >= 0 & V <= 6800){
        if (inlet_outlet == 'outlet'){cap <- 4400}
        if (inlet_outlet == 'inlet'){cap <- 4600}
      }
      else {cap <- 'Impossible Analysis'}
    }
    if (N >= 4){
      if (V >= 0 & V <= 2300*N){
        if (inlet_outlet == 'outlet'){cap <- 4400}
        if (inlet_outlet == 'inlet'){cap <- 4600}
      }
      else {cap <- 'Impossible Analysis'}
    }
  }
  if (free_speed > 100 & free_speed <= 110){
    if (N == 2){
      if (V >= 0 & V <= 4500){
        if (inlet_outlet == 'outlet'){cap <- 4400}
        if (inlet_outlet == 'inlet'){cap <- 4600}
      }
      else {cap <- 'Impossible Analysis'}
    }
    if (N == 3){
      if (V >= 0 & V <= 6750){
        if (inlet_outlet == 'outlet'){cap <- 4400}
        if (inlet_outlet == 'inlet'){cap <- 4600}
      }
      else {cap <- 'Impossible Analysis'}
    }
    if (N >= 4){
      if (V >= 0 & V <= 2250*N){
        if (inlet_outlet == 'outlet'){cap <- 4400}
        if (inlet_outlet == 'inlet'){cap <- 4600}
      }
      else {cap <- 'Impossible Analysis'}
    }
  }
  if (free_speed > 90 & free_speed <= 100){
    if (N == 2){
      if (V >= 0 & V <= 4400){
        if (inlet_outlet == 'outlet'){cap <- 4400}
        if (inlet_outlet == 'inlet'){cap <- 4600}
      }
      else {cap <- 'Impossible Analysis'}
    }
    if (N == 3){
      if (V >= 0 & V <= 6600){
        if (inlet_outlet == 'outlet'){cap <- 4400}
        if (inlet_outlet == 'inlet'){cap <- 4600}
      }
      else {cap <- 'Impossible Analysis'}
    }
    if (N >= 4){
      if (V >= 0 & V <= 2200*N){
        if (inlet_outlet == 'outlet'){cap <- 4400}
        if (inlet_outlet == 'inlet'){cap <- 4600}
      }
      else {cap <- 'Impossible Analysis'}
    }
  }
  if (free_speed <= 90){
    if (N == 2){
      if (V >= 0 & V <= 4200){
        if (inlet_outlet == 'outlet'){cap <- 4400}
        if (inlet_outlet == 'inlet'){cap <- 4600}
      }
      else {cap <- 'Impossible Analysis'}
    }
    if (N == 3){
      if (V >= 0 & V <= 6300){
        if (inlet_outlet == 'outlet'){cap <- 4400}
        if (inlet_outlet == 'inlet'){cap <- 4600}
      }
      else {cap <- 'Impossible Analysis'}
    }
    if (N >= 4){
      if (V >= 0 & V <= 2100*N){
        if (inlet_outlet == 'outlet'){cap <- 4400}
        if (inlet_outlet == 'inlet'){cap <- 4600}
      }
      else {cap <- 'Impossible Analysis'}
    }
  }
  cap
}
