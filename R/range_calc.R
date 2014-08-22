range_calc <- function(min_val, val){
  
  #Calculate a range
  r_ceiling <- ceiling((val - min_val)/2)
  r_floor <- floor((val - min_val)/2)
  range <- r_ceiling:((val - r_floor)-1)
  
  #Return
  return(range)
}