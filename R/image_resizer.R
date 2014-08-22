image_resizer <- function(image, dims){
    
  #If the x-axis is wider than the minimum x value...
  if(dim(image)[1] != dims[1]){
    
    x_range <- range_calc(min_val = dims[1], val = dim(image)[1])
    
    #Otherwise...
  } else {
    
    #Just take everything
    x_range <- 1:dim(image)[1]
    
  }
  
  #Same for Y-axis
  if(dim(image)[2] != dims[2]){
    
    y_range <- range_calc(min_val = dims[2], val = dim(image)[2])
    
  } else {
    
    y_range <- 1:dim(image)[2]
    
  }
  
  #Restrict dimensions to the minimum of the images in the set.
  image <- image[x_range,
                 y_range,
                 1:dim(image)[3]]
  
  #Return!
  return(image)

}