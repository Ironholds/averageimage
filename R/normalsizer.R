normalsizer <- function(images){
  
  #Retrieve dimensions
  dimensions <- lapply(images, dim)
  dimensions <- data.frame(matrix(unlist(dimensions),
                                  nrow = length(dimensions),
                                  byrow = TRUE),
                           stringsAsFactors = FALSE)
  img_dimnames <- names(dimensions)
  
  #Work out minimum dimensions in the set.
  #Use a loop to avoid running into two-versus-three problems.
  min_dims <- integer()
  for(i in 1:2){
    
    min_dims[i] <- min(dimensions[,img_dimnames[i]])
    
  }
  
  #Resize
  images <- lapply(X = images, FUN = image_resizer, dims = min_dims)
  
  #Return
  return(images)
}