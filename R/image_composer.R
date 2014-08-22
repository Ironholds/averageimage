image_composer <- function(images){
  
  #Create blank exit point
  composite <- NULL
  
  for(i in seq_along(images)){
    
    #If this is the first image, throw it in entirely
    if(is.null(composite)){
      
      composite <- images[[i]]
      
    } else {
      
      #Otherwise, sum the values
      composite <- composite + images[[i]]
      
    }
  }
  
  #Divide the values
  composite <- composite/length(images)
  
  #Return
  return(composite)
}