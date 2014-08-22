retrieve_from_file <- function(filename, handler){
  
  #Do what it says on the tin; retrieve a filename from a file, with an appropriate handler
  image <- handler(source = filename)
  
  #Return
  return(image)
}