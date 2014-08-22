#'@importFrom jpeg readJPEG writeJPEG
#'@importFrom png readPNG writePNG
type_checker <- function(x){
  
  #Check what they match
  is_png <- grepl(x = x, pattern = "png$", ignore.case = TRUE, perl = TRUE)
  
  #If they're all PNGs, return the readPNG function
  if(sum(is_png) == length(x)){
    
    return(list(reader = readPNG,
                writer = writePNG))
    
  }
  
  #Otherwise, check for JPEG
  is_jpg <- grepl(x = x, pattern = "(jpg|JPEG)$", ignore.case = TRUE, perl = TRUE)
  if(sum(is_jpg) == length(x)){
    
    return(list(reader = readJPEG,
                writer = writeJPEG))
    
  }
  
  #If neither matched, stop with an appropriate error
  stop("Not all of the provided URLs/filenames matched a single image type. Either they are mixed JPEGs/PNGs, or you have provided non-images")
}