#'@importFrom downloader download
retrieve_from_url <- function(url, handler){
  
  #Identify plausible temp file address.
  file_location <- tempfile()
  
  #Try to download image.
  download(url = url, destfile = file_location, quiet = TRUE)
  
  #Grab it from the temp file.
  image <- retrieve_from_file(filename = file_location,
                              handler = handler)
  
  #remove file and return image.
  file.remove(file_location)
  return(image)
}