compJPEG <- function(file.list = NULL, url.list = NULL, save.file, sample = NULL){
  
  #Assume that files are the preferred option; people who submit both deserve what they get
  if(!is.null(file.list)){
    
    #Assign to the file class
    Validation.Obj <- FileClass$new(data = file.list,
                              sample = sample)
    
  #If they've not submitted files, but have submitted URLs..
  } else if(!is.null(url.list)){
    
    #Assign to the URL class
    Validation.Obj <- URLClass$new(data = url.list,
                              sample = sample)
  
  #If you haven't provided files OR URLs, you didn't read the documentation and deserve precisely what you get.
  } else {
  
    stop("You have not provided any file names or URLs")
    
  }
  
  #Either way, validate them
  validation.Obj$validate()
  
  #Take the resulting data, add it to the JPEGClass.
  JPEG.Obj <- JPEGClass$new(data = Validation.Obj$data,
                            type = class(Validation.Obj)[1])
  
  #Generate the image
  JPEG.Obj$generator()
  
  #Save it
  writeJPEG(JPEG.Obj$data, target = save.filem quality = 1)
}