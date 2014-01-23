meanit <- function(file.list = NULL, url.list = NULL, save.file, sample = NULL, imagetype){
  
  #Assume that files are the preferred option; people who submit both deserve what they get
  if(!is.null(file.list)){
    
    #If the sample is NULL
    if(is.null(sample)){
      
      #Assume that the sample is, well, everything
      sample <- length(file.list)
      
    }
    
    #Assign to the file class
    Validation.Obj <- FileClass$new(data = file.list,
                              sample = sample)
    
  #If they've not submitted files, but have submitted URLs..
  } else if(!is.null(url.list)){
    
    #If the sample is NULL
    if(is.null(sample)){
      
      #Assume that the sample is, well, everything
      sample <- length(url.list)
      
    }
    
    #Assign to the URL class
    Validation.Obj <- URLClass$new(data = url.list,
                              sample = sample)
  
  #If you haven't provided files OR URLs, you didn't read the documentation.
  } else {
  
    stop("You have not provided any file names or URLs")
    
  }
  
  #Either way, validate them
  Validation.Obj$validate()
  
  #Take the resulting data, decide what class to add it to.
  switch(imagetype,
         "jpeg" = {
           
           #Add to the JPEGClass
           Composer.Obj <- JPEGClass$new(imagenames = Validation.Obj$data,
                                         retrieval_type = class(Validation.Obj)[1],
                                         savefile = save.file)
         },
         "png" = {
           
           #Add to the PNGClass
           Composer.Obj <- PNGClass$new(imagenames = Validation.Obj$data,
                                        retrieval_type = class(Validation.Obj)[1],
                                        savefile = save.file)
         })
  
  #Generate and save the image
  Composer.Obj$generator()
  
}