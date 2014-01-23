PNGClass <- setRefClass(Class = "PNGClass",
                        fields = list(
                          imagenames = "character",
                          retrieval_type = "character",
                          images = "list"),
                        methods = list(
                          
                          #Method for reading the image in
                          reader = function(){
                            
                            .self$images <- lapply(.self$imagenames, function(x){
                              
                              #For URLs...
                              if(retrieval_type == "URLClass"){
                                
                                #Download the file
                                download.file(url = x,
                                              destfile = "tempfile.png",
                                              quiet = TRUE)
                                
                                #Read it in from the temporary location
                                image_array <- readPNG("tempfile.png")
                                
                                #Delete the temporary file
                                file.remove("tempfile.png")
                                
                              #For files..
                              } else {
                                
                                #Just read it straight in
                                image_array <- readPNG(x)
                                
                              }
                              
                              #Return
                              return(image_array)
                              
                            })
                            
                          },
                          
                          #Method for trimming down odd-sized images
                          trimmer = function(){
                            
                            
                          },
                          
                          #Method for actually, well, generating the composites ;p
                          compositor = function(){
                            
                            #Image to export
                            composite_image = NULL
                            
                            #For each image...
                            for(i in seq_along(.self$images)){
                              
                              if(is.null(composite_image)){
                                
                                #If it's the first image to be read, just shadow composite_image with it
                                composite_image <- .self$images[[i]]
                              
                              } else {
                                
                                #Otherwise, add the values
                                composite_image <- composite_image + .self$images[[i]]
                              }
                              
                            }
                            
                            #Average
                            composite_image <- composite_image/length(.self$images)
                            
                            #Return
                            return(composite_image)
                          },
                          
                          #General method
                          generator = function(){
                            
                            #Read files in
                            .self$reader()
                            
                            #Trim them down
                            .self$trimmer()
                            
                            #Generate a composite
                            composite_image <- .self$compositor()
                            
                            #Save it
                            writePNG(image = composite_image,
                                     target = save.file)
                          }
                        
                        )
)

JPEGClass <- setRefClass(Class = "JPEGClass",
                         contains = "PNGClass",
                         methods = list(
                           
                          #Method for reading the image in - shadows PNGClass$reader()
                          reader = function(){
                            
                            .self$images <- lapply(.self$imagenames, function(x){
                              
                              #For URLs...
                              if(retrieval_type == "URLClass"){
                                
                                #Download the file
                                download.file(url = x,
                                              destfile = "tempfile.jpeg",
                                              quiet = TRUE)
                                 
                                #Read it in from the temporary location
                                image_array <- readJPEG("tempfile.jpeg")
                                 
                                #Delete the temporary file
                                file.remove("tempfile.jpeg")
                                 
                                #For files..
                              } else {
                                 
                                #Just read it straight in
                                image_array <- readJPEG(x)
                                 
                              }
                               
                              #Return
                              return(image_array)
                               
                            })
                             
                          },
                           
                          
                          #Grouped function - shadows PNGClass$generator()
                          generator = function(){
                             
                            #Read files in
                            .self$reader()
                            
                            #Trim them down
                            .self$trimmer()
                            
                            #Generate a composite
                            composite_image <- .self$compositor()
                             
                            #Save it
                            writeJPEG(image = composite_image,
                                      target = save.file,
                                      quality = 1)
                           }
                        )
)

TIFFClass <- setRefClass(Class = "TIFFClass",
                         contains = "PNGClass",
                         methods = list(
                           
                         )
)

BMPClass <- setRefClass(Class = "BMPClass",
                        contains = "PNGClass",
                        methods = list(
                          
                        )
)