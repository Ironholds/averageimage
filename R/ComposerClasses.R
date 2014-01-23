# Copyright (c) 2013 Oliver Keyes
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
PNGClass <- setRefClass(Class = "PNGClass",
                        fields = list(
                          imagenames = "character",
                          retrieval_type = "character",
                          savefile = "character",
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
                            
                            #Grab the dimensions of the object
                            array_dims <- lapply(.self$images, dim)
                            
                            #Form into a dataframe
                            array_dims <- do.call("rbind", array_dims)
                            
                            #Identify minimum values
                            min_x <- min(array_dims[,1])
                            min_y <- min(array_dims[,2])
                            min_depth <- min(array_dims[,3])
                            
                            #Trim each entry
                            .self$images <- lapply(.self$images, function(x){
                              
                              #If the x-axis is wider than the minimum x value...
                              if(dim(x)[1] != min_x){
                                
                                #Calculate the pixels to take, orienting around the middle of the image
                                x_ceiling <- ceiling((dim(x)[1] - min_x)/2)
                                x_floor <- floor((dim(x)[1] - min_x)/2)
                                x_range <- x_ceiling:((dim(x)[1] - x_floor)-1)
                                
                                #Otherwise...
                              } else {
                                
                                #Just take everything
                                x_range <- 1:dim(x)[1]
                                
                              }
                              
                              #Same for Y-axis
                              if(dim(x)[2] != min_y){
                                
                                y_ceiling <- ceiling((dim(x)[2] - min_y)/2)
                                y_floor <- floor((dim(x)[2] - min_y)/2)
                                y_range <- y_ceiling:((dim(x)[2] - y_floor)-1)
                                
                              } else {
                                
                                y_range <- 1:dim(x)[2]
                                
                              }
                              
                              #Restrict dimensions to the minimum of the images in the filtered set.
                              x <- x[x_range,
                                     y_range,
                                     1:min_depth]
                              
                              #Return!
                              return(x)
                            })
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
                                     target = .self$savefile)
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
                                      target = .self$savefile,
                                      quality = 1)
                          }
                        )
)