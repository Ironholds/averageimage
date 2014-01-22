#Class for file validation
FileClass <- setRefClass(Class = "FileClass",
                         fields = list(
                           data = "character",
                           sample = "numeric"),
                         methods = list(
                           
                           #A function for testing the validity of file strings
                           TestString = function(){
                             
                             filematches <- grepl(x = .self$data, pattern = "(png|jpeg|jpg)")
                             
                             #If there are fewer valid entries than the requested sample..
                             if(sum(filematches) < .self$sample & sum(filematches) > 0){
                               
                               #Warn
                               warning("There are fewer valid entries than specified in the 'sample' parameter.")
                               
                               #Restrict input data and reduce sample size
                               .self$data <- .self$data[filematches,]
                               .self$sample <- sum(filematches)
                               
                             #The valid matches might also be zero
                             } else if(sum(filematches) == 0){
                               
                               stop("There are no valid filenames or URLs")
                              
                             #Otherwise...
                             } else {
                               
                               #Restrict data
                               .self$data <- .self$data[filematches,]
                               
                             }
                           },
                           
                           TestCon = function(){
                             
                             #Check for the file's existence
                             exists <- file.exists(.self$data)
                             
                             #If there are fewer existing files than sample size, but more than none..
                             if(sum(exists) < .self$sample & sum(exists) > 0){
                               
                               #Warn
                               warning("There are fewer valid filenames than specified in the 'sample' parameter.")
                               
                               #Reduce object size (and sample size)
                               .self$data <- .self$data[exists,]
                               .self$sample <- sum(exists)
                              
                             #If there are no matches...
                             } else if( sum(exists) == 0){
                               
                               #Stop
                               stop("There are no valid filenames")
                               
                             } else {
                               
                               #Restrict data
                               .self$data <- .self$data[filematches,]
                               
                             }
                           },
                           
                           validate = function(){
                             
                             if(length(.self$data > 0)){
                               
                               #Validate file names
                               .self$TestString()
                              
                               #Validate files, full stop
                               .self$TestCon()
                               
                             } else {
                               
                               stop("you have not submitted any file names or URLs")
                               
                             }
                             
                           }
                        )
)

#Class for URLs; inherits from FileClass
URLClass <- setRefClass(Class = "URLClass",
                        contains = "FileClass",
                        methods = list(
                          
                          #Test connections, shadowing FileClass$TestCon
                          TestCon = function(){
                            
                            TestCon = function(){
                              
                              #Check for the file's existence
                              exists <- url.exists(.self$data)
                              
                              #If there are fewer existing files than sample size, but more than none..
                              if(sum(exists) < .self$sample & sum(exists) > 0){
                                
                                #Warn
                                warning("There are fewer valid URLs than specified in the 'sample' parameter.")
                                
                                #Reduce object size (and sample size)
                                .self$data <- .self$data[exists,]
                                .self$sample <- sum(exists)
                                
                                #If there are no matches...
                              } else if( sum(exists) == 0){
                                
                                #Stop
                                stop("There are no valid URLs")
                                
                              } else {
                                
                                #Restrict data
                                .self$data <- .self$data[filematches,]
                              } 
                          }
                          )
)