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
#'@title meanit
#'@description Generate composite images from a list of PNGs or JPEGs
#'@details Meanit is the primary function of the package, and ties the internal methods and functions
#'together in generating a composite image. A vector of absolute file names or URLs is provided, and meanit
#'validates that the strings look like images, validates that they resolve to a valid address, and
#'then retrieves the images and generates a composite of them.
#'
#'To deal with images of different sizes, the smallest dimensions of listed images are calculated, and
#'each image is then trimmed to match those dimensions. This trimming operates from the middle of the image,
#'rather than any edge, in order to prioritise retaining the 'important' bits.
#'
#'@section Caveats: Meanit accepts both PNGs (greyscale or full) and JPEGs, although the list of
#'files or URLs provided should only refer to one type of image. If JPEGs and PNGs are combined, it won't
#'work - if full and grayscale PNGs are provided, the returned image will be grayscale.
#'@param files a vector of absolute file names that contain PNGs or JPEGs
#'@param urls a vector of absolute URLs that refer to PNGs or JPEGs. Priority is given to 
#'files, not urls; submit one or the other.
#'@param save.file the absolute file name to save the composite image into
#'@param sample the number of files or URLs to take from the validated list. If it's left as NULL,
#'meanit assumes you want all of them.
#'@param imagetype the format of the image. Currently accepts "jpeg" or "png"
#'@export 
meanit <- function(files = NULL, urls = NULL, save.file, sample = NULL, imagetype){
  
  #Assume that files are the preferred option; people who submit both deserve what they get
  if(!is.null(files)){
    
    #If the sample is NULL
    if(is.null(sample)){
      
      #Assume that the sample is, well, everything
      sample <- length(files)
      
    }
    
    #Assign to the file class
    Validation.Obj <- FileClass$new(data = files,
                              sample = sample)
    
  #If they've not submitted files, but have submitted URLs..
  } else if(!is.null(urls)){
    
    #If the sample is NULL
    if(is.null(sample)){
      
      #Assume that the sample is, well, everything
      sample <- length(urls)
      
    }
    
    #Assign to the URL class
    Validation.Obj <- URLClass$new(data = urls,
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