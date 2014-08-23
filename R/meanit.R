#'@title meanit
#'@description Generate composite images from a list of PNGs or JPEGs
#'@details meanit generates composite PNGs or JPEGs. To deal with images of different sizes, the smallest dimensions of listed images are calculated, and
#'each image is then trimmed to match those dimensions. This trimming operates from the middle of the image,
#'rather than any edge, in order to prioritise retaining the 'important' bits.
#'
#'@section Caveats and errors: Meanit accepts both PNGs (greyscale or full) and JPEGs, although the list of
#'files or URLs provided should only refer to one type of image. If JPEGs and PNGs are combined, it won't
#'work - if full and grayscale PNGs are provided, the returned image will be grayscale.
#'
#'For retrieving images from URLs, the package (and function) is dependent on RCurl. This means,
#'amongst other things, that https is not a supported protocol. URLs must be provided with http.
#'@param input any one of a vector of URLs, a vector of absolute file names, or a list of
#'already-read PNGs or JPEGs.
#'@param save.file the absolute file name to save the composite image into
#'@export 
meanit <- function(input, save.file = NULL){
  
  #If URLs are provided instead of files...
  if(length(grep(x = input[[1]], pattern = "http", ignore.case = TRUE)) > 0 ){
    
    #Check type and retrieve relevant function
    image_handler <- type_checker(x = urls)

    #Run retrieve_from_url with relevant handler
    images <- lapply(urls, retrieve_from_url, image_handler$reader)
  
  } else if(class(input[[1]]) == "array"){
    
    #If it's already read in, work out the handlers from save.file
    #Then simply use the input as the images
    image_handler <- type_checker(x = save.file)
    images <- input
    
  } else { 
    
    #Otherwise, check and retrieve from file with relevant handler
    image_handler <- type_checker(x = files)
    images <- lapply(files, retrieve_from_file, image_handler$reader)
    
  }
  
  #Work out dims and subsection image
  normalised_images <- normalsizer(images)
  
  #Compose
  composite <- image_composer(normalised_images)
  
  #If save.file = NULL, simply return the composite image as an array
  if(is.null(save.file)){
    
    return(composite)
    
  }
  
  #Otherwise, save and return TRUE
  image_handler$writer(image = composite, target = save.file)
  return(TRUE)
}