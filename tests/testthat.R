library(testthat)
library(averageimage)

test_check("averageimage")
read_images <- lapply(c("first_image.JPG",
                        "second_image.JPG"))