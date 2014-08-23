test_that("image composition works correctly with two identically-sized images", {
  
  #Example set.
  example_set <- list(array(data = 12, dim = c(12, 30,2)),
                        array(data = 14, dim = c(12, 30,2)))
  
  #Generate composite
  composite <- image_composer(example_set)
  
  #Test resulting value
  expect_that(sum(unlist(composite)), equals(9360))
  
})

test_that("image composition does not drop dimensions", {
  
  #Example set.
  example_set <- list(array(data = 12, dim = c(12, 30,2)),
                      array(data = 14, dim = c(12, 30,2)))
  
  #Generate composite
  composite <- image_composer(example_set)
  
  #Test resulting value
  expect_that(length(dim(composite)), equals(3))
  
})