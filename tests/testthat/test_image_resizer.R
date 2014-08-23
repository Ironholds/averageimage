test_that("image resizing works correctly with both minimums in a single image", {
  
  #Example set; 1 with both minimums, 1 with neither.
  both_minimums <- list(array(dim = c(12, 30,2)),
                        array(dim = c(20, 45,2)))
  results <- lapply(both_minimums, image_resizer, dims = dim(both_minimums[[1]]))
  expect_that(dim(results[[1]]), equals(c(12,30,2)))
  expect_that(dim(results[[2]]), equals(c(12,30,2)))
  
})
  
test_that("image resizing works correctly with minimums split between images", {
  
  #Example set; 1 with both minimums, 1 with neither.
  split_minimums <- list(array(dim = c(12, 45,2)),
                        array(dim = c(20, 30,2)))
  
  dimensions <- lapply(split_minimums, dim)
  dimensions <- data.frame(matrix(unlist(dimensions),
                                  nrow = length(dimensions),
                                  byrow = TRUE),
                           stringsAsFactors = FALSE)
  img_dimnames <- names(dimensions)
  
  #Work out minimum dimensions in the set.
  #Use a loop to avoid running into two-versus-three problems.
  min_dims <- integer()
  for(i in 1:2){
    
    min_dims[i] <- min(dimensions[,img_dimnames[i]])
    
  }
  
  results <- lapply(split_minimums, image_resizer, dims = min_dims)
  
  
  expect_that(dim(results[[1]]), equals(c(12,30,2)))
  expect_that(dim(results[[2]]), equals(c(12,30,2)))
  
})