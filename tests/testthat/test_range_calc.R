context("Image range calculation")
test_that("range generation generates objects of the corrent length", {
  
  #Length of output
  expect_that(length(range_calc(12, 300)), equals(12))
  
})

test_that("range generation generates objects of the corrent value", {
  
  #Value
  expect_that(sum(range_calc(12, 300)), equals(1794))
  
})