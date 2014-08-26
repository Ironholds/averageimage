context("Image type checking")
test_that("image type checking handles invalid entries", {
  
  #Test for handling invalid entries
  expect_that(type_checker("foo"), throws_error("Not all of the provided"))
  expect_that(type_checker(c("jpeg","png")), throws_error("Not all of the provided"))
  
})

test_that("image type checking retrieves PNGs correctly", {
  
  expect_that(body(type_checker("png")$reader), equals(body(readPNG)))
  expect_that(body(type_checker("PNG")$reader), equals(body(readPNG)))
  
})

test_that("image type checking retrieves JPEGs correctly", {
  
  expect_that(body(type_checker("jpg")$reader), equals(body(readJPEG)))
  expect_that(body(type_checker("JPG")$reader), equals(body(readJPEG)))
  expect_that(body(type_checker("jpeg")$reader), equals(body(readJPEG)))
  expect_that(body(type_checker("JPEG")$reader), equals(body(readJPEG)))
  
})