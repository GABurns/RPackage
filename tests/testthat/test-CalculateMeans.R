test_that("No errors or warnings returned", {
  expect_silent(CalculateMeans(data = mtcars))
})

test_that("Error return on missing input", {
  expect_error(CalculateMeans())
})
