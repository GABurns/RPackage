test_that("No errors or warnings returned", {
  expect_silent(CalculateMeans(data = mtcars))
})

test_that("Error return on missing input", {
  expect_error(CalculateMeans())
})

test_that("Function returns expected values", {
  # Create calculatedValues from function
  calculatedValues <- CalculateMeans(data = attitude)

  # Calculate the expected Values
  expectedValues <- sapply(attitude, mean)

  expect_identical(calculatedValues, expectedValues)
})

test_that("Function returns expected values: Edge Cases", {
  # EdgeCase1: Contains NAs
  mockData <- attitude
  mockData$rating[1] <- NA_real_


  # Create calculatedValues from function
  calculatedValues <- CalculateMeans(data = mockData)

  # Manually calculated and hardcoded
  expectedValues <-
    c(
      "rating" = 65.37931,
      "complaints" = 66.60000,
      "privileges" = 53.13333,
      "learning" = 56.36667,
      "raises" = 64.63333,
      "critical" = 74.76667,
      "advance" = 42.93333
    )

  # Tolerance argument may be needed for negligible differences
  expect_identical(calculatedValues, expectedValues, tolerance = 0.00001)

  # EdgeCase2: character value in dataset
  mockData <- attitude
  mockData$character <- "Character"


  # A warning is expected to inform us the means aren't calculated
  calculatedValues <- expect_warning(CalculateMeans(data = mockData),
    regexp = "The row character contains non-numeric data did not have mean calculated"
  )

  # Create calculatedValues from function
  calculatedValues <- suppressWarnings(CalculateMeans(data = mockData))

  # The expected values does not have a "character" value as a mean can't be
  # calculated for this
  expectedValues <-
    c(
      "rating" = 64.63333,
      "complaints" = 66.60000,
      "privileges" = 53.13333,
      "learning" = 56.36667,
      "raises" = 64.63333,
      "critical" = 74.76667,
      "advance" = 42.93333
    )

  # Tolerance argument may be needed for negligible differences
  expect_identical(calculatedValues, expectedValues, tolerance = 0.00001)

  # Edgecase 3: Data.frame with single column
  mockData <- data.frame(SingleColumn = c(1, 2, 3))

  expectedValues <- c("SingleColumn" = 2)
  calculatedValues <- CalculateMeans(data = mockData)
  expect_identical(calculatedValues, expectedValues)
})
