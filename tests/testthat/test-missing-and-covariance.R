test_that("covariance conversion preserves values", {
  correlation <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  covariance <- Cor2cov(correlation, sds = c(2, 3))

  expect_equal(unname(covariance), matrix(c(4, 3, 3, 9), nrow = 2))
  expect_equal(Cov2cor(covariance), correlation)
})

test_that("GetCov parses lower-triangle text and creates default names", {
  covariance <- GetCov("1 .5 1")

  expect_equal(dim(covariance), c(2, 2))
  expect_equal(colnames(covariance), c("V1", "V2"))
  expect_equal(rownames(covariance), c("V1", "V2"))
  expect_equal(unname(covariance), matrix(c(1, 0.5, 0.5, 1), nrow = 2))
})

test_that("GetCov converts correlations when standard deviations are supplied", {
  covariance <- GetCov("1 .5 1", sds = c(2, 3))

  expect_equal(unname(covariance), matrix(c(4, 3, 3, 9), nrow = 2))
})

test_that("missfillin preserves a data frame and imputes by column", {
  data <- data.frame(
    a = c(1, NA, 3),
    b = c(NA, 4, 6)
  )

  result <- missfillin(data)

  expect_s3_class(result, "data.frame")
  expect_equal(result$a, c(1, 2, 3))
  expect_equal(result$b, c(5, 4, 6))
})

test_that("missCheck works without optional plotting packages", {
  data <- data.frame(
    complete = 1:3,
    incomplete = c(1, NA, 3)
  )

  result <- missCheck(data)

  expect_equal(result$Variable, "incomplete")
  expect_equal(result$V_count, 2)
  expect_equal(result$Count, 1)
})

test_that("missfillinFor changes only selected columns", {
  data <- data.frame(
    a = c(1, NA, 3),
    b = c(NA, 4, 6)
  )

  result <- missfillinFor(data, range = 1)

  expect_equal(result$a, c(1, 2, 3))
  expect_true(is.na(result$b[1]))
})

test_that("missfillinFor validates character and numeric ranges", {
  data <- data.frame(a = 1:3, b = 4:6)

  expect_error(missfillinFor(data, range = "a"), "시작열:끝열")
  expect_error(missfillinFor(data, range = 3), "존재하는 열")
})

test_that("PMM gives an actionable error when mice is unavailable", {
  skip_if(requireNamespace("mice", quietly = TRUE))
  data <- data.frame(a = c(1, NA, 3), b = c(2, 4, NA))

  expect_error(missfillin(data, method = "pmm"), "mice")
  expect_error(missfillinFor(data, method = "pmm"), "mice")
})
