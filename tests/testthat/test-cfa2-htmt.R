test_that("HTMT excludes single-item factors before either HTMT calculation", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("semTools")
  skip_if_not_installed("semPlot")
  skip_if_not_installed("kableExtra")
  data <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nsingle =~ x7",
                       data = data)
  r <- abs(stats::cor(data[, paste0("x", 1:6)]))
  mono_i <- r[1:3, 1:3][upper.tri(r[1:3, 1:3])]
  mono_j <- r[4:6, 4:6][upper.tri(r[4:6, 4:6])]
  for (geometric in c(FALSE, TRUE)) {
    average <- if (geometric) function(z) exp(mean(log(z))) else mean
    expected <- average(r[1:3, 4:6]) / sqrt(average(mono_i) * average(mono_j))
    result <- cfa2(fit, type = "htmt", web = FALSE, htmt2 = geometric)
    expect_s3_class(result, "data.frame")
    expect_equal(rownames(result), c("visual", "textual"))
    expect_equal(colnames(result), c("visual", "textual"))
    expect_equal(as.numeric(gsub("*", "", result[2, 1], fixed = TRUE)), round(expected, 3))
  }
  all <- cfa2(fit, web = FALSE)
  expect_match(paste(all$Discriminant_HTMT, collapse = "\n"),
               "Single-item factors excluded: single", fixed = TRUE)
  expect_true("x7" %in% all$variable_order)
})

test_that("HTMT explains when fewer than two multi-item factors remain", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("semTools")
  skip_if_not_installed("semPlot")
  skip_if_not_installed("kableExtra")
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\nsingle =~ x4",
                       data = lavaan::HolzingerSwineford1939)
  result <- cfa2(fit, web = FALSE)
  expect_match(result$Discriminant_HTMT, "at least two multi-item factors", fixed = TRUE)
  expect_match(result$Discriminant_HTMT, "single", fixed = TRUE)
  expect_null(cfa2(fit, type = "HTMT", web = FALSE))
})
