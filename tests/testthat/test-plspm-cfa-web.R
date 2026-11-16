test_that("PLS CFA web output contains all tables and embedded graphs", {
  skip_if_not_installed("plspm")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("qgraph")
  data("satisfaction", package = "plspm", envir = environment())
  path <- matrix(0, 3, 3, dimnames = list(c("IMAG", "EXPE", "QUAL"),
                                        c("IMAG", "EXPE", "QUAL")))
  path[2, 1] <- path[3, 2] <- 1
  set.seed(2026)
  fit <- plspm::plspm(satisfaction, path, list(1:5, 6:10, 11:15),
                      modes = rep("A", 3), boot.val = TRUE, br = 20)
  # Reproduce the user's single-file source workflow with an old helper present.
  sourced <- new.env(parent = environment(plspm_cfa))
  sourced$.cfa2_web_report <- function(tables, model, plot, variable_order) {
    stop("The installed cfa2 helper must never be called")
  }
  sys.source(testthat::test_path("..", "..", "R", "plspm_cfa.R"),
             envir = sourced)
  plspm_cfa <- sourced$plspm_cfa
  expect_true(is.function(sourced$.plspm_cfa_web_report))
  opened <- character()
  old <- options(browser = function(url, ...) opened <<- c(opened, url))
  on.exit(options(old), add = TRUE)
  before <- grDevices::dev.cur()
  result <- withVisible(plspm_cfa(fit, type = "CRAVE", web = TRUE))
  report <- attr(result$value, "web_file")
  on.exit(unlink(report), add = TRUE)
  expect_false(result$visible)
  expect_length(opened, 1L)
  expect_equal(grDevices::dev.cur(), before)
  expect_true(file.exists(report))
  expect_s3_class(result$value$CR_AVE, "data.frame")
  html <- paste(readLines(report, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (text in c("PLS-PM CFA / SEM Results", "Fornell-Larcker", "HTMT",
                 "Total effects", "Effect size (f2)", "Crossloadings", "Path diagram",
                 "Direct / indirect effects", "IMAG =~ imag1 + imag2",
                 'id="download-html"', 'id="download-md"', 'class="copy-table"',
                 "plspm-cfa-results.html", "plspm-cfa-factor-loadings.png")) {
    expect_match(html, text, fixed = TRUE)
  }
  expect_equal(length(gregexpr('<img class="report-plot"', html, fixed = TRUE)[[1]]), 4L)
  expect_false(grepl('<script src=', html, fixed = TRUE))
  # Legacy mode still returns the requested table without opening another page.
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit(unlink(file), add = TRUE)
  device <- grDevices::dev.cur()
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE)
  selected <- plspm_cfa(fit, type = "CRAVE", web = FALSE)
  expect_equal(selected, result$value$CR_AVE)
  expect_length(opened, 1L)
  cross <- plspm_cfa(fit, type = "crossloadings_plot", web = FALSE)
  expect_s3_class(cross, "ggplot")
  expect_error(plspm_cfa(fit, web = NA), "web must be TRUE or FALSE", fixed = TRUE)
  expect_error(plspm_cfa(fit, web = 1), "web must be TRUE or FALSE", fixed = TRUE)
  fit$boot <- FALSE
  expect_error(plspm_cfa(fit), "boot.val = TRUE", fixed = TRUE)
})

test_that("failed PLS diagrams preserve caller graphics and report availability", {
  before <- grDevices::dev.cur()
  image <- .plspm_cfa_image(function() stop("diagram unavailable"))
  expect_null(image$uri)
  expect_equal(image$error, "diagram unavailable")
  expect_equal(grDevices::dev.cur(), before)
})
