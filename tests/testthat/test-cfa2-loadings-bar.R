test_that("loading bars do not require fit measures or reliability", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  data <- lavaan::HolzingerSwineford1939[, c("x1", "x2", "x3")]
  data[] <- lapply(data, function(z) as.ordered(cut(z, breaks = 3)))

  # No test statistic is available, so the old eager fitMeasures() call fails.
  fit <- lavaan::cfa(
    "visual =~ x1 + x2 + x3", data = data,
    ordered = c("x1", "x2", "x3"), test = "none"
  )
  expect_error(lavaan::fitMeasures(fit))
  plot <- cfa2(fit, web = FALSE, type = "loadings_bar", dis.sort = FALSE)
  expect_s3_class(plot, "ggplot")
  estimates <- lavaan::parameterEstimates(fit, standardized = TRUE)
  estimates <- estimates[estimates$op == "=~", ]
  expect_equal(plot$data$Item, estimates$rhs)
  expect_equal(plot$data$std, estimates$std.all)
  expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[1]]), 3L)

  renamed <- cfa2(fit, web = FALSE, type = "loadings_bar", rename = TRUE,
                  var_name = c("First", "Second", "Third"), cut = 0.8)
  expect_equal(renamed$data$Item, c("First", "Second", "Third"))
  expect_equal(renamed$layers[[2]]$data$yintercept, 0.8)
})
test_that("loading bar order follows model items independently of display names", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")

  items <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q10", "Q11", "Q13", "Q14", "Q15", "Q16")
  population <- paste("Attitude =~", paste(paste0("0.7*", items), collapse = " + "))
  data <- lavaan::simulateData(population, sample.nobs = 200, seed = 412)
  fit <- lavaan::cfa(paste("Attitude =~", paste(items, collapse = " + ")),
                     data = data, test = "none")
  axis_labels <- function(plot) {
    ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x$get_labels()
  }

  model_order <- cfa2(fit, web = FALSE, type = "loadings_bar", dis.sort = FALSE)
  sorted <- cfa2(fit, web = FALSE, type = "loadings_bar", dis.sort = TRUE)
  expect_equal(axis_labels(model_order), items)
  expect_equal(axis_labels(sorted), sort(items))
  expect_false(identical(axis_labels(model_order), axis_labels(sorted)))
  expect_equal(model_order$data$std, sorted$data$std)
  expect_equal(model_order$data$Item, sorted$data$Item)
  expect_equal(as.numeric(ggplot2::ggplot_build(model_order)$data[[1]]$x),
               seq_along(items))

  labels <- rev(letters[seq_along(items)])
  renamed <- cfa2(fit, web = FALSE, type = "loadings_bar", dis.sort = FALSE,
                  rename = TRUE, var_name = labels)
  renamed_sorted <- cfa2(fit, web = FALSE, type = "loadings_bar", dis.sort = TRUE,
                         rename = TRUE, var_name = labels)
  expect_equal(axis_labels(renamed), labels)
  expect_equal(axis_labels(renamed_sorted), labels[match(sort(items), items)])
  ignored_names <- cfa2(fit, web = FALSE, type = "loadings_bar", dis.sort = FALSE,
                        var_name = labels)
  expect_equal(axis_labels(ignored_names), items)
  expect_error(cfa2(fit, web = FALSE, type = "loadings_bar", rename = TRUE,
                    var_name = "one"), "one non-missing name per loading row")
  expect_error(cfa2(fit, web = FALSE, type = "loadings_bar", dis.sort = NA),
                "dis.sort must be TRUE or FALSE", fixed = TRUE)
})

test_that("legend follows model order with dis.sort FALSE and preserves factor colors", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("ggplot2")
  fit <- lavaan::cfa("ZFactor =~ x1 + x2 + x3\nAFactor =~ x4 + x5 + x6",
                       data = lavaan::HolzingerSwineford1939)
  model_order <- cfa2(fit, type = "loadings_bar", web = FALSE)
  sorted <- cfa2(fit, type = "loadings_bar", web = FALSE, dis.sort = TRUE)
  model_scale <- ggplot2::ggplot_build(model_order)$plot$scales$get_scales("fill")
  sorted_scale <- ggplot2::ggplot_build(sorted)$plot$scales$get_scales("fill")
  expect_equal(as.character(model_scale$get_breaks()), c("ZFactor", "AFactor"))
  expect_equal(as.character(sorted_scale$get_breaks()), c("AFactor", "ZFactor"))
  expect_equal(model_scale$map(c("ZFactor", "AFactor")),
               sorted_scale$map(c("ZFactor", "AFactor")))
})
