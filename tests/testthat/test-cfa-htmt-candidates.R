# Shared deterministic fixture: a3 is an indicator of B in the generating model
# but assigned to A in the fitted CFA, creating an interpretable HTMT candidate.
.htmt_candidate_fit <- function() {
  population <- 'A =~ .8*a1 + .8*a2
                 B =~ .9*a3 + .8*b1 + .8*b2 + .8*b3
                 A ~~ .86*B'
  data <- lavaan::simulateData(population, sample.nobs = 3000, seed = 718)
  data$s1 <- seq_len(nrow(data)) %% 7 + seq_len(nrow(data)) / nrow(data)
  lavaan::cfa('A =~ a1 + a2 + a3\nB =~ b1 + b2 + b3\nSingle =~ s1', data = data)
}

test_that("candidate HTMT matches the existing calculation and identifies a3", {
  skip_if_not_installed("lavaan")
  fit <- .htmt_candidate_fit()
  result <- cfa_htmt_candidates(fit, print_result = FALSE)
  baseline <- attr(result, "baseline_pairs")
  expect_equal(baseline$pair, "A / B")
  expect_true(baseline$htmt > .9)
  expect_equal(nrow(result), 6L)
  expect_true(all(result$items_remaining >= 2L))
  expect_equal(result$Item[1], "a3")
  best <- result[result$Item == "a3", ]
  expect_true(best$htmt_if_dropped < .9)
  expect_true(best$improvement > 0)
  expect_equal(best$delta, -best$improvement)
  expect_equal(attr(result, "excluded_factors")$latent, "Single")
  R <- attr(result, "item_correlations")
  expected <- mean(abs(R[c("a1", "a2", "a3"), c("b1", "b2", "b3")])) /
    sqrt(mean(abs(R[c("a1", "a2", "a3"), c("a1", "a2", "a3")][upper.tri(diag(3))])) *
           mean(abs(R[c("b1", "b2", "b3"), c("b1", "b2", "b3")][upper.tri(diag(3))])))
  expect_equal(baseline$htmt, expected)
  expect_equal(best$n_exceed_after, 0L)
  expect_equal(best$n_invalid_after, 0L)
  expect_true(is.finite(best$cross_loading_mi))
  expect_equal(best$mean_between, mean(abs(R["a3", c("b1", "b2", "b3")])))
  for (geometric in c(FALSE, TRUE)) {
    checks <- cfa_htmt_candidates(fit, htmt2 = geometric, print_result = FALSE)
    old <- lav_htmt(data = as.data.frame(lavaan::lavInspect(fit, "data")),
      blocks = list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3")),
      htmt2 = geometric, sig = FALSE, digits = 8)
    expect_equal(round(attr(checks, "baseline_pairs")$htmt, 8), as.numeric(old[2, 1]))
  }
})

test_that("minimum item count cannot hide an excessive HTMT by dropping a factor", {
  fit <- .htmt_candidate_fit()
  result <- cfa_htmt_candidates(fit, min_items = 3, print_result = FALSE)
  expect_true(all(result$status == "too_few_items"))
  expect_true(all(is.na(result$htmt_if_dropped)))
  expect_true(all(is.na(result$below_cutoff)))
  expect_true(all(is.na(result$n_exceed_after)))
})

test_that("pair selection and all-pair impact respect the full factor universe", {
  fit <- lavaan::cfa('A =~ x1 + x2 + x3\nB =~ x4 + x5 + x6\nC =~ x7 + x8 + x9',
                      data = lavaan::HolzingerSwineford1939)
  all <- cfa_htmt_candidates(fit, only_exceed = FALSE, print_result = FALSE)
  one <- cfa_htmt_candidates(fit, latent = "A", only_exceed = FALSE, print_result = FALSE)
  two <- cfa_htmt_candidates(fit, latent = c("B", "C"), only_exceed = FALSE, print_result = FALSE)
  expect_equal(length(unique(all$pair)), 3L)
  expect_equal(unique(one$pair), c("A / B", "A / C"))
  expect_equal(unique(two$pair), "B / C")
  expect_true(all(is.finite(all$max_htmt_after)))
  expect_equal(nrow(attr(two, "baseline_pairs")), 3L)
  expect_error(cfa_htmt_candidates(fit, latent = "Absent"), "latent")
  expect_error(cfa_htmt_candidates(fit, cutoff = 1.1), "cutoff")
  expect_error(cfa_htmt_candidates(fit, min_items = 1), "min_items")
  expect_error(cfa_htmt_candidates(lm(mpg ~ wt, mtcars)), "lavaan")
})

test_that("overlapping blocks and unsupported groups are explicitly rejected", {
  cross <- lavaan::cfa('A =~ x1 + x2 + x3\nB =~ x4 + x5 + x6 + x3',
                        data = lavaan::HolzingerSwineford1939)
  expect_error(cfa_htmt_candidates(cross), "Cross-loaded items")
  grouped <- lavaan::cfa('A =~ x1 + x2 + x3', data = lavaan::HolzingerSwineford1939, group = "school")
  expect_error(cfa_htmt_candidates(grouped), "single-group")
})

test_that("plot handles candidate results, lavaan fits, empty and unevaluable candidates", {
  fit <- .htmt_candidate_fit()
  checks <- cfa_htmt_candidates(fit, print_result = FALSE)
  plot <- cfa_htmt_candidates_plot(checks, top_n = Inf)
  expect_s3_class(plot, "ggplot")
  expect_equal(nrow(plot$data), 6L)
  expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[1]]), 6L)
  expect_s3_class(cfa_htmt_candidates_plot(fit, top_n = 2), "ggplot")
  blocked <- cfa_htmt_candidates(fit, min_items = 3, print_result = FALSE)
  expect_equal(nrow(ggplot2::ggplot_build(cfa_htmt_candidates_plot(blocked))$data[[4]]), 6L)
  empty <- checks[FALSE, ]
  expect_s3_class(cfa_htmt_candidates_plot(empty), "ggplot")
  expect_error(cfa_htmt_candidates_plot(data.frame(x = 1)), "df must")
  expect_error(cfa_htmt_candidates_plot(checks, top_n = 0), "top_n")
  high <- checks
  high$htmt_current <- 1.2; high$htmt_if_dropped <- 1.15
  expect_true(cfa_htmt_candidates_plot(high)$coordinates$limits$x[2] > 1.2)
})

test_that("missing data handling is explicit and fixed across exclusion scenarios", {
  data <- lavaan::HolzingerSwineford1939
  data$x2[1] <- NA_real_
  fit <- lavaan::cfa('A =~ x1 + x2 + x3\nB =~ x4 + x5 + x6', data = data, missing = "fiml")
  unchanged <- cfa_htmt_candidates(fit, only_exceed = FALSE, print_result = FALSE)
  expect_true(is.na(attr(unchanged, "baseline_pairs")$htmt))
  expect_true(length(attr(unchanged, "diagnostic_notes")) > 0)
  pairwise <- cfa_htmt_candidates(fit, missing = "pairwise.complete.obs", only_exceed = FALSE, print_result = FALSE)
  expect_true(all(is.finite(pairwise$htmt_current)))
  complete <- cfa_htmt_candidates(fit, missing = "complete.obs", only_exceed = FALSE, print_result = FALSE)
  expect_equal(attr(complete, "item_correlations"), stats::cor(data[, paste0("x", 1:6)], use = "complete.obs"))
})

test_that("two-item factors are retained and cannot create artificial threshold success", {
  fit <- lavaan::cfa('A =~ x1 + x2\nB =~ x4 + x5 + x6', data = lavaan::HolzingerSwineford1939)
  checks <- cfa_htmt_candidates(fit, only_exceed = FALSE, print_result = FALSE)
  expect_true(all(checks$status[checks$latent == "A"] == "too_few_items"))
  expect_true(all(is.na(checks$htmt_if_dropped[checks$latent == "A"])))
  expect_true(all(checks$status[checks$latent == "B"] == "evaluated"))
})

test_that("plot TRUE draws immediately and invisibly returns candidates with the graph", {
  fit <- .htmt_candidate_fit()
  ordinary <- withVisible(cfa_htmt_candidates(fit, print_result = FALSE))
  expect_true(ordinary$visible)
  expect_null(attr(ordinary$value, "plot"))
  file <- tempfile(fileext = ".png")
  grDevices::png(file, width = 1000, height = 600)
  device <- grDevices::dev.cur()
  on.exit({
    if (device %in% grDevices::dev.list()) grDevices::dev.off(device)
    unlink(file)
  }, add = TRUE)
  plotted <- withVisible(cfa_htmt_candidates(fit, print_result = FALSE, plot = TRUE))
  grDevices::dev.off(device)
  expect_false(plotted$visible)
  expect_s3_class(plotted$value, "tbl_df")
  expect_s3_class(attr(plotted$value, "plot"), "ggplot")
  expect_equal(plotted$value$htmt_if_dropped, ordinary$value$htmt_if_dropped)
  expect_equal(file.info(file)$size > 0, TRUE)
  expect_error(cfa_htmt_candidates(fit, plot = NA), "Logical options")
})

test_that("selected factors remain in results and plot even when HTMT is below cutoff", {
  fit <- lavaan::cfa('Value =~ x1 + x2 + x3\nEffort =~ x4 + x5',
                      data = lavaan::HolzingerSwineford1939)
  checks <- cfa_htmt_candidates(fit, latent = c("Effort", "Value"), print_result = FALSE)
  expect_false(formals(cfa_htmt_candidates)$only_exceed)
  expect_true(all(checks$htmt_current < .9))
  expect_equal(nrow(checks), 5L)
  expect_equal(ncol(checks), 22L)
  expect_setequal(checks$latent, c("Effort", "Value"))
  expect_true(all(checks$status[checks$latent == "Effort"] == "too_few_items"))
  plot <- cfa_htmt_candidates_plot(checks)
  expect_equal(nrow(plot$data), 5L)
  expect_true(all(c("htmt_current", "htmt_if_dropped", "label_x", "result") %in% names(plot$data)))
  expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[2]]), 5L)
  filtered <- cfa_htmt_candidates(fit, latent = c("Effort", "Value"),
                                  only_exceed = TRUE, print_result = FALSE)
  expect_equal(nrow(filtered), 0L)
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = 1000, height = 600)
  device <- grDevices::dev.cur()
  on.exit({if (device %in% grDevices::dev.list()) grDevices::dev.off(device); unlink(path)}, add = TRUE)
  printed <- capture.output(plotted <- cfa_htmt_candidates(fit, latent = c("Effort", "Value"),
                                            plot = TRUE, print_result = FALSE))
  grDevices::dev.off(device)
  expect_s3_class(plotted, "tbl_df")
  expect_s3_class(attr(plotted, "plot"), "ggplot")
  expect_equal(nrow(attr(plotted, "plot")$data), 5L)
})


test_that("rollback returns an ordinary full tibble with no summary interface", {
  fit <- .htmt_candidate_fit()
  checks <- cfa_htmt_candidates(fit, print_result = FALSE)
  expect_identical(class(checks), c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(checks), 22L)
  expect_false(any(c("show", "output") %in% names(formals(cfa_htmt_candidates))))
  expect_null(attr(checks, "show"))
  expect_null(attr(checks, "output"))
  full <- paste(capture.output(print(checks, n = Inf, width = Inf)), collapse = "\n")
  expect_match(full, "cross_loading_mi", fixed = TRUE)
  expect_false(grepl("HTMT candidates: summary", full, fixed = TRUE))
})
