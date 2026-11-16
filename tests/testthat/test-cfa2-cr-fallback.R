test_that("CR above one uses full-precision standardized loadings only for affected factors", {
  values <- data.frame(Cronbach = c(.8, .7, .6, .5), CR = c(1.089, .9, 1, NA),
                       AVE = c(.714, .5, .4, .3), row.names = c("Value", "Other", "Boundary", "Missing"))
  map <- data.frame(Latent = rep("Value", 4), lambda = c(.905, .849, .757, .846))
  result <- .cfa2_cr_fallback(values, map)
  expected <- sum(map$lambda)^2 / (sum(map$lambda)^2 + sum(1 - map$lambda^2))
  expect_equal(result$values["Value", "CR"], expected)
  expect_equal(round(result$values["Value", "CR"], 3), .906)
  expect_equal(result$values[-1, ], values[-1, ])
  expect_equal(result$values$Cronbach, values$Cronbach)
  expect_equal(result$values$AVE, values$AVE)
  expect_equal(result$adjustments$original_CR, 1.089)
  expect_equal(result$adjustments$adjusted_CR, expected)
  expect_equal(result$adjustments$Latent, "Value")
})

test_that("CR fallback reports invalid input instead of manufacturing a bounded value", {
  values <- data.frame(Cronbach = .8, CR = 1.2, AVE = .7, row.names = "Value")
  for (lambda in list(c(1.1, .8), c(.8, NA), c(.8, Inf))) {
    result <- .cfa2_cr_fallback(values, data.frame(Latent = "Value", lambda = lambda))
    expect_true(is.na(result$values$CR))
    expect_true(is.na(result$adjustments$adjusted_CR))
    expect_match(result$adjustments$status, "유효한 대체값", fixed = TRUE)
  }
})

test_that("converted CR is used consistently and annotated beneath each affected web table", {
  for (package in c("lavaan", "semTools", "semPlot", "semptools", "kableExtra", "htmltools")) {
    skip_if_not_installed(package)
  }
  fit <- lavaan::cfa("Value =~ x1 + x2 + x3 + x4\nOther =~ x5 + x6 + x7",
                       data = lavaan::HolzingerSwineford1939)
  # Inject a known above-one estimate at the conversion boundary; no package namespace mutation.
  fn <- cfa2
  test_env <- new.env(parent = environment(cfa2))
  original <- .cfa2_cr_fallback
  test_env$.cfa2_cr_fallback <- function(values, measurement) {
    values["Value", "CR"] <- 1.089
    original(values, measurement)
  }
  environment(fn) <- test_env
  opened <- character()
  old <- options(browser = function(url, ...) {opened <<- c(opened, url)})
  on.exit(options(old), add = TRUE)
  all <- fn(fit)
  path <- attr(all, "web_file")
  on.exit(unlink(path), add = TRUE)
  change <- attr(all, "CR_adjustments")
  expect_equal(change$Latent, "Value")
  expect_equal(change$original_CR, 1.089)
  expect_true(is.finite(change$adjusted_CR) && change$adjusted_CR <= 1)
  convergent <- fn(fit, type = "CR_AVE", web = FALSE)
  reliability <- fn(fit, type = "alpha", web = FALSE)
  combined <- fn(fit, type = "item_CR_AVE", web = FALSE)
  expect_equal(convergent["Value", "CR"], change$adjusted_CR)
  expect_equal(reliability["Value", "CR"], change$adjusted_CR)
  expect_equal(as.numeric(combined$CR[combined$Latent == "Value"]), round(change$adjusted_CR, 3))
  expect_equal(attr(convergent, "CR_adjustments"), change)
  html <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_equal(lengths(regmatches(html, gregexpr('class="table-note"', html, fixed = TRUE))), 3L)
  expect_equal(lengths(regmatches(html, gregexpr('</table></div><div class="table-note"', html, fixed = TRUE))), 3L)
  expect_match(html, "CR 계산 전환: Value", fixed = TRUE)
  expect_match(html, "기존 omega3 = 1.089", fixed = TRUE)
  expect_match(html, "단순 표준화 CR 공식", fixed = TRUE)
  expect_match(html, "&gt; CR 계산 전환: Value", fixed = TRUE) # embedded Markdown export
  expect_length(opened, 1L)
  # Keep a copy for inspecting exported HTML/Markdown annotation content.
  file.copy(path, file.path(tempdir(), "cfa2-cr-fallback-tested.html"), overwrite = TRUE)
})
