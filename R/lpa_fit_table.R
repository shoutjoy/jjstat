#' Compare Latent Profile Analysis Model Fit Using mclust
#'
#' Fit Gaussian mixture models with different numbers of latent profiles and
#' compare log-likelihood, AIC, BIC, sample-size adjusted BIC, normalized
#' entropy, minimum class size, and optional bootstrap likelihood-ratio tests.
#'
#' @param data A numeric data frame or matrix containing at least two continuous
#'   indicators. Missing or non-finite values are not permitted.
#' @param profiles Unique positive integer profile counts (default `1:6`).
#' @param model A single covariance parameterization supported by
#'   [mclust::Mclust()], such as `"EEE"`, `"EEI"`, or `"VVV"`.
#' @param nboot Number of bootstrap replicates for BLRT (default 199).
#' @param seed Integer random seed for reproducibility.
#' @param blrt Logical; compute bootstrap likelihood-ratio tests (default TRUE).
#' @param digits Nonnegative integer number of decimals for LL and information criteria.
#' @param format Either `"data"` for a data frame or `"kable"` for a formatted table.
#'
#' @details
#' Each BLRT p-value for G profiles tests the G-1 versus G component model.
#' The BLRT routine tests consecutive component counts from 1 through max(G).
#' If a fit or bootstrap test fails, unavailable results are reported as NA,
#' never as zero. Entropy is normalized to the interval 0 to 1 in exact arithmetic.
#' The SABIC uses the penalty log((n + 2)/24); it can be negative for small n.
#' The function preserves the caller's random-number-generator state.
#'
#' @return A data frame or knitr kable object. The underlying data frame has
#'   attributes `fits` (named list of mclust fit objects), `model`, and `blrt`
#'   (the raw BLRT result or NULL). For `format="kable"`, these are attached
#'   to the returned kable object as well.
#' @seealso [mclust::Mclust()], [mclust::mclustBootstrapLRT()]
#' @references
#' Scrucca, L., Fop, M., Murphy, T. B., & Raftery, A. E. (2016).
#' mclust 5: Clustering, classification and density estimation using Gaussian
#' finite mixture models. The R Journal, 8(1), 289-317.
#' https://doi.org/10.32614/RJ-2016-021
#' @export
#' @examples
#' dat <- iris[, 1:4]
#' # Quick model comparison without bootstrap
#' result <- lpa_fit_table(dat, profiles = 1:4, model = "EEE", blrt = FALSE)
#' print(result)
#' attr(result, "fits")[["3"]]
#' # Inspect the estimated class sizes
#' fit3 <- attr(result, "fits")[["3"]]
#' if (!is.null(fit3)) table(fit3$classification)
#' # Alternative covariance model
#' lpa_fit_table(dat, profiles = 1:3, model = "EEI", blrt = FALSE)
#' # Formatted table
#' lpa_fit_table(dat, profiles = 1:3, format = "kable", blrt = FALSE)
#' \dontrun{
#' # Bootstrap comparison can take considerable time
#' result_blrt <- lpa_fit_table(dat, profiles = 1:6,
#'                              model = "EEE", nboot = 199,
#'                              seed = 123, blrt = TRUE)
#' print(result_blrt)
#' # Compare with jjstat exploratory LPA routines
#' jjstat::lpa_explore_modelfit(dat, n_profiles_range = 1:6,
#'   modelNames = c("EEE", "EEI", "EEV", "EII", "VVI", "VII", "VVV"),
#'   na2zero = FALSE)
#' jjstat::lpa(dat, profile = 4, model = "EEE")
#' }
#'
lpa_fit_table <- function(data, profiles = 1:6, model = "EEE",
                          nboot = 199, seed = 123, blrt = TRUE,
                          digits = 2, format = c("data", "kable")) {
  # 1. Validate input and parameter settings.
  format <- match.arg(format)
  if (!requireNamespace("mclust", quietly = TRUE))
    stop("Package 'mclust' is required. Install it with install.packages('mclust').")
  if (format == "kable" && !requireNamespace("knitr", quietly = TRUE))
    stop("Package 'knitr' is required for format='kable'.")
  if (!(is.data.frame(data) || is.matrix(data)))
    stop("data must be a data frame or matrix.")
  if (is.data.frame(data) && !all(vapply(data, is.numeric, logical(1))))
    stop("All indicator columns must be numeric.")
  x <- as.matrix(data)
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)))
    stop("All indicators must be finite numeric values without NA. Clean data first.")
  if (ncol(x) < 2L || nrow(x) < 3L)
    stop("At least two indicators and three observations are required.")
  if (any(apply(x, 2L, stats::sd) <= 0))
    stop("All indicators must have positive variance.")
  if (!is.numeric(profiles) || !length(profiles) ||
      anyNA(profiles) || any(!is.finite(profiles)) ||
      any(profiles != floor(profiles)) || any(profiles < 1) ||
      any(profiles >= nrow(x)) || anyDuplicated(profiles))
    stop("profiles must contain unique integers from 1 to nrow(data)-1.")
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
      !nzchar(model)) stop("model must be one nonempty covariance model name.")
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed) || seed != floor(seed) || seed < 0 ||
      seed > .Machine$integer.max)
    stop("seed must be a nonnegative integer within R's integer range.")
  if (!is.logical(blrt) || length(blrt) != 1L || is.na(blrt))
    stop("blrt must be TRUE or FALSE.")
  if (!is.numeric(nboot) || length(nboot) != 1L || is.na(nboot) ||
      !is.finite(nboot) || nboot < 1 || nboot != floor(nboot))
    stop("nboot must be a positive integer.")
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      !is.finite(digits) || digits < 0 || digits != floor(digits) || digits > 15)
    stop("digits must be an integer between 0 and 15.")
  profiles <- sort(as.integer(profiles))
  n <- nrow(x)

  # 2. Save and restore the caller's RNG state, including absence of a seed.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  old_rngkind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(old_rngkind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))

  # 3. Fit each requested number of profiles independently.
  fits <- lapply(profiles, function(g) {
    tryCatch(mclust::Mclust(x, G = g, modelNames = model, verbose = FALSE),
      error = function(e) {
        warning(sprintf("G=%d: %s", g, conditionMessage(e)), call. = FALSE)
        NULL
      })
  })
  names(fits) <- as.character(profiles)

  # 4. Calculate information criteria, classification entropy and class sizes.
  tab <- do.call(rbind, Map(function(g, fit) {
    row <- data.frame(
      프로파일수 = g, LL = NA_real_, 모수 = NA_integer_,
      AIC = NA_real_, BIC = NA_real_, SABIC = NA_real_,
      엔트로피 = NA_real_, BLRT_p = NA_real_,
      최소_n = NA_integer_, 최소_pct = NA_real_
    )
    if (is.null(fit) || is.null(fit$loglik) ||
        !is.finite(fit$loglik) || is.null(fit$df) ||
        !is.finite(fit$df)) return(row)
    ll <- as.numeric(fit$loglik)
    k <- as.integer(fit$df)
    row$LL <- ll
    row$모수 <- k
    row$AIC <- -2 * ll + 2 * k
    row$BIC <- -2 * ll + k * log(n)
    row$SABIC <- -2 * ll + k * log((n + 2) / 24)
    if (g > 1L) {
      z <- fit$z
      if (is.matrix(z) && nrow(z) == n && ncol(z) == g &&
          all(is.finite(z))) {
        # 0 * log(0) is defined as 0 for Shannon entropy.
        terms <- ifelse(z > 0, z * log(pmax(z, .Machine$double.xmin)), 0)
        row$엔트로피 <- 1 + sum(terms) / (n * log(g))
      }
      cls <- fit$classification
      if (!is.null(cls) && length(cls) == n && !anyNA(cls) &&
          all(cls %in% seq_len(g))) {
        counts <- tabulate(cls, nbins = g)
        row$최소_n <- min(counts)
        row$최소_pct <- 100 * min(counts) / n
      }
    }
    row
  }, profiles, fits))
  rownames(tab) <- NULL

  # 5. Run a single consecutive bootstrap LRT and map by tested G.
  lrt <- NULL
  if (blrt && any(profiles >= 2L)) {
    set.seed(as.integer(seed))
    lrt <- tryCatch(
      mclust::mclustBootstrapLRT(x, modelName = model,
                                nboot = as.integer(nboot),
                                maxG = max(profiles)),
      error = function(e) {
        warning(sprintf("BLRT failed: %s", conditionMessage(e)), call. = FALSE)
        NULL
      }
    )
    if (!is.null(lrt) && !is.null(lrt$p.value)) {
      pv <- as.numeric(lrt$p.value)
      tested_g <- seq.int(2L, length.out = length(pv))
      tab$BLRT_p <- pv[match(tab$프로파일수, tested_g)]
      # A failed target fit should not have an interpretable comparison row.
      tab$BLRT_p[vapply(fits, is.null, logical(1))] <- NA_real_
    }
  }

  # 6. Round only presentation values; retain raw fit objects.
  out <- tab
  for (nm in c("LL", "AIC", "BIC", "SABIC"))
    out[[nm]] <- round(out[[nm]], digits)
  out$엔트로피 <- round(out$엔트로피, 3)
  out$BLRT_p <- round(out$BLRT_p, 3)
  out$`최소 프로파일 n(%)` <- ifelse(
    is.na(out$최소_n), "-",
    sprintf("%d(%.1f)", out$최소_n, out$최소_pct)
  )
  out$최소_n <- NULL
  out$최소_pct <- NULL
  attr(out, "fits") <- fits
  attr(out, "model") <- model
  attr(out, "blrt") <- lrt
  if (format == "kable") {
    rendered <- knitr::kable(out, caption = paste0(
      "잠재프로파일 모형의 적합도 비교 (", model, " 모형)"))
    attr(rendered, "fits") <- fits
    attr(rendered, "model") <- model
    attr(rendered, "blrt") <- lrt
    return(rendered)
  }
  out
}
