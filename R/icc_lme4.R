#' Calculate Intraclass Correlation Coefficient (ICC) using lme4
#'
#' This function computes the Intraclass Correlation Coefficient (ICC)
#' based on a random-intercept-only linear mixed-effects model.
#'
#' @param y numeric vector. Outcome variable.
#' @param cluster vector or factor. Grouping (cluster) variable.
#'
#' @return A data.frame with the following components:
#' \describe{
#'   \item{ICC}{Intraclass correlation coefficient}
#'   \item{between_var}{Between-cluster variance (tau^2)}
#'   \item{within_var}{Within-cluster variance (sigma^2)}
#'   \item{J}{Number of clusters}
#'   \item{kbar}{Average cluster size}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Example using a built-in R dataset
#' data(ToothGrowth)
#'
#' # ICC of tooth length clustered by supplement type
#' icc_lme4(
#'   y = ToothGrowth$len,
#'   cluster = ToothGrowth$supp
#' )
#'
#' # ICC of tooth length clustered by dose
#' icc_lme4(
#'   y = ToothGrowth$len,
#'   cluster = ToothGrowth$dose
#'
#'
#' # Built-in dataset
#' data(mtcars)
#'
#' # Single ICC calculation
#' icc_lme4(
#'   y = mtcars$mpg,
#'   cluster = mtcars$cyl
#' )
#'
#' # Batch ICC calculation using icc_batch()
#' icc_batch(
#'   data = mtcars,
#'   vars = c("mpg", "hp", "wt"),
#'   cluster = "cyl"
#' )
#' }
icc_lme4 <- function(y, cluster) {

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required but not installed.")
  }

  dat <- data.frame(
    y = y,
    cluster = factor(cluster)
  )

  dat <- dat[complete.cases(dat), ]

  fit <- lme4::lmer(y ~ 1 + (1 | cluster), data = dat, REML = TRUE)

  vc <- as.data.frame(lme4::VarCorr(fit))

  tau2   <- vc$vcov[vc$grp == "cluster"]
  sigma2 <- vc$vcov[vc$grp == "Residual"]

  icc <- tau2 / (tau2 + sigma2)

  data.frame(
    ICC = icc,
    between_var = tau2,
    within_var  = sigma2,
    J = length(unique(dat$cluster)),
    kbar = mean(table(dat$cluster))
  )
}
