#' text to get covariance matrix
#'
#' @param x  text
#' @param lower True
#' @param diagonal TRUE
#' @param sds NULL if cor is need
#' @param names  mat colnames,
#'
#' @return matrix
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' library(jjstat)
#'
#' lower <- '
#'  11.834
#'   6.947   9.364
#'   6.819   5.091  12.532
#'   4.783   5.028   7.495   9.986
#'  -3.839  -3.889  -3.841  -3.625  9.610
#' -21.899 -18.831 -21.748 -18.775 35.522 450.288 '
#'
#' GetCov(lower)
#' #' GetCov(lower, names = c("anomia67", "powerless67", "anomia71", "powerless71","education", "sei"))
#'
#' wheaton.cov <-
#'   GetCov(lower, names = c("anomia67", "powerless67",
#'                           "anomia71", "powerless71",
#'                           "education", "sei"))
#' wheaton.cov
#' wheaton.model <- '
#'   # latent variables
#'     ses     =~ education + sei
#'     alien67 =~ anomia67 + powerless67
#'     alien71 =~ anomia71 + powerless71
#'   # regressions
#'     alien71 ~ alien67 + ses
#'     alien67 ~ ses
#'   # correlated residuals
#'     anomia67 ~~ anomia71
#'     powerless67 ~~ powerless71
#' '
#' fit <- sem(wheaton.model,
#'            sample.cov = wheaton.cov,
#'            sample.nobs = 932)
#' summary(fit, standardized = TRUE)
#' diagram2(fit, curve=2)

#' }
#'
#'
GetCov = function (x, lower = TRUE, diagonal = TRUE,
                   sds = NULL, names = paste("V", 1:nvar, sep = "")){
  if (is.character(x))
    x <- char2num(x)
  if (is.character(sds))
    sds <- char2num(sds)

  nels <- length(x)

  if (lower) {
    COV <- lav_matrix_lower2full(x, diagonal = diagonal)
  }
  else {
    COV <- lav_matrix_upper2full(x, diagonal = diagonal)
  }

  nvar <- ncol(COV)

  if (!diagonal)
    diag(COV) <- 1

  if (!is.null(sds)) {
    stopifnot(length(sds) == nvar)
    COV <- cor2cov(COV, sds)
  }

  stopifnot(length(names) == nvar)
  rownames(COV) <- colnames(COV) <- names

  COV
}
