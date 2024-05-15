#' cochran_mantel_haenszel_test
#'
#' @param x array
#' @param y NULL
#' @param z NULL
#' @param alternativec("two.sided", "less", "greater")
#' @param correct TRUE
#' @param exact FALSE
#' @param conf.level 0.95
#'
#' @return result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data(Titanic, package = "datasets")
#' Titanic
#' cmh_test(temp)
#' cmh_test(temp)%>%tidy()

#' }
cmh_test = function (x, y = NULL, z = NULL,
                     alternative = c("two.sided", "less", "greater"),
                     correct = TRUE, exact = FALSE, conf.level = 0.95){
  #cochran_mantel_haenszel_test
  DNAME <- deparse1(substitute(x))
  if (is.array(x)) {
    if (length(dim(x)) == 3L) {
      if (anyNA(x))
        stop("NAs are not allowed")
      if (any(dim(x) < 2L))
        stop("each dimension in table must be >= 2")
    }
    else stop("'x' must be a 3-dimensional array")
  }
  else {
    if (is.null(y))
      stop("if 'x' is not an array, 'y' must be given")
    if (is.null(z))
      stop("if 'x' is not an array, 'z' must be given")
    if (any(diff(c(length(x), length(y), length(z))) != 0L))
      stop("'x', 'y', and 'z' must have the same length")
    DNAME <- paste(DNAME, "and", deparse1(substitute(y)),
                   "and", deparse1(substitute(z)))
    OK <- complete.cases(x, y, z)
    x <- factor(x[OK])
    y <- factor(y[OK])
    if ((nlevels(x) < 2L) || (nlevels(y) < 2L))
      stop("'x' and 'y' must have at least 2 levels")
    else x <- table(x, y, z[OK])
  }
  if (any(apply(x, 3L, sum) < 2))
    stop("sample size in each stratum must be > 1")
  I <- dim(x)[1L]
  J <- dim(x)[2L]
  K <- dim(x)[3L]
  if ((I == 2) && (J == 2)) {
    alternative <- match.arg(alternative)
    if (!missing(conf.level) && (length(conf.level) != 1 ||
                                 !is.finite(conf.level) || conf.level < 0 || conf.level >
                                 1))
      stop("'conf.level' must be a single number between 0 and 1")
    NVAL <- c(`common odds ratio` = 1)
    if (!exact) {
      s.x <- apply(x, c(1L, 3L), sum)
      s.y <- apply(x, c(2L, 3L), sum)
      n <- as.double(apply(x, 3L, sum))
      DELTA <- sum(x[1, 1, ] - s.x[1, ] * s.y[1, ]/n)
      YATES <- if (correct && (abs(DELTA) >= 0.5))
        0.5
      else 0
      STATISTIC <- ((abs(DELTA) - YATES)^2/sum(apply(rbind(s.x,
                                                           s.y), 2L, prod)/(n^2 * (n - 1))))
      PARAMETER <- 1
      if (alternative == "two.sided")
        PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
      else {
        z <- sign(DELTA) * sqrt(STATISTIC)
        PVAL <- pnorm(z, lower.tail = (alternative ==
                                         "less"))
      }
      names(STATISTIC) <- "Mantel-Haenszel X-squared"
      names(PARAMETER) <- "df"
      METHOD <- paste("Mantel-Haenszel chi-squared test",
                      if (YATES)
                        "with"
                      else "without", "continuity correction")
      s.diag <- sum(x[1L, 1L, ] * x[2L, 2L, ]/n)
      s.offd <- sum(x[1L, 2L, ] * x[2L, 1L, ]/n)
      ESTIMATE <- s.diag/s.offd
      sd <- sqrt(sum((x[1L, 1L, ] + x[2L, 2L, ]) * x[1L,
                                                     1L, ] * x[2L, 2L, ]/n^2)/(2 * s.diag^2) + sum(((x[1L,
                                                                                                       1L, ] + x[2L, 2L, ]) * x[1L, 2L, ] * x[2L, 1L,
                                                                                                       ] + (x[1L, 2L, ] + x[2L, 1L, ]) * x[1L, 1L, ] *
                                                                                                      x[2L, 2L, ])/n^2)/(2 * s.diag * s.offd) + sum((x[1L,
                                                                                                                                                       2L, ] + x[2L, 1L, ]) * x[1L, 2L, ] * x[2L, 1L,
                                                                                                                                                       ]/n^2)/(2 * s.offd^2))
      CINT <- switch(alternative, less = c(0, ESTIMATE *
                                             exp(qnorm(conf.level) * sd)), greater = c(ESTIMATE *
                                                                                         exp(qnorm(conf.level, lower.tail = FALSE) * sd),
                                                                                       Inf), two.sided = {
                                                                                         ESTIMATE * exp(c(1, -1) * qnorm((1 - conf.level)/2) *
                                                                                                          sd)
                                                                                       })
      RVAL <- list(statistic = STATISTIC, parameter = PARAMETER,
                   p.value = PVAL)
    }
    else {
      METHOD <- paste("Exact conditional test of independence",
                      "in 2 x 2 x k tables")
      mn <- apply(x, c(2L, 3L), sum)
      m <- mn[1L, ]
      n <- mn[2L, ]
      t <- apply(x, c(1L, 3L), sum)[1L, ]
      s <- sum(x[1L, 1L, ])
      lo <- sum(pmax(0, t - n))
      hi <- sum(pmin(m, t))
      support <- lo:hi
      dc <- .Call(C_d2x2xk, K, m, n, t, hi - lo + 1L)
      logdc <- log(dc)
      dn2x2xk <- function(ncp) {
        if (ncp == 1)
          return(dc)
        d <- logdc + log(ncp) * support
        d <- exp(d - max(d))
        d/sum(d)
      }
      mn2x2xk <- function(ncp) {
        if (ncp == 0)
          return(lo)
        if (ncp == Inf)
          return(hi)
        sum(support * dn2x2xk(ncp))
      }
      pn2x2xk <- function(q, ncp = 1, upper.tail = FALSE) {
        if (ncp == 0) {
          as.numeric(if (upper.tail) q <= lo else q >=
                       lo)
        }
        else if (ncp == Inf)
          as.numeric(if (upper.tail) q <= hi else q >=
                       hi)
        else {
          d <- dn2x2xk(ncp)
          sum(d[if (upper.tail) support >= q else support <=
                  q])
        }
      }
      PVAL <- switch(alternative, less = pn2x2xk(s, 1),
                     greater = pn2x2xk(s, 1, upper.tail = TRUE), two.sided = {
                       relErr <- 1 + 10^(-7)
                       d <- dc
                       sum(d[d <= d[s - lo + 1] * relErr])
                     })
      mle <- function(x) {
        if (x == lo)
          return(0)
        if (x == hi)
          return(Inf)
        mu <- mn2x2xk(1)
        if (mu > x)
          uniroot(function(t) mn2x2xk(t) - x, c(0, 1))$root
        else if (mu < x)
          1/uniroot(function(t) mn2x2xk(1/t) - x, c(.Machine$double.eps,
                                                    1))$root
        else 1
      }
      ESTIMATE <- mle(s)
      ncp.U <- function(x, alpha) {
        if (x == hi)
          return(Inf)
        p <- pn2x2xk(x, 1)
        if (p < alpha)
          uniroot(function(t) pn2x2xk(x, t) - alpha,
                  c(0, 1))$root
        else if (p > alpha)
          1/uniroot(function(t) pn2x2xk(x, 1/t) - alpha,
                    c(.Machine$double.eps, 1))$root
        else 1
      }
      ncp.L <- function(x, alpha) {
        if (x == lo)
          return(0)
        p <- pn2x2xk(x, 1, upper.tail = TRUE)
        if (p > alpha)
          uniroot(function(t) pn2x2xk(x, t, upper.tail = TRUE) -
                    alpha, c(0, 1))$root
        else if (p < alpha)
          1/uniroot(function(t) pn2x2xk(x, 1/t, upper.tail = TRUE) -
                      alpha, c(.Machine$double.eps, 1))$root
        else 1
      }
      CINT <- switch(alternative, less = c(0, ncp.U(s,
                                                    1 - conf.level)), greater = c(ncp.L(s, 1 - conf.level),
                                                                                  Inf), two.sided = {
                                                                                    alpha <- (1 - conf.level)/2
                                                                                    c(ncp.L(s, alpha), ncp.U(s, alpha))
                                                                                  })
      STATISTIC <- c(S = s)
      RVAL <- list(statistic = STATISTIC, p.value = PVAL)
    }
    names(ESTIMATE) <- names(NVAL)
    attr(CINT, "conf.level") <- conf.level
    RVAL <- c(RVAL, list(conf.int = CINT, estimate = ESTIMATE,
                         null.value = NVAL, alternative = alternative))
  }
  else {
    df <- (I - 1) * (J - 1)
    n <- m <- double(length = df)
    V <- matrix(0, nrow = df, ncol = df)
    for (k in 1:K) {
      f <- x[, , k]
      ntot <- as.double(sum(f))
      rowsums <- rowSums(f)[-I]
      colsums <- colSums(f)[-J]
      n <- n + c(f[-I, -J])
      m <- m + c(outer(rowsums, colsums))/ntot
      V <- V + (kronecker(diag(ntot * colsums, nrow = J -
                                 1L) - outer(colsums, colsums), diag(ntot * rowsums,
                                                                     nrow = I - 1L) - outer(rowsums, rowsums))/(ntot^2 *
                                                                                                                  (ntot - 1)))
    }
    n <- n - m
    STATISTIC <- c(crossprod(n, qr.solve(V, n)))
    PARAMETER <- df
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- "Cochran-Mantel-Haenszel M^2"
    names(PARAMETER) <- "df"
    METHOD <- "Cochran-Mantel-Haenszel test"
    RVAL <- list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL)
  }
  structure(c(RVAL, list(method = METHOD, data.name = DNAME)),
            class = "htest")
}

#' Breslow-Day Test for Homogeneity of the Odds Ratios
#'
#' @param data 2x2xk table
#' @param tidy broom::tidy
#' @param OR the odds ratio to be tested against. If left undefined (default) the Mantel-Haenszel estimate will be used.
#' @param correct If TRUE, the Breslow-Day test with Tarone's adjustment is computed, which subtracts an adjustment factor to make the resulting statistic asymptotically chi-square.
#' @details
#' For the Breslow-Day test to be valid, the sample size should be relatively large in each stratum, and at least 80% of the expected cell counts should be greater than 5. Note that this is a stricter sample size requirement than the requirement for the Cochran-Mantel-Haenszel test for tables, in that each stratum sample size (not just the overall sample size) must be relatively large. Even when the Breslow-Day test is valid, it might not be very powerful against certain alternatives, as discussed in Breslow and Day (1980).
#' Alternatively, it might be better to cast the entire inference problem into the setting of a logistic regression model. Here, the underlying question of the Breslow-Day test can be answered by investigating whether an interaction term with the strata variable is necessary (e.g. using a likelihood ratio test using the anova function).
#'
#' @return result
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' migraine <- xtabs(freq ~ .,
#'                   cbind(expand.grid(treatment=c("active", "placebo"),
#'                                     response =c("better", "same"),
#'                                     gender   =c("female", "male")),
#'                         freq=c(16, 5, 11, 20, 12, 7, 16, 19))
#' )
#' # get rid of gender
#' tab <- xtabs(Freq ~ treatment + response, migraine)
#' DescTools::Desc(tab)
#' # only the women
#' female <- migraine[,, 1]
#' DescTools::Desc(female)
#' # .. and the men
#' male <- migraine[,, 2]
#' DescTools::Desc(male)
#' breslowdata_test(migraine)
#' breslowdata_test(migraine, correct = TRUE)
#'
#' }
breslowdata_test= function(data, OR = NA, correct = FALSE, tidy=TRUE){

  if(tidy){
    DescTools::BreslowDayTest(data, OR=OR,correct=correct )%>%tidy()
  }else{
    DescTools::BreslowDayTest(data, OR=OR,correct=correct )
  }


}


