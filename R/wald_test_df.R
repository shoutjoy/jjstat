#' wald_test_df: Wald test for linear model coefficients using a data frame or tibble
#'
#' @description This function performs a Wald test to compare two coefficients from a linear model.
#' It calculates the difference, z-statistic, and p-value between the selected coefficients.
#'
#' @param data An object of class `lm`, representing a fitted linear model.
#' @param row1 Integer. The row number of the first coefficient to compare (default = 2).
#' @param row2 Integer. The row number of the second coefficient to compare (default = 3).
#' @param rname1 Character. Name of the first term to select when `sel = "term"`. Defaults to "".
#' @param rname2 Character. Name of the second term to select when `sel = "term"`. Defaults to "".
#' @param sel Character. Method for selecting coefficients: "row" (default) for row numbers,
#' or "term" for term names.
#'
#' @return A tibble containing the estimated coefficients, standard errors, difference, z-statistic,
#' and p-value.
#' @export
#'
#' @importFrom broom tidy
#' @importFrom tibble tibble
#' @importFrom dplyr filter select
#' @importFrom stats pnorm
#' @examples
#' \dontrun{
#' # Using default row selection
#' lm(mpg ~ hp + wt + drat, mtcars) %>% wald_test_df()
#'
#' # Selecting specific rows by number
#' lm(mpg ~ hp + wt + drat, mtcars) %>% wald_test_df(row1 = 2, row2 = 4)
#'
#' # Selecting specific terms by name
#' lm(mpg ~ hp + wt + drat, mtcars) %>% wald_test_df(sel = "term", rname1 = "hp", rname2 = "wt")
#' }
wald_test_df <- function(data, row1 = 2, row2 = 3, rname1 = "", rname2 = "", sel = "row") {
  # Ensure necessary libraries are loaded
  if (!requireNamespace("broom", quietly = TRUE)) {
    stop("The 'broom' package is required but not installed. Please install it.")
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required but not installed. Please install it.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required but not installed. Please install it.")
  }

  # Load required functions
  library(broom)
  library(tibble)
  library(dplyr)

  data <- tidy(data)

  if (sel == "row") {
    # Extract coefficients and standard errors by row number
    b1 <- unlist(data[row1, 2])
    b2 <- unlist(data[row2, 2])
    se1 <- unlist(data[row1, 3])
    se2 <- unlist(data[row2, 3])
    se1_square <- se1^2
    se2_square <- se2^2
    diff <- b1 - b2
    z <- diff / sqrt(se1_square + se2_square)
    p <- 2 * (1 - pnorm(abs(z)))
    statistics <- tibble::tibble(
      b1, b2, se1, se2, diff, z, p
    )
    colnames(statistics) <- c(
      paste0(data[row1, 1] %>% unlist(), "_est"),
      paste0(data[row2, 1] %>% unlist(), "_est"),
      paste0(data[row1, 1] %>% unlist(), "_se"),
      paste0(data[row2, 1] %>% unlist(), "_se"),
      "diff", "z", "p.value"
    )
  } else if (sel == "term") {
    # Extract coefficients and standard errors by term name
    b1 <- unlist(data %>% filter(term == rname1) %>% select(2))
    b2 <- unlist(data %>% filter(term == rname2) %>% select(2))
    se1 <- unlist(data %>% filter(term == rname1) %>% select(3))
    se2 <- unlist(data %>% filter(term == rname2) %>% select(3))
    se1_square <- se1^2
    se2_square <- se2^2
    diff <- b1 - b2
    z <- diff / sqrt(se1_square + se2_square)
    p <- 2 * (1 - pnorm(abs(z)))
    statistics <- tibble::tibble(
      b1, b2, se1, se2, diff, z, p
    )
    colnames(statistics) <- c(
      paste0(rname1, "_est"),
      paste0(rname2, "_est"),
      paste0(rname1, "_se"),
      paste0(rname2, "_se"),
      "diff", "z", "p.value"
    )
  }

  return(statistics)
}
