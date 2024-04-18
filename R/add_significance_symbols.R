#' Functions that output statistical significance for matrices and dataframes Simple and exact expressions
#'
#' @param p_value_table A matrix of p-values, or other numerical matrix
#' @param simple Whether to represent the * mark simply or precisely with ***.
#'
#' @return Matrices marked with a *.
#' @export
#'
#' @examples
#'
#'  \dontrun{
#'
#'  #' p_value <- matrix(c(0, 0.00001, 0.00000,
#'                     0, 0.00000, 0.01497,
#'                     0, 0.00013, 0.00000,
#'                     0, 0.00000, 0.00102),
#'                   nrow = 4, byrow = TRUE,
#'                   dimnames = list(c("공명음", "마찰음", "유기음_경음", "평파열음_평파찰음"),
#'                                   c("H", "H(H)", "L")))
#'      # Add significance symbols
#'      # p_value
#'
#'      add_significance_symbols(p_value)
#'
#'  ##                  H     H(H)  L
#'  ##공명음            "***" "***" "***"
#'  ##마찰음            "***" "***" "*"
#'  ##유기음_경음       "***" "***" "***"
#'  ##평파열음_평파찰음 "***" "***" "**"
#'
#'
#'
#'
#'  }
#'
#'
add_significance_symbols <- function(p_value_table, simple=FALSE) {
  # Define significance levels
  significance_levels <- c(0.001, 0.01, 0.05)


  if(simple){
    # Convert p-values to significance symbols
    symbols <- matrix("", nrow = nrow(p_value_table), ncol = ncol(p_value_table))
    for (i in 1:nrow(p_value_table)) {
      for (j in 1:ncol(p_value_table)) {
        if (p_value_table[i, j] < significance_levels[1]) {
          symbols[i, j] <- "*"
        } else if (p_value_table[i, j] < significance_levels[2]) {
          symbols[i, j] <- "*"
        } else if (p_value_table[i, j] < significance_levels[3]) {
          symbols[i, j] <- "*"
        }
      }
    }
  }else{
    # Convert p-values to significance symbols
    symbols <- matrix("", nrow = nrow(p_value_table), ncol = ncol(p_value_table))
    for (i in 1:nrow(p_value_table)) {
      for (j in 1:ncol(p_value_table)) {
        if (p_value_table[i, j] < significance_levels[1]) {
          symbols[i, j] <- "***"
        } else if (p_value_table[i, j] < significance_levels[2]) {
          symbols[i, j] <- "**"
        } else if (p_value_table[i, j] < significance_levels[3]) {
          symbols[i, j] <- "*"
        }
      }
    }
  }


  # Combine symbols with p-values
  result <- p_value_table
  for (i in 1:nrow(p_value_table)) {
    for (j in 1:ncol(p_value_table)) {
      # result[i, j] <- paste0(format(p_value_table[i, j], scientific = FALSE, digits = 5), symbols[i, j])

      result[i, j] <- symbols[i, j]

    }
  }

  return(result)
}
