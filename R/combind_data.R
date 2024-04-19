
#' A function that combines two matrices of data, for example, to combine observation/expectation tables and p-values.
#'
#' @param mat_observed_expected  matrix  observed/expected
#' @param mat_p_value  matirx pvalue
#' @param left  "(" etc input
#' @param right ")"
#' @return matrix
#'

#'
#'  \dontrun{
#'
#' # Example data
#' observed_expected <- matrix(c(1.0587162, 1.1477347, 0.50877193,
#'                               1.1269248, 0.5743712, 1.94957983,
#'                               0.9163776, 1.4060606, 0.03515152,
#'                               0.9530237, 0.8624314, 1.44531611),
#'                             nrow = 4, byrow = TRUE,
#'          dimnames = list(c("공명음", "마찰음", "유기음_경음", "평파열음_평파찰음"),
#'                            c("H", "H(H)", "L")))
#'
#' p_value <- matrix(c(0, 0.00001, 0.00000,
#'                     0, 0.00000, 0.01497,
#'                     0, 0.00013, 0.00000,
#'                     0, 0.00000, 0.00102),
#'                   nrow = 4, byrow = TRUE,
#'        dimnames = list(c("공명음", "마찰음", "유기음_경음", "평파열음_평파찰음"),
#'                       c("H", "H(H)", "L")))
#'
#' sig_sym = add_significance_symbols(p_value)
#'
#' # Combine the data
#' combined_data1 <- combine_data(observed_expected, p_value)
#'  print(combined_data1)
#' combined_data2 <- combine_data(observed_expected, sig_sym)
#'  print(combined_data2)
#'
#' #iris cor
#' iris_cor = iris %>% select(-Species) %>%psych::corr.test()
#' iris_cor$r
#' iris_cor$p
#'
#' combine_data(iris_cor$r%>%round(2),
#'              iris_cor$p%>%round(5),
#'              left="(", right=")")
#'  sig = add_significance_symbols(iris_cor$p)
#'  sig
#'
#'  combine_data( round(iris_cor$r, 3) , sig)
#'
#'  combine_data( round(iris_cor$r, 3) , sig) %>%
#'  lowerMat(diag = 1, fill="")
#'}
#'
#'
#' @export
#'
combine_data <- function(mat_observed_expected,
                         mat_p_value,
                         left=" ",
                         right="") {
  # Extract row and column names
  row_names <- rownames(mat_observed_expected)
  col_names <- colnames(mat_observed_expected)

  # Create a combined data frame
  combined <- data.frame(matrix("",
                                nrow = nrow(mat_observed_expected),
                                ncol = ncol(mat_observed_expected)))
  row.names(combined) <- row_names

  # Fill in combined data frame with observed/expected values and p-values
  for (i in 1:nrow(mat_observed_expected)) {
    for (j in 1:ncol(mat_observed_expected)) {
      value <- mat_observed_expected[i, j]
      mat_p_value_cell <- mat_p_value[i, j]
      combined[i, j] <- paste0(value, left,
                               format(mat_p_value_cell,
                                      scientific = FALSE,
                                      digits = 5),
                               right)
    }
  }

  # Assign column names
  colnames(combined) <- col_names

  return(combined)
}


