#' mplus_fit_df: model fit from  Mplus result
#'
#' @param text mplut copy data
#' @param digits 3
#'
#' @return fit data.frame
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'
#'
#' text= c("MODEL FIT INFORMATION
#'
#' Number of Free Parameters                       21
#'
#' Loglikelihood
#'
#'           H0 Value                       -4568.339
#'           H1 Value                       -4551.194
#'
#' Information Criteria
#'
#'           Akaike (AIC)                    9178.677
#'           Bayesian (BIC)                  9263.623
#'           Sample-Size Adjusted BIC        9196.983
#'             (n* = (n + 2) / 24)
#'
#' Chi-Square Test of Model Fit
#'
#'           Value                             34.290
#'           Degrees of Freedom                    24
#'           P-Value                           0.0796
#'
#' RMSEA (Root Mean Square Error Of Approximation)
#'
#'           Estimate                           0.032
#'           90 Percent C.I.                    0.000  0.054
#'           Probability RMSEA <= .05           0.901
#'
#' CFI/TLI
#'
#'           CFI                                0.996
#'           TLI                                0.994
#'
#' Chi-Square Test of Model Fit for the Baseline Model
#'
#'           Value                           2533.266
#'           Degrees of Freedom                    36
#'           P-Value                           0.0000
#'
#' SRMR (Standardized Root Mean Square Residual)
#'
#'           Value                              0.022")
#'
#' mplus_fit_df(text)
#' #' }
mplus_fit_df <- function(text, digits = 3) {
  # Replace specific phrases in the text
  text <- gsub("RMSEA \\(Root Mean Square Error Of Approximation\\)", "RMSEA", text)
  text <- gsub("Chi-Square Test of Model Fit for the Baseline Model", "Chi-Square Test(Baseline)", text)
  text <- gsub("SRMR \\(Standardized Root Mean Square Residual\\)", "SRMR", text)

  # Split the input text into lines
  lines <- unlist(strsplit(text, "\n"))

  # Initialize an empty data frame to store the results
  results <- data.frame(index = character(),
                        term = character(),
                        value = character(),
                        value2 = character(),
                        stringsAsFactors = FALSE)

  # Variable to store the current index
  current_index <- ""

  # Iterate over each line and parse the information
  for (line in lines) {
    line <- trimws(line)
    if (line == "") next

    # Match lines with at least two parts separated by multiple spaces
    parts <- unlist(strsplit(line, "\\s{2,}"))
    if (length(parts) == 2) {
      term <- parts[1]
      value <- parts[2]
      results <- rbind(results, data.frame(index = current_index, term = term, value = value, value2 = "", stringsAsFactors = FALSE))
    } else if (length(parts) == 3) {
      term <- parts[1]
      value <- parts[2]
      value2 <- parts[3]
      results <- rbind(results, data.frame(index = current_index, term = term, value = value, value2 = value2, stringsAsFactors = FALSE))
    } else if (length(parts) == 1) {
      term <- parts[1]
      current_index <- term
    }
  }

  # Convert value columns to numeric and round
  results$value <- round(as.numeric(results$value), digits)
  results$value2[results$value2 != ""] <- round(as.numeric(results$value2[results$value2 != ""]), digits)

  return(results %>%nice_table%>%dall())
}
