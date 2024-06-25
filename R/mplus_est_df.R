#' mplus_est_df
#'
#' @param text text est
#' @param digits 3
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' #'
#' esttest <- "MODEL RESULTS
#'
#'                                                     Two-Tailed
#'                     Estimate       S.E.  Est./S.E.    P-Value
#'
#'  F1       BY
#'     X1                 1.000      0.000    999.000    999.000
#'     X2                 0.771      0.034     22.380      0.000
#'     X3                 0.683      0.033     20.510      0.000
#'
#'  F2       BY
#'     X4                 1.000      0.000    999.000    999.000
#'     X5                 0.722      0.032     22.400      0.000
#'     X6                 0.702      0.033     21.320      0.000
#'
#'  F3       BY
#'     Y1                 1.000      0.000    999.000    999.000
#'     Y2                 0.705      0.029     24.023      0.000
#'     Y3                 0.695      0.029     24.243      0.000
#'
#'  F2       WITH
#'     F1                -0.482      0.082     -5.912      0.000
#'
#'  F3       WITH
#'     F1                 0.522      0.084      6.203      0.000
#'     F2                -0.558      0.088     -6.315      0.000
#'
#'  Variances
#'     F1                 1.347      0.121     11.095      0.000
#'     F2                 1.492      0.131     11.382      0.000
#'     F3                 1.599      0.135     11.868      0.000
#'
#'  Residual Variances
#'     X1                 0.364      0.046      7.916      0.000
#'     X2                 0.245      0.029      8.583      0.000
#'     X3                 0.320      0.029     11.160      0.000
#'     X4                 0.347      0.048      7.224      0.000
#'     X5                 0.279      0.029      9.579      0.000
#'     X6                 0.342      0.031     10.865      0.000
#'     Y1                 0.302      0.044      6.931      0.000
#'     Y2                 0.290      0.028     10.444      0.000
#'     Y3                 0.269      0.026     10.230      0.000"
#'
#' mplus_est_df(esttest)
#' #'
#' }
#'
mplus_est_df <- function(text, digits = 3) {
  # Remove the "MODEL RESULTS" and "Two-Tailed" lines
  text <- gsub("MODEL RESULTS", "", text)
  text <- gsub("Two-Tailed", "", text)

  # Split the input text into lines and remove empty lines
  lines <- unlist(strsplit(text, "\n"))
  lines <- lines[lines != ""]

  # Initialize an empty data frame to store the results
  results <- data.frame(var1 = character(),
                        var2 = character(),
                        Estimate = numeric(),
                        SE = numeric(),
                        Est_SE = numeric(),
                        P_Value = character(),
                        stringsAsFactors = FALSE)

  # Initialize variables to store the current var1
  current_var1 <- ""

  # Iterate over each line and parse the information
  for (line in lines) {
    line <- trimws(line)

    # Match lines with at least five parts separated by multiple spaces
    parts <- unlist(strsplit(line, "\\s{2,}"))

    if (length(parts) == 5) {
      var2 <- parts[1]
      Estimate <- as.numeric(parts[2])
      SE <- as.numeric(parts[3])
      Est_SE <- as.numeric(parts[4])
      P_Value <- as.numeric(parts[5])

      # Convert P_Value to character and apply the required transformations
      if (P_Value == 999) {
        P_Value <- ""
      } else if (P_Value == 0) {
        P_Value <- "< .001"
      } else {
        P_Value <- as.character(round(P_Value, digits))
      }

      # Convert Est_SE to numeric and apply the required transformation
      if (Est_SE == 999) {
        Est_SE <- 0
      } else {
        Est_SE <- round(Est_SE, digits)
      }

      results <- rbind(results,
                       data.frame(var1 = current_var1, var2 = var2,
                                  Estimate = round(Estimate, digits),
                                  SE = round(SE, digits),
                                  Est_SE = Est_SE,
                                  P_Value = P_Value,
                                  stringsAsFactors = FALSE))

    } else if (length(parts) == 1 | length(parts) == 2) {
      current_var1 <- gsub("\\s+", "_", trimws(line))
    }
  }

  results <- results %>% tidyr::unite(var, var1, var2, sep = "_")

  # Modify the var column to add prefix when necessary
  results$var <- ifelse(grepl("^Variances_", results$var),
                        paste0(gsub("Variances_", "", results$var), "_", results$var),
                        results$var)
  res = results%>%replace_df_rep("Variances","Var")%>%
    mutate_col(op = sub(".*_([^_]+)_.*", "\\1", .$var), col=2)

  return(res%>%replace_df_rep("_ON_"," <- ", "_BY_", " =~ ", "_Var_", " ~~ ",
                              "_WITH_", " ~~ ") )
}
