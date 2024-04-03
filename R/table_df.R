
#Frequency Analysis Functions
#' Title
#'
#' @param data  data.frame
#' @param remove Select variables to exclude from analysis
#'
#' @return  Publication frequency table
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## Identifying variables
#'  mtcars %>% as_trt("cyl","am","vs","gear")%>% tibble()
#'
#'  mtcars %>% as_trt("cyl","am","vs","gear")%>% table_df()
#'
#'  iris %>% as_trt("Species")%>% table_df()
#' }
#'
#'
table_df <- function(data, remove = NULL) {
  # Extract Factor and chr variables from a data frame
  factor_vars <- sapply(data, is.factor)
  chr_vars <- sapply(data, is.character)

  # Handling variables to exclude
  if (!is.null(remove)) {
    factor_vars[remove] <- FALSE
    chr_vars[remove] <- FALSE
  }
  # Extract a list of factor and chr variables
  factor_var_names <- names(data)[factor_vars]
  chr_var_names <- names(data)[chr_vars]

  # Create a frequency table of factor variables
  factor_freq <- lapply(factor_var_names, function(var) {
    table_data <- table(data[[var]])
    result <- data.frame(Term = rep(var, length(table_data)),
                         Level = names(table_data),
                         Freq = as.vector(table_data))
    return(result)
  })

  # Create a frequency table for the chr variable
  chr_freq <- lapply(chr_var_names, function(var) {
    table_data <- table(data[[var]])
    result <- data.frame(Term = rep(var, length(table_data)),
                         Level = names(table_data),
                         Freq = as.vector(table_data))
    return(result)
  })

  # Combine frequency tables of factor and chr variables into a list
  result <- c(factor_freq, chr_freq)

  # Returning results
  return(do.call(rbind, result))
}
