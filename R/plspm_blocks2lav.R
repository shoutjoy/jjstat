#' blocks to lavaan syntax
#'
#' @param listdata block list data
#' @param name defulat FALSE, TRUE generate blocks_model=
#'
#' @return lavaan syntax
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' listdata <- list(
#'   IMAG = c("imag1", "imag2", "imag3", "imag4", "imag5"),
#'   EXPE = c("expe1", "expe2", "expe3", "expe4", "expe5"),
#'   QUAL = c("qual1", "qual2", "qual3", "qual4", "qual5"),
#'   VAL = c("val1", "val2", "val3", "val4"),
#'   SAT = c("sat1", "sat2", "sat3", "sat4"),
#'   LOY = c("loy1", "loy2", "loy3", "loy4")
#' )
#' listdata
#' # Function calls and result output
#' plspm_blocks2lav(listdata)
#' plspm_blocks2lav(listdata)%>%cat("\n")
#' }
#'
plspm_blocks2lav <- function(listdata, name=FALSE) {
  # Setting the initial model string
  if (name) {
    blocks_model <- "blocks_model= \"\n"
  } else {
    blocks_model <- ""
  }

  # Generate a model string by iterating over each element
  for (block in names(listdata)) {
    # Create a row with variable names
    row <- paste(block, "=~", paste(listdata[[block]], collapse = " + "))
    # Adding Rows to the Model String
    blocks_model <- paste(blocks_model, row, sep = "\n")
  }


  blocks_models= blocks_model
  if (name) {
  # Add and close the last row
  blocks_model <- paste(blocks_model, " \" ", sep = "\n")

  }else{
  # Add and close the last row
  blocks_model <- paste(blocks_model, "", sep = "\n")

  }

  # Return the final model string
  return(blocks_model)
}
