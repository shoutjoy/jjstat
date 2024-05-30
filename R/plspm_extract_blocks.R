#' olsom boot result model
#'
#' @param model result model
#'
#' @return lsit
#' @export
#'
#' @examples
#'
#'\dontrun{
#' # using htmt
#'}
#'
#'
plspm_extract_blocks <- function(model) {
  # Extract the blocks and mvs_names from the model
  blocks <- model$blocks
  mvs_names <- model$gens$mvs_names

  # Initialize an empty list to store the result
  res <- list()

  # Initialize an index to keep track of the position in mvs_names
  mvs_index <- 1

  # Loop through each block and extract the corresponding mvs_names
  for (block_name in names(blocks)) {
    # Get the number of variables in the current block
    num_vars <- length(blocks[[block_name]])

    # Get the corresponding mvs_names for the current block
    res[[block_name]] <- mvs_names[mvs_index:(mvs_index + num_vars - 1)]

    # Update the index to the next position
    mvs_index <- mvs_index + num_vars
  }

  return(res)
}
