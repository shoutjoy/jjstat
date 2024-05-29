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
#' #'
#'
#' # Example usage:
#' jutpls_boot0 <- list(
#'   model = list(
#'     IDM = matrix(c(0,0,0,0,1,0,0,0,1,0,0,0,1,1,1,0), nrow=4, byrow=TRUE,
#'                  dimnames = list(c("자기효능감", "진로동기", "진로태도", "진로준비"),
#'                                  c("자기효능감", "진로동기", "진로태도", "진로준비"))),
#'     blocks = list(
#'       자기효능감 = c(111, 112, 113, 114, 115),
#'       진로동기 = c(105, 106, 107),
#'       진로태도 = c(109, 110),
#'       진로준비 = c(116, 117, 118)
#'     ),
#'     specs = list(
#'       scaling = NULL,
#'       modes = c("A", "A", "A", "A"),
#'       scheme = "centroid",
#'       scaled = TRUE,
#'       tol = 1e-06,
#'       maxiter = 100,
#'       plscomp = NULL
#'     ),
#'     iter = 4,
#'     boot.val = TRUE,
#'     br = 100,
#'     gens = list(
#'       obs = 92,
#'       obs_names = as.character(1:93),
#'       mvs = 13,
#'       mvs_names = c("C_S1", "C_S2", "C_S3", "C_S4", "C_S5", "A_M1", "A_M2", "A_M3", "B_A2", "B_A3", "D_P1", "D_P2", "D_P3"),
#'       lvs = 4,
#'       lvs_names = c("자기효능감", "진로동기", "진로태도", "진로준비")
#'     )
#'   )
#' )
#' jutpls_boot0
#' # # Call the function with the example data
#' plspm_extract_blocks(jutpls_boot0$model)
#'
#'
#' #'}
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
