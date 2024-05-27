#' lavaan syntax to plspm blocks
#'
#' @param blocks_model lavaan syntax
#' @param blocks_model lavaan syntax
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Input data
#' blocks_model <- "
#' IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5
#' EXPE =~ expe1 + expe2 + expe3 + expe4 + expe5
#' QUAL =~ qual1 + qual2 + qual3 + qual4 + qual5
#' VAL =~ val1 + val2 + val3 + val4
#' SAT =~ sat1 + sat2 + sat3 + sat4
#' LOY =~ loy1 + loy2 + loy3 + loy4
#' "
#'
#' # Example function calls
#' plspm_lav2blocks(blocks_model)
#'
#' }
plspm_lav2blocks <- function(blocks_model) {
  # Separate them and save them as a line-by-line list.
  # lines <- unlist(strsplit(blocks_model, "\n"))
  lines <- unlist(strsplit(blocks_model, "\\s*;\\s*|\n"))
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  # Create an empty list.
  blocks_list <- list()

  # Process each line.
  for (line in lines) {
    # Remove whitespace.
    line <- trimws(line)
    # Ignore blank lines.
    if (nchar(line) == 0) {
      next
    }
    # Separate based on the =~ operator.
    parts <- unlist(strsplit(line, " =~ "))
    block_name <- parts[1]
    indicators <- unlist(strsplit(parts[2], " \\+ "))
    blocks_list[[block_name]] <- indicators
  }

  return(blocks_list)
}


