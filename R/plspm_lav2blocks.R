#' lavaan syntax to plspm blocks
#'
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


#' blocks to lavaan syntax
#'
#' @param listdata block list data
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
plspm_blocks2lav <- function(listdata) {
  # 초기 모델 문자열 설정
  blocks_model <- 'blocks_model =" '

  # 각 요소를 반복하면서 모델 문자열 생성
  for (block in names(listdata)) {
    # 변수명을 포함한 행 생성
    row <- paste(block, "=~", paste(listdata[[block]], collapse = " + "))
    # 모델 문자열에 행 추가
    blocks_model <- paste(blocks_model, row, sep = "\n")
  }

  # 마지막 행 추가 및 닫기
  blocks_model <- paste(blocks_model, '"', sep = "\n")

  # 최종 모델 문자열 반환
  return(blocks_model)
}
