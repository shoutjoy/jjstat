#' item
#'
#' @param ... input
#'
#' @return list
#' @export
item <- function(...) {
  return(c(...))
}

#
#' Function to create blocks------------
#'
#' @param ... list data
#' @param dataset data.frame
#' @param output 'match', 'match_colnames' is compare data.frame
#'
#' @return list blocks
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #'
#' 문자열 블록:
#'   블록의 모든 항목이 문자열이고 output이 "match"일 경우,
#'    블록을 그대로 유지합니다.
#'
#' 숫자 블록:
#'   블록의 모든 항목이 숫자이고 output이 "match"일 경우,
#'   블록을 숫자로 변환합니다.
#'
#' "match_colnames" 출력 옵션:
#'   output이 "match_colnames"일 경우, 블록이 숫자인
#'   경우 해당 인덱스를 데이터셋의 컬럼 이름으로
#'   변환합니다. 변환된 컬럼 이름 중 데이터셋에
#'   존재하는 이름만 유지합니다.
#'
#' 기타:
#' #'   블록의 항목이 혼합되어 있거나 output이
#' "match_colnames"가 아닌 경우, 블록을 숫자로
#' 변환하고 last_value를 기준으로 연속된 숫자로
#' 변환합니다. 빈 블록의 경우 빈 정수 벡터를
#' 반환합니다.
#' bl1 <- c("A", "B", "C")
#' bl2 <- c(1, 2, 3)
#' dataset <- data.frame(A = 1:5, B = 1:5, C = 1:5, D = 1:5)
#' dataset
#' # 함수 호출
#' plspm_blocks(block1 = bl1, block2 = bl2,
#' dataset = dataset, output = "match")
#' plspm_blocks(block1 = bl1, block2 = bl2,output = "match")
#' plspm_blocks(block1 = bl1, block2 = bl2,
#' dataset = dataset, output = "match_colnames")
#' plspm_blocks(block1 = bl1, block2 = bl2,
#' output = "match_colnames") #character(0)
#' #'
#' #'
#' #'
#' # selected indicators from satisfaction
#' satisfaction2 = satisfaction[, c(20:22, 24:26)]
#' # add dummy variables to satisfaction2
#' satisfaction2$dummy1 = dummy1
#' satisfaction2$dummy2 = dummy2
#' # add product terms to satisfaction2
#' satisfaction2$sat1m1 = satisfaction2$sat1 * dummy1
#' satisfaction2$sat2m1 = satisfaction2$sat2 * dummy1
#' satisfaction2$sat3m1 = satisfaction2$sat3 * dummy1
#' satisfaction2$sat1m2 = satisfaction2$sat1 * dummy2
#' satisfaction2$sat2m2 = satisfaction2$sat2 * dummy2
#' satisfaction2$sat3m2 = satisfaction2$sat3 * dummy2
#'
#' satisfaction2 %>%str()
#' #'
#' #'
#'
#' plspm_blocks(
#'   Satis = item(1:3),
#'   M1= item(7),
#'   SatisM1 = item(9:11),
#'   M2 = item(8),
#'   SatisM2 = item(12:14),
#'   Loyalty = item(4:6),
#'   dataset=satisfaction2,
#'   output="match_colnames"
#' )
#'
#'
#' plspm_blocks(
#'   Satis = 1:3,
#'   M1 = 7,
#'   SatisM1 = 9:11,
#'   M2 = 8,
#'   SatisM2 = 12:14,
#'   Loyalty = 4:6,
#'   dataset = satisfaction2,
#'   output = "match_colnames"
#' )
#'
#' plspm_blocks(
#'   Satis = 1:3,
#'   M1 = 7,
#'   SatisM1 = 9:11,
#'   M2 = 8,
#'   SatisM2 = 12:14,
#'   Loyalty = 4:6,
#'   dataset = satisfaction2,
#'   output = "match"
#' )
#'
#'
#'
#' #참고
#' plspm_blocks(
#'   Satis = item(1:3),
#'   M1= item(7),
#'   SatisM1 = item(9:11),
#'   M2 = item(8),
#'   SatisM2 = item(12:14),
#'   Loyalty = item(4:6),
#'   dataset=satisfaction2,
#'   output="match"
#' )
#'
#'
#'
#' edu_blocks_named2 <- plspm_blocks(
#'   Support = item(1:4),
#'   Advising = item(5:8),
#'   Tutoring= item( 9:12),
#'   Value = item(13:16),
#'   Satisfaction = item(17:19),
#'   Loyalty =item(20:23),
#'   output = "match_colnames"
# ')
#' edu_blocks_named2
#'
#' # to lavaan syntax
#' edu_blocks_named2 %>% plspm_blocks2lav()%>%cat("\n")
#'
#' # # Example usage
#' edu_blocks_named1 <- plspm_blocks(
#'   Support = item("sup.help", "sup.under", "sup.safe", "sup.conc"),
#'   Advising = item("adv.comp", "adv.acces", "adv.comm", "adv.qual"),
#'   Tutoring = item("tut.prof", "tut.sched", "tut.stud", "tut.qual"),
#'   Value = item("val.devel", "val.deci", "val.meet", "val.info"),
#'   Satisfaction = item("sat.glad", "sat.expe", "sat.over"),
#'   Loyalty = item("loy.proud", "loy.recom", "loy.asha", "loy.back"),
#'   dataset = education , # Provide your dataset here
#'   output = "match_colnames"
#' )
#' #'
#' sat_blocks1 <- plspm_blocks(
#'   IMAG = item(paste0("imag",1:5)),
#'   EXPE = item(paste0("expe", 1:5)),
#'   QUAL = item( paste0("qual", 1:5)),
#'   VAL = item(paste0("val", 1:4)),
#'   SAT = item(paste0("sat", 1:4)),
#'   LOY = item(paste0("loy", 1:4))
#' )
#' sat_blocks1
#' ##to lavaan syntax
#' sat_blocks1 %>% plspm_blocks2lav()%>%cat("\n")
#'
#'
#'
#' }
#'
plspm_blocks <- function(..., dataset = NULL, output = "match") {
  block_list <- list(...)

  # Combine the blocks into a named list
  names_list <- names(block_list)
  if (length(names_list) == 0) {
    stop("Please provide names for the blocks.")
  }

  combined_blocks <- list()
  last_value <- 0  # Variable to keep track of the last value in a block

  for (i in seq_along(names_list)) {
    current_block <- block_list[[i]]

    # Check if all items are character
    is_char <- all(sapply(current_block, is.character))

    # Check if all items are numeric
    is_numeric <- all(sapply(current_block, is.numeric))

    if (output == "match_colnames") {
      # Match items with column names
      if (is_numeric) {
        current_block <- colnames(dataset)[current_block]
      }
      current_block <- current_block[current_block %in% colnames(dataset)]
      combined_blocks[[names_list[i]]] <- current_block
    } else {
      # If items are character and output is "match", keep them as is
      if (is_char && output == "match") {
        combined_blocks[[names_list[i]]] <- current_block

      } else if (is_numeric && output == "match") {
        # If items are numeric and output is "match", convert to numeric
        combined_blocks[[names_list[i]]] <- as.numeric(current_block)

      } else {
        # If items are mixed or output is not "match_colnames", convert to numeric
        current_block <- as.numeric(current_block)
        if (length(current_block) > 0) {
          combined_blocks[[names_list[i]]] <- (last_value + 1):(last_value + length(current_block))
          last_value <- max(combined_blocks[[names_list[i]]])
        } else {
          # Handle the case when the block is empty
          combined_blocks[[names_list[i]]] <- integer(0)
        }
      }
    }
  }

  return(combined_blocks)
}
#
#' Function to create blocks------------
#'
#' @param ... list data
#' @param dataset data.frame
#' @param output 'match', 'match_colnames' is compare data.frame
#'
#' @return list blocks
#' @export
#'
#' @examples
#'
#' \dontrun{
#' edu_blocks_named2 <- blocks(
#'   Support = item(1:4),
#'   Advising = item(5:8),
#'   Tutoring= item( 9:12),
#'   Value = item(13:16),
#'   Satisfaction = item(17:19),
#'   Loyalty =item(20:23),
#'   output = "match_colnames"
# ')
#' edu_blocks_named2
#'
#' # # Example usage
#' edu_blocks_named1 <- blocks(
#'   Support = item("sup.help", "sup.under", "sup.safe", "sup.conc"),
#'   Advising = item("adv.comp", "adv.acces", "adv.comm", "adv.qual"),
#'   Tutoring = item("tut.prof", "tut.sched", "tut.stud", "tut.qual"),
#'   Value = item("val.devel", "val.deci", "val.meet", "val.info"),
#'   Satisfaction = item("sat.glad", "sat.expe", "sat.over"),
#'   Loyalty = item("loy.proud", "loy.recom", "loy.asha", "loy.back"),
#'   dataset = education , # Provide your dataset here
#'   output = "match_colnames"
#' )
#'
#'
#' #' }
#' #'
#' plspm_blocks <- function(..., dataset = NULL, output = "match") {
#'   block_list <- list(...)
#'
#'   # Combine the blocks into a named list
#'   names_list <- names(block_list)
#'   if (length(names_list) == 0) {
#'     stop("Please provide names for the blocks.")
#'   }
#'
#'   combined_blocks <- list()
#'   last_value <- 0  # Variable to keep track of the last value in a block
#'
#'   for (i in seq_along(names_list)) {
#'     current_block <- block_list[[i]]
#'
#'     # Check if all items are character
#'     is_char <- all(sapply(current_block, is.character))
#'
#'     # Check if all items are numeric
#'     is_numeric <- all(sapply(current_block, is.numeric))
#'
#'     # If items are character and output is "match", keep them as is
#'     if (is_char && output == "match") {
#'       combined_blocks[[names_list[i]]] <- current_block
#'     } else if (is_numeric && output == "match") {  # If items are numeric and output is "match", convert to numeric
#'       combined_blocks[[names_list[i]]] <- as.numeric(current_block)
#'     } else if (output == "match_colnames") {  # If output is "match_colnames", match items with column names
#'       # Check if current_block is numeric and retrieve column names from dataset
#'       if (is.numeric(current_block)) {
#'         current_block <- colnames(dataset)[current_block]
#'       }
#'       # Keep only the column names that exist in the dataset
#'       current_block <- current_block[current_block %in% colnames(dataset)]
#'       combined_blocks[[names_list[i]]] <- current_block
#'     } else {  # If items are mixed or output is not "match_colnames", convert to numeric
#'       current_block <- as.numeric(current_block)
#'       if (length(current_block) > 0) {
#'         combined_blocks[[names_list[i]]] <- (last_value + 1):(last_value + length(current_block))
#'         last_value <- max(combined_blocks[[names_list[i]]])
#'       } else {
#'         # Handle the case when the block is empty
#'         combined_blocks[[names_list[i]]] <- integer(0)
#'       }
#'     }
#'   }
#'
#'   return(combined_blocks)
#' }
