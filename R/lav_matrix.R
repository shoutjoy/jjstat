#' paper table to matrix
#'
#' @param text input copy data
#'
#' @return matrix
#' @export
#'
#' @examples
#' \dontrun{
#'
#' Input data Cho(2014)
#' text <- "필 요 성 1
#'          수 업 0.320** 1
#'          교 사 0.266** 0.647** 1
#'          계 획 0.506** 0.463** 0.383** 1
#'          영 향 0.429** 0.548** 0.491** 0.531** 1
#'          만 족 도 0.401** 0.554** 0.545** 0.491** 0.743** 1"
#'
#' lav_matrix(text)
#'
#' #Kim(2014)
#' kim2014text = "
#' 자기주도성  1
#' 컴퓨터효능감 .284** 1
#' 학습설계  .369** .234** 1
#' 상호작용  .124** .179** .361** 1
#' 강사선호도  .267** .130** .423** .368** 1
#' 학업성취도  .281** .197** .548** .306** .513** 1
#' 동기변화  .246** .108* .453** .248** .405** .445** 1
#' 자신감  .262** .092 .471** .149** .353** .482** .576** 1
#' 흥미  .317** .149** .535** .272** .470** .571** .596** .628** 1"
#' #
#' lav_matrix(kim2014text)
#'
#' }
lav_matrix <- function(text) {
  # 텍스트를 줄 단위로 분할
  lines <- strsplit(text, "\n")[[1]]
  #헤더 추출 # # 열 이름 추출
  header <- gsub("\\*", "", lines)
  header <- gsub("\\d+\\.?\\d*", "", header)
  header <- gsub("\\s+", "", header)
  header <- gsub("\\.+\\s*$", "", header)  # 마지막에 오는 기호들 제거
  header <- header[header != ""]

  # 앞부분 제거된 결과
  data_no_header <- gsub("[^0-9\\.]+", " ", lines)
  data_no_header <- trimws(data_no_header)
  data_no_header <- data_no_header[data_no_header != ""]
  # cat("앞부분이 제거된 결과:\n", data_no_header, "\n\n")

  #  # 별표시 제거하고 lav_matrix_lower2full() 함수에 넣기 위한 자료 생성
  input <- gsub("\\*", "", data_no_header)
  input <- gsub("\\s+", ", ", input)
  # # 숫자 1만 있는 경우에는 "1,"으로 변형
  input <- gsub("(?<!\\d)1(?!\\d)", "1,", input, perl = TRUE)
  input <- paste(input, collapse = "")
  # 마지막 쉼표 제거
  input <- gsub(", 1,$", ", 1", input)
  # input <- c(input)
  res = lavaan::lav_matrix_lower2full(as.numeric(unlist(strsplit(input, ","))))
  colnames(res)= header
  rownames(res)= header
  cat( "\n\n")
  #  return(list(header, res))
  return(res)

}
