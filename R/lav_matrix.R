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
#' # 마이너스가 포함되어 있는 경우
#'#정성령(2014)
#' Jungsy2014 = "
#' 수강료 1
#' 강사 .165** 1
#' 수업 .303** .144** 1
#' 개인관리 .494** .069 .284** 1
#' 인지도 .425** .176** .407** .424** 1
#' 이상적영향력 .210** -.056 .212** .286** .222** 1
#' 영감적동기화 .190** -.133** .211** .290** .230** .659** 1
#' 지적자극 .300** -.136** .159** .402** .238** .533** .517** 1
#' 개별적 배려 .202** -.153** .150** .250** .291** .684** .618** .530** 1
#' "
#' lav_matrix(Jungsy2014)
#'
#' Jungsy2014_cor = lav_matrix(Jungsy2014)
#' Jungsy2014_cor
#'
#'
#' }

lav_matrix <- function(text) {
  # 텍스트를 줄 단위로 분할
  lines <- strsplit(text, "\n")[[1]]

  # 헤더 추출
  header <- gsub("\\*", "", lines)
  header <- gsub("\\d+\\.?\\d*", "", header)
  header <- gsub("\\s+", "", header)
  header <- gsub("\\.+\\s*$", "", header)  # 마지막에 오는 기호들 제거
  header <- gsub("\\.-\\s*$", "", header)
  header <- header[header != ""]

  #음수의 위치를 추출해서 앞부분 제거된 결과의 위치에 맞추어서 표시 하기
  data_no_header_2 <- gsub("(?<=[^\\d])-(?=\\s)", "- ", lines, perl = TRUE)
  data_no_header_2 <- trimws(data_no_header_2)
  data_no_header_2 <- data_no_header_2[data_no_header_2 != ""]

  data_no_header_2 <- gsub("[가-힣]+", "", data_no_header_2, perl = TRUE)
  data_no_header_2 <- gsub("[a-zA-Z]+", "", data_no_header_2, perl = TRUE)
  data_no_header_2 <- gsub("\\*", "", data_no_header_2)  #별표시 삭제
  input2 <- paste(data_no_header_2, collapse = "")  # 벡터화
  input2 <- gsub("\\s+", ", ", input2)   #문자뒤 콤마
  input2 <- gsub("^, 1,", "1,", input2)  #첫번째 1의 수정

  # 헤더와 데이터를 사용하여 행렬 생성
  res <- lavaan::lav_matrix_lower2full(as.numeric(unlist(strsplit(input2, ","))))
  colnames(res) <- header
  rownames(res) <- header

  cat("\n\n")
  # return(res)
  return(res)

}

