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
#' #'
#' 지경선(2016)
#' # 개인배경: 수학성적, 기대수준
#' # 선행학습실태: 학습시간, 학습진도, 반복횟수, 이해정도, 비용, 향후계획
#' # 선행학습유발요인:입시준비, 학벌사회,학교수업
#' # 수학학습태도: 흥미와동기, 학습자세, 수학효능감
#'
#'
#' Jiks2016="
#' 수학성적 1
#' 기대수준 .256*** 1
#' 학습시간 .128** .079 1
#' 학습진도 .304*** .146** .381*** 1
#' 반복횟수 .115* .073 .155*** .173*** 1
#' 이해정도 .150*** .283*** .142** .070 .236*** 1
#' 비용 .132** .214*** .295*** .212*** .042 .086 1
#' 향후계획 .228*** .368*** .134** .183*** .061 .302*** .226*** 1
#' 입시준비 .201*** .300*** .155*** .249*** .121** .128*** .224*** .624*** 1
#' 학벌사회 .097* .196*** .106* .176*** .063 -.032 .152*** .287*** .497*** 1
#' 학교수업 .072 .258*** .120** .140** .087 .242*** .049 .292*** .235*** .098* 1
#' 흥미동기 .357*** .358*** .113* .182*** .062 .350*** .175*** .514*** .359*** .182*** .373*** 1
#' 학습자세 .257*** .441*** .177*** .193*** .101* .320*** .186*** .478*** .448*** .268*** .391*** .636*** 1
#' 수학효능감 .419*** .385*** .249*** .322*** .121** .373*** .182*** .505*** .423*** .261*** .459*** .703*** .614*** 1
#' "
#' lav_matrix(Jiks2016, "check")
#' lav_matrix(Jiks2016)
#' }
#'

lav_matrix <- function(text, type="res") {
  # 텍스트를 줄 단위로 분할
  lines <- strsplit(text, "\n")[[1]]

  # 헤더 추출
  header <- gsub("\\*", "", lines)
  header <- gsub("\\d+\\.?\\d*", "", header)
  header <- gsub("\\s+", "", header)
  header <- gsub("\\.+\\s*$", "", header)  # 마지막에 오는 기호들 제거
  header <- gsub("\\.-\\s*$", "", header)
  header <- gsub("\\....\\s*$", "", header) #에외.....발생부분
  header <- gsub("\\-\\s*$", "", header)
  header <- header[header != ""]

  #음수의 위치를 추출해서 앞부분 제거된 결과의 위치에 맞추어서 표시 하기
  data_no_header_2 <- gsub("(?<=[^\\d])-(?=\\s)", "- ", lines, perl = TRUE)
  data_no_header_2 <- trimws(data_no_header_2)
  data_no_header_2 <- data_no_header_2[data_no_header_2 != ""]

  data_no_header_2 <- gsub("[가-힣]+", "", data_no_header_2, perl = TRUE)
  data_no_header_2 <- gsub("[a-zA-Z]+", "", data_no_header_2, perl = TRUE)
  data_no_header_2 <- gsub("\\*", "", data_no_header_2)  #별표시 삭제
  data_no_header_2 <- gsub("_", "", data_no_header_2)

  input2 <- paste(data_no_header_2, collapse = "")  # 벡터화
  input2 <- gsub("\\s+", ", ", input2)   #문자뒤 콤마
  input2 <- gsub("^, 1,", "1,", input2)  #첫번째 1의 수정

  if(type =="check"){
    check =  list(header, data_no_header_2, input2)
  }else{

    # 헤더와 데이터를 사용하여 행렬 생성
    res <- lavaan::lav_matrix_lower2full(as.numeric(unlist(strsplit(input2, ","))))
    colnames(res) <- header
    rownames(res) <- header
    cat("\n\n")
    return(res)

    # return(res)


    all =  list(header, data_no_header_2, input2, res)
  }

switch(type,
       res = res,
       all= all,
       check = check )
}
# lav_matrix <- function(text) {
#   # 텍스트를 줄 단위로 분할
#   lines <- strsplit(text, "\n")[[1]]
#
#   # 헤더 추출
#   header <- gsub("\\*", "", lines)
#   header <- gsub("\\d+\\.?\\d*", "", header)
#   header <- gsub("\\s+", "", header)
#   header <- gsub("\\.+\\s*$", "", header)  # 마지막에 오는 기호들 제거
#   header <- gsub("\\.-\\s*$", "", header)
#   header <- header[header != ""]
#
#   #음수의 위치를 추출해서 앞부분 제거된 결과의 위치에 맞추어서 표시 하기
#   data_no_header_2 <- gsub("(?<=[^\\d])-(?=\\s)", "- ", lines, perl = TRUE)
#   data_no_header_2 <- trimws(data_no_header_2)
#   data_no_header_2 <- data_no_header_2[data_no_header_2 != ""]
#
#   data_no_header_2 <- gsub("[가-힣]+", "", data_no_header_2, perl = TRUE)
#   data_no_header_2 <- gsub("[a-zA-Z]+", "", data_no_header_2, perl = TRUE)
#   data_no_header_2 <- gsub("\\*", "", data_no_header_2)  #별표시 삭제
#   input2 <- paste(data_no_header_2, collapse = "")  # 벡터화
#   input2 <- gsub("\\s+", ", ", input2)   #문자뒤 콤마
#   input2 <- gsub("^, 1,", "1,", input2)  #첫번째 1의 수정
#
#   # 헤더와 데이터를 사용하여 행렬 생성
#   res <- lavaan::lav_matrix_lower2full(as.numeric(unlist(strsplit(input2, ","))))
#   colnames(res) <- header
#   rownames(res) <- header
#
#   cat("\n\n")
#   # return(res)
#   return(res)
#
# }

