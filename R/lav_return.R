
#' lavaan::sem으로 분석한 함수에서 model추출
#'
#' @param sem_res sem data
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시 사용법
#' data(satisfaction)
#' model1 <- "
#' IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5
#' EXPE =~ expe1 + expe2 + expe3 + expe4 + expe5
#' QUAL =~ qual1 + qual2 + qual3 + qual4 + qual5
#' VAL =~ val1 + val2 + val3 + val4
#' SAT =~ sat1 + sat2 + sat3 + sat4
#' LOY =~ loy1 + loy2 + loy3 + loy4
#'
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~  QUAL + EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#'
#' sat_sem0 <- sem(model1, data = satisfaction)
#'
#' # 함수 결과 확인
#' lav_return_model(sat_sem0)
#' lav_return_model(sat_sem0)%>%cat("\n")
#' #data
#' lav_return_data(sat_sem0)
#'
#' #lav sem reuslt  model to blocks
#' lav_return_model(sat_sem0)%>%
#'   lav_extract_mm()%>%
#'   plspm_lav2blocks()
#'
#'
#' }
#'
#'
lav_return_model <- function(sem_res) {
  library(dplyr)
  library(lavaan)
  # Extract parameter table
  param_table <- lavaan::parameterTable(sem_res)

  # Create empty lists to hold different parts of the model
  latent_vars <- list()
  regressions <- list()

  # Populate the lists
  for (i in 1:nrow(param_table)) {
    row <- param_table[i, ]
    if (row$op == "=~") {
      if (!(row$lhs %in% names(latent_vars))) {
        latent_vars[[row$lhs]] <- list()
      }
      latent_vars[[row$lhs]] <- c(latent_vars[[row$lhs]], row$rhs)
    } else if (row$op == "~") {
      regressions <- c(regressions, paste(row$lhs, row$op, row$rhs))
    }
  }

  # Create the model string
  model_string <- ""

  for (latent in names(latent_vars)) {
    model_string <- paste0(model_string, latent, " =~ ", paste(latent_vars[[latent]], collapse = " + "), "\n")
  }

  for (reg in regressions) {
    model_string <- paste0(model_string, reg, "\n")
  }

  return(model_string)
}



#' lavaan결과에서 데이터 추출하는 함수
#'
#' @param sem_res sem result
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data(satisfaction)
#' model1 <- "
#' IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5
#' EXPE =~ expe1 + expe2 + expe3 + expe4 + expe5
#' QUAL =~ qual1 + qual2 + qual3 + qual4 + qual5
#' VAL =~ val1 + val2 + val3 + val4
#' SAT =~ sat1 + sat2 + sat3 + sat4
#' LOY =~ loy1 + loy2 + loy3 + loy4
#'
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~  QUAL + EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#'
#' sat_sem0 <- sem(model1, data = satisfaction)
#'
#' # 함수 결과 확인
#' lav_return_data(sat_sem0)
#' lav_return_data(sat_sem0)%>%head()

#' }
#'
#'
lav_return_data <- function(sem_res) {
  library(dplyr)
  library(lavaan)
  # Check if the input is a lavaan object
  if (!inherits(sem_res, "lavaan")) {
    stop("The input object is not a valid lavaan object.")
  }

  # Extract the data
  data <- lavaan::lavInspect(sem_res, "data")

  return(data)
}




#' lavaan:sem에서 blocks을 생성
#'
#' @param sem_res sem result
#'
#' @return list
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data(satisfaction)
#' model1 <- "
#' IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5
#' EXPE =~ expe1 + expe2 + expe3 + expe4 + expe5
#' QUAL =~ qual1 + qual2 + qual3 + qual4 + qual5
#' VAL =~ val1 + val2 + val3 + val4
#' SAT =~ sat1 + sat2 + sat3 + sat4
#' LOY =~ loy1 + loy2 + loy3 + loy4
#'
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~  QUAL + EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#'
#' sat_sem0 <- sem(model1, data = satisfaction)
#'
#' # 함수 결과 확인
#' lav_return_blocks(sat_sem0)
#' }
#'
lav_return_blocks =function(sem_res){
  library(dplyr)
  library(lavaan)

res1  = dplyr::bind_cols(
        lat = sem_res@ParTable$lhs ,
        OP = sem_res@ParTable$op,
        mm =sem_res@ParTable$rhs )%>%
       dplyr::filter(OP=="=~")

# 리스트로 변환
  result_list = res1 %>%
    group_by(lat) %>%
    summarise(mm = list(mm)) %>%
    deframe()

  return(result_list)

}

