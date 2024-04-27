#' Title
#'
#' @param data
#' @param md
#'
#' @return
#' @export
#'
#' @examples
#'
#'
LRT_apa <- function(data, md=TRUE) {

  print(data)

  if(md){
    print( md(data, caption="로그우도비 테스트") )
  }
  model_name <- rownames(data)
  model_name1 <- model_name[1]
  model_name2 <- model_name[2]

  model1 <- data[1, ]
  model2 <- data[2, ]

  cat("\n\n 이 결과는 카이제곱차이를 통해서 모델을 비교하는 로그우도비테스트(Log likelihood Test)입니다(satorra.bentler, 2001). 각 모형의 적합도 지표를 살펴보면 다음과 같습니다.\n")
  cat(paste0("연구모형(", model_name1, ")의 AIC = ",
             as.character(round(model1["AIC"], 2)), ", BIC = ",
             as.character(round(model1["BIC"], 2)),  "이고, "))
  cat(paste0("대안모형(", model_name2, ")의 AIC = ",
             as.character(round(model2["AIC"], 2)), ", BIC = ",
             as.character(round(model2["BIC"], 2)), "입니다."), "\n")
  cat("적합도 지표인 AIC, BIC는 작을수록 모형의 적합도가 더 좋습니다. ")
  if (model1["AIC"] < model2["AIC"] & model1["BIC"] < model2["BIC"]) {
    cat("따라서, 연구모형이 대안모형에 비해 더 우수한 적합도를 보입니다.\n")
  } else {
    cat("따라서, 대안모형이 연구모형에 비해 더 우수한 적합도를 보입니다.\n")
  }

  cat("RMSEA (근사 평균 제곱 오차): 모델의 적합도를 0에서 1 사이의 범위로 나타내는 지표입니다. RMSEA 값이 0.05 이하라면 모델 적합도가 양호하다고 판단합니다.",
      "대안모델의 RMSEA =", as.character(round(model2["RMSEA"], 5)),"으로",
      ifelse(model2["RMSEA"]< 0.05,"모델적합도가 양호합니다. \n","모델적합도는 양호한 편은 아닙니다. \n"))

  cat("다음으로, 카이제곱 통계량인 Chisq를 살펴보겠습니다.\n")
  cat("연구모형의 Chisq(χ²) =", as.character(round(model1["Chisq"], 2)), "이고, 대안모형의 Chisq(χ²) =", as.character(round(model2["Chisq"], 2)), "입니다.\n")
  cat("카이제곱 통계량은 데이터와 모형 간의 차이를 나타내는 지표로, 작을수록 데이터를 잘 설명한다는 것을 의미합니다.")
  if (!is.na(model1["Chisq"]) & !is.na(model2["Chisq"])) {
    if (model1["Chisq"] < model2["Chisq"]) {
      cat(" 분석결과, 연구모형이 대안모형에 비해 데이터를 더 잘 설명한다고 할 수 있습니다.\n")
    } else {
      cat(" 분석결과, 대안모형이 연구모형에 비해 데이터를 더 잘 설명한다고 할 수 있습니다.\n")
    }
  } else {
    cat("Chisq(χ²)가 모두 NA입니다. 데이터에 대한 정보가 충분하지 않습니다.\n")
  }

  if (!is.na(model2["Chisq.diff"])) {
    cat("Chisq(χ²) diff는 두 모형 간의 카이제곱 통계량의 차이를 나타냅니다. ")
    cat("Chisq(χ²) diff =", as.character(round(model2["Chisq.diff"], 2)), "으로, ")
    cat("이 값이 크면 클수록 두 모형 간의 차이가 크다는 것을 의미합니다.\n")
  } else {
    cat("Chisq diff가 NA입니다. 데이터에 대한 정보가 충분하지 않습니다.\n")
  }

  cat("마지막으로, Pr(>Chisq)는 카이제곱 검정의 유의확률로,")
  cat("두 모형 간의 차이가 통계적으로 유의한지를 나타냅니다. ")
  cat("이 값은 p = ",
      as.character(format(model2["Pr..Chisq."], digits = 3, scientific = TRUE)), "입니다. ")
  cat(ifelse(model2["Pr..Chisq."] < 0.05,
             "이 값은 두 모형 간 통계적으로 유의한 차이가 있다는 것을 의미합니다. ",
             "이 값은 유의하지 않은 값이며, 이는 통계적으로 두 모형 간의 차이가 유의하지 않다는 것을 의미합니다."), "\n\n")
}
