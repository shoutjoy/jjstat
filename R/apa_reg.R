
#' regression APA report
#'
#' @param model lm_data
#' @param digits 3
#'
#' @return report
#' @export
#'
#' @examples
#' \dontrun{
#' lm(mpg ~ hp + wt + disp + qsec, data = mtcars) %>% reg_apa()
#' }
#' reg1
reg_apa <- function(model, digits= 3) {
  # Extracting coefficients
  coeffs <- coef(model)
  summary_model = summary(lm.beta::lm.beta(model))

  vars = model$call %>%formula() %>%as.character()
  dv = vars[2]
  ivs = vars[3]

  reg_fit = model%>% broom::glance()
  # Extracting p-values
  p_vals <- summary(model)$coefficients[, 4]
  DF = model$df.residual

  tidy_model = broom::tidy(lm.beta::lm.beta(model))

  sort_std = tidy_model[-1,]%>%arrange(desc(std_estimate))%>%
    select(term, std_estimate,  p.value) %>%
    p_mark_sig()%>%
    replace_df(pattern="ns",imp="")%>%
    mutate(Term = paste0(term,"(β = ",
                         round(std_estimate, 3),")",sig))%>%
    dplyr::select(Term)%>%unlist()%>%
    paste(collapse =" > ")%>%as.character()



  # Formatting coefficients and p-values
  formatted_coeffs <- format(coeffs, digits = 3)
  formatted_p_vals <- ifelse(p_vals < 0.001, "p < .001",
                             formatC(p_vals, format = "f", digits = 5))


  explain = ifelse(reg_fit$r.squared > 0.6, "뛰어난(excellent)설명력",
                   ifelse(reg_fit$r.squared > 0.4, "좋은(good)설명력",
                          ifelse(reg_fit$r.squared > 0.2,
                                 "충분한(adequate)설명력", "부족한 설명력")))

  ## Printing the results
  res_msg1 = paste0("설명변수(", dv ,")에 대한 예측변수(", ivs,
                    ")의 회귀분석 결과, 회귀모형은 통계적으로 유의하였다",
                    "(F(", reg_fit$df, ", ", reg_fit$df.residual, ") = ",
                    round(reg_fit$statistic, 2), ", p-value ",
                    formatC(
                      ifelse(reg_fit$p.value < 0.001," < .001)." ,
                             paste0("= ",reg_fit$p.value, "%).")),
                      format = "f", digits = 12),
                    " 모델의 설명력(R2) = ",round(reg_fit$r.squared*100, 2),"%으로, ",
                    explain,"이라고 할 수 있다.",
                    " 조정된 설명력(", "adj.R2)은 = ",round(reg_fit$adj.r.squared*100, 2),"%으로 나타났다.",
                    " 표준화 회귀계수의 크기는 순서대로 확인한 결과, ",
                    sort_std,"순으로 나타났다. 이는 ",dv,
                    "에 큰 영향을 미치는 변수를 추측해볼 수 있다. 실제 통계적으로 유의한 차이 여부는 Wald Test를 통해서 확인할 수 있다.  ",
                    "\n")

  # Loop through each coefficient
  res=c()
  for (i in seq_along(coeffs)) {
    if (i == 1) {
      next  # Skip the intercept
    }

    # Extracting variable name
    variable_name <- names(coeffs)[i]

    res[i]= paste0(dv ,"(dv)에 관한 ", variable_name,
                   "(iv)는 통계적으로 유의하였다 (b =",
                   formatted_coeffs[i],
                   ", t(", model$df.residual, ") = ", formatC(coeffs[i]/summary(model)$coefficients[i, 2], digits = 3),
                   ", p = ", formatted_p_vals[i], ").",
                   " 즉, ",variable_name,"가 한단위 증가할 수록 ",dv,"의 비표준화계수 b = ",
                   round(coeffs[i], 3),"만큼",
                   ifelse(coeffs[i] > 0," 증가한다고 할 수 있다. ",
                          " 감소한다고 할 수 있다. " ),
                   "\n")
  }

  options(tibble.width = Inf)
  #  print(summary_model)
  print(tidy_model%>% p_mark_sig())

  print(reg_fit)
  print(sort_std)

  if (length(res) > 0) {
    cat("\n",
        res_msg1,
        "\n\n", res[-1])
  }

}


