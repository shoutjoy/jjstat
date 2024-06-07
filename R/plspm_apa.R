#' plspm_apa_paths
#'
#' @param data plspm_paths_sig
#'
#' @return interpretation
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시 데이터 프레임
#' jutpls_boot1 <- data.frame(
#'   endogenous = c("진로동기", "진로태도", "진로준비", "진로준비", "진로준비"),
#'   exogenous = c("자기효능감", "자기효능감", "자기효능감", "진로동기", "진로태도"),
#'   Est = c(0.880, -0.674, 0.429, 0.446, 0.007),
#'   SE = c(0.050, 0.078, 0.129, 0.121, 0.078),
#'   t = c(17.597, -8.656, 3.330, 3.694, 0.087),
#'   p.value = c("<.001", "<.001", 0.001, "<.001", 0.931),
#'   sig = c("***", "***", "**", "***", "ns")
#' )
#'
#' # 함수 실행
#' plspm_apa_paths(jutpls_boot)
#'
#' jutpls_boot %>% plspm_paths_sig()%>% plspm_apa_paths()
#' #'
#' jutpls_boot %>% plspm_apa_paths()
#' jutpls_boot %>% plspm_paths_sig()%>% plspm_apa_paths()
#' jutpls_boot %>% plspm_paths_sig(unite=T)%>% plspm_apa_paths()
#'
#'
#' }
#'
plspm_apa_paths <- function(data) {

  if(length(data) ==13){
    df = data %>% plspm_paths_sig()

  }else if(length(data)==7){
    df = data
  }else if(length(data)==5){
    df <- data %>%
      mutate(exogenous = str_extract(Path, "^[^ ]+"),
             endogenous = str_extract(Path, "(?<=-> )[가-힣]+"))%>%
      p_mark_sig()
  }


  results <- df %>%
    mutate(hypothesis = paste0("가설[", row_number(), "]: ",
                               exogenous, "에서 ", endogenous, "에 미치는 효과는 ",
                               ifelse(sig == "ns", "통계적으로 유의하지 않았다",
                                      "통계적으로 유의하였다"),
                               "(Est = ", Est, ", t = ", t, ", p = ", p.value, ").")) %>%
    pull(hypothesis)

  cat("PLS구조방정식의 경로분석 결과, 다음과 같다.\n")
  for (result in results) {
    cat(result, "\n")
  }
}


#' Function to interpret plspm results
#'
#' @param data plspm_ind_effect_test data
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Example data frame
#' data11 <- tibble::tibble(
#'   paths = c("IMAG -> EXPE -> QUAL -> VAL -> SAT -> LOY",
#'             "IMAG -> EXPE -> QUAL -> SAT -> LOY",
#'             "IMAG -> EXPE -> VAL -> SAT -> LOY",
#'             "IMAG -> EXPE -> SAT -> LOY",
#'             "IMAG -> SAT -> LOY"),
#'   ind_effect = c(0.0970, 0.0297, 0.0178, -0.000790, 0.0995),
#'   SE = c(4.44e-4, 4.07e-3, 2.98e-3, 1.91e-2, 3.35e-2),
#'   Z = c(218.39, 7.31, 5.98, -0.0413, 2.97),
#'   p.value = c(0, 2.76e-13, 2.26e-9, 9.67e-1, 3.00e-3),
#'   sig = c("***", "***", "***", "ns", "**")
#' )
#'
#' plspm_apa_ind(data11)
#'
#' ## exaample
#' plspm_ind_effect_test(satpls_boot)%>%plspm_apa_ind()
#'
#' }
plspm_apa_ind <- function(data) {
  data <- data %>%
    mutate(
      p.value = ifelse(p.value < 0.001, "< .001",
                       as.character( paste0("= ",round(p.value,3)) )),
      interpretation = ifelse(sig == "ns", "통계적으로 유의하지 않았다",
                              "통계적으로 유의하였다")
    ) %>%
    mutate(interpretation_text = paste0(
      "간접효과[", row_number(), "] : ", paths, " 의 경로는 ", interpretation,
      "(est = ", round(ind_effect, 4), ", Z = ", round(Z, 2), ", p ", p.value, ")"
    ))

  return(data$interpretation_text)
}



#' plspm_apa_grp
#'
#' @param data plspm_grp_summary
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example usage
#' data14 <- tibble::tribble(
#'   ~paths, ~global, ~`group.고등학교`, ~`group.중학교`, ~diff, ~t, ~p.value, ~sig,
#'   "자기효능감->진로동기", 0.88, 0.894, 0.81, 0.085, 0.908, 0.183, "no",
#'   "자기효능감->진로태도", -0.674, -0.749, 0.438, 1.19, 2.49, 0.007, "yes",
#'   "자기효능감->진로준비", 0.429, 0.542, 0.595, 0.053, 0.395, 0.347, "no",
#'   "진로동기->진로준비", 0.446, 0.431, 0.385, 0.046, 0.232, 0.408, "no",
#'   "진로태도->진로준비", 0.007, 0.098, -0.126, 0.224, 0.626, 0.266, "no"
#' )
#'
#' plspm_apa_grp(data14)
#'
#' }
#'
plspm_apa_grp <- function(data) {
  # Extracting the relevant columns dynamically
  group_cols <- grep("^group\\.", colnames(data), value = TRUE)
  cols <- c("paths", "global", group_cols, "diff", "p.value", "sig")

  # Check if 't' column is present
  if ("t" %in% colnames(data)) {
    cols <- c(cols, "t")
  }

  data <- data[cols]

  # Creating the output text
  output <- "집단간 차이 분석결과는 다음과 같았다.\n"

  for (i in 1:nrow(data)) {
    path <- data$paths[i]
    global_est <- data$global[i]
    group1_est <- data[[group_cols[1]]][i]
    group2_est <- data[[group_cols[2]]][i]
    diff <- data$diff[i]
    p_val <- data$p.value[i]
    sig <- ifelse(data$sig[i] == "yes", "통계적으로 유의하였다.", "통계적으로 유의하지 않았다.")

    group1_name <- sub("group\\.", "", group_cols[1])
    group2_name <- sub("group\\.", "", group_cols[2])

    output <- paste0(output,
                     "비교[", i, "] : ", path, " (est=", global_est,
                     ")에 대한 그룹간 비교결과 ", group1_name, "(est=", group1_est,
                     ")와 ", group2_name, "(est=", group2_est,
                     ")의 차이는 ", sig,
                     "(diff = ", diff, ", p = ", p_val, ").\n")
  }

  cat("\n",output,"\n")
}
