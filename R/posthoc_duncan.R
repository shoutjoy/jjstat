#' posthoc_duncan
#'
#' @param aov_res aov result
#' @param group_var group
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' aov_post_eng <- pwj3_long %>%
#'   filter(time == "Post" & factor == "eng") %>%
#'   aov(formula = score ~ Eng_Class)
#'
#' # Duncan 사후분석 수행
#' posthoc_duncan(aov_post_eng, "Eng_Class")
#' }
#'
#'
posthoc_duncan <- function(aov_res, group_var) {
  # Duncan 사후분석 수행
  duncan_result <- agricolae::duncan.test(aov_res,
                                          group = group_var,
                                          group = TRUE,
                                          console = TRUE)

  # 결과 반환
  return(duncan_result)
}
