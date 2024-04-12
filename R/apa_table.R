#' Report frequency analysis result
#'
#' @param data table_df objecte
#' @param print consolt outpur
#' @param md markdown_table output
#'
#' @return report data apa
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' sats %>%table_df(digits=3) %>% table_apa()
#' sats %>%table_df(digits=3) %>% table_apa(print=F)
#' sats %>%table_df(digits=3) %>% table_apa(print=T, md= T)
#' }
#'
table_apa <- function(data, print = TRUE, md = FALSE) {
  data <- data

  if(print){
    print(data)
  }
  if(md){
    print(markdown_table(data, caption ="Data frequency analysis results") )
  }


  cat("\n 데이터의 빈도분석 결과는 다음과 같이 나타났다.\n\n")

  for (term in unique(data$Term)) {
    cat(paste0(term, "에는 "))
    levels <- data[data$Term == term, c("Level", "Freq", "Prop(%)")]
    for (i in 1:nrow(levels)) {
      cat(levels[i, "Level"], paste0(levels[i, "Freq"], "(", levels[i, "Prop(%)"], "%)명"))
      if (i < nrow(levels)) {
        cat(", ")
      }
    }
    cat("으로 나타났다. ")
  }

  cat("\n\n")
}
