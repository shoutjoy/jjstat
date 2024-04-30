#' Functions to convert character to numeric
#'
#' @param data data.frame
#' @param char_col Specify which columns should be kept as char col
#' @param iter Select the range of variables to convert
#'
#' @return  data.frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Replacing a character variable with numeric
#' mysummary(sats, "sux1") %>%
#'     mutate(across(everything(), ~ format(., digits = 4))) %>%
#'     tibble() %>% Char2num()
#'
#'
#' ## Set remove=TRUE if you only want to process the selected variable and output only that.
#'   mysummary(sats, "sux1") %>%
#'   mutate(across(everything(), ~ format(., digits = 5))) %>%
#'     tibble() %>%Char2num(auto=FALSE, iter= 4:ncol(.), remove= TRUE)
#'
#' ##
#' mysummary(sats, "sux1") %>%
#'   mutate(across(everything(), ~ format(., digits = 5))) %>%
#'     tibble() %>%Char2num(auto=FALSE, iter= 4:ncol(.), remove= FALSE)
#' ## same
#'   mysummary(sats, "sux1") %>%
#'      mutate(across(everything(), ~ format(., digits = 5))) %>%
#'      tibble() %>%Char2num(auto=FALSE, iter= 4:ncol(.))
#'
#'
#' #새로운 데이터
#' reg_text <- "var B se b t
#' 성취감 -.03 .09 -.03 -.33
#' 인정감 .24 .09 .20 2.50
#' 책임감 .30 .08 .23 3.56***
#' 발전성 -.08 .06 -.10 -1.29
#' 감독 .06 .05 .08 1.35
#' 보수 -.14 .07 -.18 -2.01*
#' 대인관계 .06 .06 .06 .89
#' 작업환경 .33 .09 .34 3.82***
#' 안정성 .17 .05 .26 3.42**
#' 성별 -.05 .09 -.03 -.52
#' 연령 -.20 .07 -.22 -3.01**
#' 학력 .05 .07 .04 .70
#' 소득 .00 .04 .01 .12
#' 종사기간 .17 .06 .23 2.71**
#' 기관규모 -.10 .03 -.20 -3.36**"
#'
#' # 함수 호출
#' text2df(reg_text) %>% tibble()
#' text2df(reg_text)%>% Char2num() %>% tibble()
#' text2df(reg_text)%>% Char2num(iter=2:4) %>% tibble()
#' }
#'
Char2num <- function(data, iter = NULL, char_col=1) {
  # Check for columns with combined numeric and '*' values
  combined_columns <- sapply(data, function(col) any(grepl("\\d+\\*", col)))

  # Exclude combined columns from iter
  if (is.null(iter)) {
    iter <- ifelse(combined_columns, which(!combined_columns), 1:ncol(data))
    # Exclude first and last column from iter
    # Exclude first and last column from iter
    iter <- iter[!iter %in% c(char_col, ncol(data))]

  } else {
    # iter <- iter[!combined_columns]
    iter <- iter
  }
  # Data treatment
  for (i in iter) {
    data[[i]] <- suppressWarnings(as.numeric(data[[i]]))
    data[[i]][is.na(data[[i]]  )] <- 0
  }

  # Final data
  data
}






#' #'
#' Char2num <- function(data, auto=TRUE, iter = 1:ncol(data), remove = FALSE){
#'   data = data
#'
#'   #first col check
#'
#'   # data processing
#'   if(auto){
#'     #  iter = iter
#'     if(is.numeric(data[1])){
#'       iter = 1:ncol(data)
#'     }else{
#'       iter = 2:ncol(data)
#'     }
#'
#'
#'   }else{
#'
#'     if(remove){
#'       #data select
#'       data = data[,c(iter)]
#'       iter = seq_along(data)
#'     }else{
#'       iter = iter  # data not remove
#'     }
#'
#'   }
#'   #data treatment
#'   for( i in iter){
#'     data[[i]] <- suppressWarnings(as.numeric(data[[i]]))
#'     data[[i]][is.na(data[[i]])  ] <- 0
#'   }
#'   #final data
#'   data
#' }
