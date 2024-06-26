#' Kakao translaste
#'
#' @param text input text
#' @param slang  "ko"
#' @param elang "en"
#' @param show nomal, data is each sentence
#'
#' @return translate result
#' @export
#'

#'
k = function (text="",
              slang="en",
              elang="ko",
              show ="data") {
  # library("httr")
  # library("httpuv")

  # 줄바꿈 문자를 공백으로 변환
  clean_text <- gsub("\n", " ", text)

  response <- httr::POST("https://translate.kakao.com/translator/translate.json",
                         body=sprintf("queryLanguage=%s&resultLanguage=%s&q=%s",
                                      slang,
                                      elang,
                                      httpuv::encodeURIComponent(clean_text)),
          httr::add_headers(.headers=c("Referer"="https://translate.kakao.com/",
         "content-type"="application/x-www-form-urlencoded; charset=UTF-8"))
  )
  data = httr::content(response, "parsed")
  output = data$result$output

  #데이터를 list에서 character로 만들기
  data_result_input= unlist(data$result$input)
  data_result_output= unlist(data$result$output)
  #결과 정리
  resout = paste(data_result_output, collapse=" ")
  resin = paste(data_result_input, collapse=" ")

  if(show == "normal"){
    cat("Source language: \n\n", resin, "\n\n")
    cat("Translate language: \n\n", resout)

  } else if(show == "data"){
    resout
  }
}



#' Kakao translaste
#'
#' @param text input text
#' @param slang  "ko"
#' @param elang "en"
#' @param show nomal, data is each sentence
#'
#' @return translate result
#' @export
#'

#'

kakaoi = function (text="",
                   slang="en",
                   elang="ko",
                   show ="data") {
  library("httr")
  library("httpuv")

  clean_text <- gsub("\n", " ", text)

  response <- httr::POST("https://translate.kakao.com/translator/translate.json",
                         body=sprintf("queryLanguage=%s&resultLanguage=%s&q=%s",
                                      slang,
                                      elang,
                                      httpuv::encodeURIComponent(clean_text)),
       httr::add_headers(.headers=c("Referer"="https://translate.kakao.com/",
      "content-type"="application/x-www-form-urlencoded; charset=UTF-8"))
  )
  data = httr::content(response, "parsed")
  output = data$result$output

  #데이터를 list에서 character로 만들기
  data_result_input = unlist(data$result$input)
  data_result_output = unlist(data$result$output)
  #결과 정리
  resout = paste(data_result_output)
  resin = paste(data_result_input)

  if(show ==" normal"){
    cat("Source language: \n\n",resin,"\n\n")
    cat("Translate language: \n\n", resout)
    #  gsub("queryLanguage=en,","",resout)

  }else if(show == "data"){
    # res = list(source = resin,
    #           translate = resout)
    res = resout
    cat( "\n\n", res, "\n\n")
  }

}
