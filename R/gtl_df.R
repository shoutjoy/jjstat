#' Google translate data.frame
#' #gtl_df() dataframe 번역 ----------
#' #영어논문을 만들 때, 표에 들어가는 단어를 번역하여 넣기
#' #너무 많은 수는 오래걸림--> 웹페이지를 갔다와야 하므로...
#' Title
#'
#' @param df data.frame
#' @param col Select the column you want to translate
#' @export
#'
#' @examples
#' \dontrun{
#'  df0 <-  data.frame(word=c("온라인","진행","교육"),
#'                    n=c(100, 200, 300))
#'  df0 %>% gtl_df()
#' }
#'
#'
gtl_df <- function(df, col=1){

  df <-  df %>% as.data.frame()   #tibble to dataframe
  df[,col]<- as.character(df[,col])  #factor to character

  for( i in 1:nrow(df)){
    df[i,col]<- gtl(df[i,col])
  }
  df
}



#' Googole Translate korean to english
#'
#' @param text input your text
#' @param slang start language. default Korean
#' @param elang translated language. default English
#' @param show show Methode. 'translate' is console, 'browser'  is Print to your computer's browser, 'all' are There are two ways to output it. 'data' is char data
#' @examples
#' # example code
#' \dontrun{
#' #korean to english : default
#' g("안녕하세요 번역을 시작합니다!(by 박중희).", slang ="ko", elang = "en")
#' g("안녕하세요 번역을 시작합니다!(by 박중희).", "ko", "en")
#' # english to korean
#' g("Hello, let’s start translating! (by Park Joong-hee", slang ="ko", elang = "en")
#' g("Hello, let’s start translating! (by Park Joong-hee","ko","en")
#' #japan to korean
#' g("こんにちは翻訳を始めます！（byパク・ジュンヒ", slang ="ja", elang = "ko")
#'
#' # ko: korean , en: English , ja: Japan, china : zh
#' # https://cloud.google.com/translate/docs/languages?hl=ko
#' }
#' @export
#'
#'
gtl <- function (text = "hi.",
                 slang = "ko",
                 elang = "en",
                 show = "data") {
  library(tidyverse)
  library(rvest)
  library(httpuv)

  url = sprintf("https://translate.google.co.kr/m?sl=%s&hl=%s&q=%s",
                slang,
                elang,
                encodeURIComponent(text))

  node = rvest::html_nodes(read_html(url), ".result-container")

  result= rvest::html_text(node)


  if(show == "all"){
    res = list(input_text= text,
               translate= result,
               View_browse=browseURL(url)
    )
    res

  }else if(show == "translate"){
    res = result
    cat(res)

  }else if(show == "data"){
    res = result
    res

  }else if(show == "browser"){
    res = paste("Input text:\n\n" ,input_text = text,
                "\n",
                "\n",
                "Translate result:\n\n",
                translate= result)
    return(res)
    cat("\n\n")
    # res
  }

}
