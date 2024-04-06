
#' DE,IE,TE Effect extraction
#'
#' @param x lavaan result
#' @param effect1 first, you must select effect1 = IE, DE, TE
#' @param caption title
#' @param effect2 '.' 2nd oprion
#' @param option  1(p_value) or 2(ci)
#'
#' @return effect result
#' @export
#'
med_effect <- function(x,
                       effect1="",
                       caption="",
                       effect2="." ,
                       option = 1      # 2: ci
                       # format="markdown"

){
  library(knitr)
  library(dplyr)
  library(stringr)

  if(option==1){  # p
    parameterEstimates(x, standardized = TRUE,ci=TRUE ) %>%
      dplyr::filter(op==":=") %>%
      dplyr::filter(str_detect(lhs, effect1) & str_detect(lhs,effect2)) %>%
      mutate(sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                     labels=c("***","**","*",""))) %>%
      dplyr::select(lhs,label, est, std.all,se, z, pvalue, sig, Parameter=rhs) #%>%
    # kable(format, digits=3, caption = caption)
  }  else if(option==2){   #ci
    parameterEstimates(x,standardized = T ) %>%
      dplyr::filter(op==":=") %>%
      dplyr::filter(str_detect(lhs, effect1) & str_detect(lhs, effect2) ) %>%
      mutate(sig=cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                     labels=c("***","**","*",""))) %>%
      dplyr::select(lhs,label, est,std.all, se, z, pvalue,  sig, ci.lower, ci.upper)
  }else if(option==3){
    parameterEstimates(x, standardized = TRUE, ci=TRUE ) %>%
      dplyr::filter(op==":=") %>%
      dplyr::filter(str_detect(lhs, effect1) & str_detect(lhs,effect2)) %>%
      mutate(sig = cut(pvalue,c(-Inf,0.001,0.01,0.05,1),
                     labels=c("***","**","*",""))) %>%
      dplyr::select(lhs,rhs, label, est, std.all,se, z, pvalue, sig, Parameter=rhs)
}


  }


