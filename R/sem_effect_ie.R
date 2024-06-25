#' SEM indirect effect
#'
#' @param model_sem lavaan objgect
#' @param effect1 "IE"
#' @param effect2 "IE"
#' @param label label
#' @param ci confidence interval
#'
#' @return parameterEstimates()
#' @export
#'
#' @examples
#'
#' \dontrun{

#' #library(lavaan) # only needed once per session
#'
#' models1 <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'     # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'       # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#'
#' models2 ='
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'      # regressions
#'     dem60 ~ a1*ind60
#'     dem65 ~ a2*ind60 + a3*dem60
#'  #NEw
#'    ie := a1*a3
#'    te := a2 +a1*a3
#'    diff := a1 - a3
#'
#'
#'
#'
#' fits1 <- sem(models1, data=PoliticalDemocracy)
#' fits2 <- sem(models2, data=PoliticalDemocracy)
#' summary(fits1, standardized=TRUE)
#' summary(fits2, standardized=TRUE)
#' fits2 %>% sem_effect_ie()
#' }
#'
sem_effect_ie = function(model_sem,
                     effect1 ="IE", effect2="IE", label=FALSE, ci= TRUE ){

  library(dplyr)
  library(tidyr)

  res = parameterEstimates(model_sem, ci=ci, stand=T)%>%
    filter(op==":=" & str_detect(lhs, effect1) |str_detect(lhs, effect2))

  if(ci){
    res = res%>%select("Path"=lhs, rhs, est, "std"= std.all,
                       se, z, pvalue, ci.lower, ci.upper)%>%
      p_mark_sig("pvalue" )%>%select(-vars,-se, -pvalue)%>%
      Round(3)%>%
      unite(CI_95, ci.lower, ci.upper, sep=", ")%>%
      unite(z, z, sig, sep=" ")%>%
      mutate(CI_95p = paste0("[", CI_95,"]"))%>%select(-CI_95)


  }else{
    res = res%>%select("Path"=lhs, rhs, est, "std"= std.all, se, z, pvalue)%>%
      p_mark_sig("pvalue") %>%select(-vars)
  }


  if(label){
    res = res
  }else{
    res = res%>% select(-rhs)
  }
  return(res)
}



#' SEM indirect effect
#'
#' @param model_sem lavaan objgect
#' @param effect1 "IE"
#' @param effect2 "IE"
#' @param label label
#' @param ci confidence interval
#'
#' @return parameterEstimates()
#' @export
#'
#' @examples
#'
#' \dontrun{

#' #library(lavaan) # only needed once per session
#'
#' models1 <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'     # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'       # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#'
#' models2 ='
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'      # regressions
#'     dem60 ~ a1*ind60
#'     dem65 ~ a2*ind60 + a3*dem60
#'  #NEw
#'    ie := a1*a3
#'    te := a2 +a1*a3
#'    diff := a1 - a3
#'
#'
#'
#'
#' fits1 <- sem(models1, data=PoliticalDemocracy)
#' fits2 <- sem(models2, data=PoliticalDemocracy)
#' summary(fits1, standardized=TRUE)
#' summary(fits2, standardized=TRUE)
#' fits2 %>% IE_effect()
#' }
#'
IE_effect = function(model_sem,
                         effect1 ="IE", effect2="IE", label=FALSE, ci=F ){
  library(dplyr)
  library(tidyr)
  res = parameterEstimates(model_sem, ci=ci, stand=T)%>%
    filter(op==":=" & str_detect(lhs, effect1) |str_detect(lhs, effect2))

  if(ci){
    res = res%>%select("Path"=lhs, rhs, est, "std"= std.all,
                       se, z, pvalue, ci.lower, ci.upper)%>%
      p_mark_sig("pvalue" )%>%select(-vars,-se, -pvalue)%>%
      Round(3)%>%
      unite(CI_95, ci.lower, ci.upper, sep=", ")%>%
      unite(z, z, sig, sep=" ")%>%
      mutate(CI_95p = paste0("[", CI_95,"]"))%>%select(-CI_95)


  }else{
    res = res%>%select("Path"=lhs, rhs, est, "std"= std.all, se, z, pvalue)%>%
      p_mark_sig("pvalue") %>%select(-vars)
  }


  if(label){
    res = res
  }else{
    res = res%>% select(-rhs)
  }
  return(res)
}



#' Total effect
#'
#' @param model_sem lavaan objgect
#' @param effect1 "IE"
#' @param effect2 "IE"
#' @param label label
#' @param ci confidence interval
#'
#' @return parameterEstimates()
#' @export
#'
#' @examples
#'
#' \dontrun{

#' #library(lavaan) # only needed once per session
#'
#' models1 <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'     # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'       # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#'
#' models2 ='
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'      # regressions
#'     dem60 ~ a1*ind60
#'     dem65 ~ a2*ind60 + a3*dem60
#'  #NEw
#'    ie := a1*a3
#'    te := a2 +a1*a3
#'    diff := a1 - a3
#'
#'
#'
#'
#' fits1 <- sem(models1, data=PoliticalDemocracy)
#' fits2 <- sem(models2, data=PoliticalDemocracy)
#' summary(fits1, standardized=TRUE)
#' summary(fits2, standardized=TRUE)
#' fits2 %>% TE_effect()
#' }
#'
TE_effect = function(model_sem, effect1="TE", effect2="te",
                     label=FALSE, ci=F){
  library(dplyr)
  library(tidyr)
  res = parameterEstimates(model_sem, ci=ci, stand=T)%>%
    filter(op==":=" & str_detect(lhs, effect1))%>%
    filter(str_detect(lhs, effect2))%>%
    select("가설" = lhs, "Path" = rhs, est, "std"= std.all, se, z, pvalue)%>%
    p_mark_sig("pvalue") %>%select(-vars)

  if(label){
    res = res
  }else{
    res = res%>% select(-Path)
  }
  return(res)
}


#' Total effect
#'
#' @param model_sem lavaan objgect
#' @param effect1 "IE"
#' @param effect2 "IE"
#' @param label label
#' @param ci confidence interval
#'
#' @return parameterEstimates()
#' @export
#'
#' @examples
#'
#' \dontrun{

#' #library(lavaan) # only needed once per session
#'
#' models1 <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'     # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'       # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#'
#' models2 ='
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'      # regressions
#'     dem60 ~ a1*ind60
#'     dem65 ~ a2*ind60 + a3*dem60
#'  #NEw
#'    ie := a1*a3
#'    te := a2 +a1*a3
#'    diff := a1 - a3
#'
#'
#'
#'
#' fits1 <- sem(models1, data=PoliticalDemocracy)
#' fits2 <- sem(models2, data=PoliticalDemocracy)
#' summary(fits1, standardized=TRUE)
#' summary(fits2, standardized=TRUE)
#' fits2 %>% sem_effect_te()
#' }
#'
sem_effect_te = function(model_sem, effect1="TE", effect2="te",
                     label=FALSE, ci=TRUE){
  library(dplyr)
  library(tidyr)
  res = parameterEstimates(model_sem, ci=ci, stand=T)%>%
    filter(op==":=" & str_detect(lhs, effect1))%>%
    filter(str_detect(lhs, effect2))%>%
    select("가설" = lhs, "Path" = rhs, est, "std"= std.all, se, z, pvalue)%>%
    p_mark_sig("pvalue") %>%select(-vars)

  if(label){
    res = res
  }else{
    res = res%>% select(-Path)
  }
  return(res)
}



#' Diff effect
#'
#' @param model_sem lavaan objgect
#' @param effect1 "Diff"
#' @param effect2 "diff"
#' @param label label
#' @param ci confidence interval
#'
#' @return parameterEstimates()
#' @export
#'
#' @examples
#'
#' \dontrun{

#' #library(lavaan) # only needed once per session
#'
#' models1 <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'     # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'       # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#'
#' models2 ='
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'      # regressions
#'     dem60 ~ a1*ind60
#'     dem65 ~ a2*ind60 + a3*dem60
#'  #NEw
#'    ie := a1*a3
#'    te := a2 +a1*a3
#'    Diff := a1 - a3
#'
#'
#'
#'
#' fits1 <- sem(models1, data=PoliticalDemocracy)
#' fits2 <- sem(models2, data=PoliticalDemocracy)
#' summary(fits1, standardized=TRUE)
#' summary(fits2, standardized=TRUE)
#' fits2 %>% sem_effect_diff()
#' }
sem_effect_diff = function(model_sem,
                       effect1 ="Diff",
                       effect2 ="diff",
                       label= FALSE,
                       ci= TRUE){
  library(dplyr)
  library(tidyr)
  if(ci){
    res = parameterEstimates(model_sem, ci = ci, stand = T)%>%
      filter(op == ":=" & str_detect(lhs, effect1) | str_detect(lhs, effect2))%>%
      select("가설" = lhs,"경로효과차이" = rhs, est, "std" = std.all, se,
             z, pvalue, ci.lower, ci.upper)%>%
      p_mark_sig("pvalue") %>%select(-vars)%>%
      Round(3)%>%
      unite(z, z, sig, sep=" ")%>%
      unite(CI_95, ci.lower, ci.upper, sep=", ")%>%
      # unite(z, z, sig, sep=" ")%>%
      mutate(CI_95p = paste0("[", CI_95,"]"))%>%select(-CI_95,-se)

  }else{
    res = parameterEstimates(model_sem, ci = ci, stand = T)%>%
      filter(op == ":=" & str_detect(lhs, effect1) | str_detect(lhs, effect2))%>%
      select("가설" = lhs,"경로효과차이" = rhs, est, "std"= std.all, se,
             z, pvalue)%>%
      p_mark_sig("pvalue") %>%select(-vars)
  }

  if(label){
    res = res
  }else{
    res = res%>% select(-경로효과차이)
  }
  return(res)
}



#' Diff effect
#'
#' @param model_sem lavaan objgect
#' @param effect1 "Diff"
#' @param effect2 "diff"
#' @param label label
#' @param ci confidence interval
#'
#' @return parameterEstimates()
#' @export
#'
#' @examples
#'
#' \dontrun{

#' #library(lavaan) # only needed once per session
#'
#' models1 <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'     # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'       # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#'
#' models2 ='
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'      # regressions
#'     dem60 ~ a1*ind60
#'     dem65 ~ a2*ind60 + a3*dem60
#'  #NEw
#'    ie := a1*a3
#'    te := a2 +a1*a3
#'    Diff := a1 - a3
#'
#'
#'
#'
#' fits1 <- sem(models1, data=PoliticalDemocracy)
#' fits2 <- sem(models2, data=PoliticalDemocracy)
#' summary(fits1, standardized=TRUE)
#' summary(fits2, standardized=TRUE)
#' fits2 %>% Diff_effect()
#' }
Diff_effect = function(model_sem,
                       effect1 ="Diff",
                       effect2 ="diff",
                       label= FALSE,
                       ci=FALSE){
  library(dplyr)
  library(tidyr)
  if(ci){
    res = parameterEstimates(model_sem, ci = ci, stand = T)%>%
      filter(op == ":=" & str_detect(lhs, effect1) | str_detect(lhs, effect2))%>%
      select("가설" = lhs,"경로효과차이" = rhs, est, "std" = std.all, se,
             z, pvalue, ci.lower, ci.upper)%>%
      p_mark_sig("pvalue") %>%select(-vars)%>%
      Round(3)%>%
      unite(z, z, sig, sep=" ")%>%
      unite(CI_95, ci.lower, ci.upper, sep=", ")%>%
      # unite(z, z, sig, sep=" ")%>%
      mutate(CI_95p = paste0("[", CI_95,"]"))%>%select(-CI_95,-se)

  }else{
    res = parameterEstimates(model_sem, ci = ci, stand = T)%>%
      filter(op == ":=" & str_detect(lhs, effect1) | str_detect(lhs, effect2))%>%
      select("가설" = lhs,"경로효과차이" = rhs, est, "std"= std.all, se,
             z, pvalue)%>%
      p_mark_sig("pvalue") %>%select(-vars)
  }

  if(label){
    res = res
  }else{
    res = res%>% select(-경로효과차이)
  }
  return(res)
}
