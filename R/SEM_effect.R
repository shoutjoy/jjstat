#' SEM direct effect result
#'
#' @param model_sem  lavaan object
#' @param effect  select op
#' @param effect2  select op 2nd option
#'
#' @return repot
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  #library(lavaan) # only needed once per session
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
#' '
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
#' '
#' fits1 <- sem(models1, data=PoliticalDemocracy)
#' fits2 <- sem(models2, data=PoliticalDemocracy)
#' summary(fits1, standardized=TRUE)
#' summary(fits2, standardized=TRUE)
#'
#' parameterEstimates(fits1, ci=F, stand=T)
#' parameterEstimates(fits2, ci=F, stand=T)
#' DE_effect(fits1)
#' DE_effect(fits1, effect="=~")
#' DE_effect(fits1, effect="=~", effect2="~")
#' DE_effect(fits1, effect="~~")
#' DE_effect(fits1, effect=":=")
#'
#' DE_effect(fits2)
#' DE_effect(fits2, effect="=~")
#' DE_effect(fits2, effect="~~")
#' DE_effect(fits2, effect=":=")
#' DE_effect(fits2, effect=":=", effect2="~")
#'  DE_effect(fits)%>% interpretation_de()
#'  # same method
#'  DE_effect(fits)%>% report_sem_de()
#'
#' }
#'
#'

DE_effect = function(model_sem, effect = "~", effect2= NULL){

  res0 = parameterEstimates(model_sem, ci=F, stand=T)
  arrow = ifelse(effect=="~"," -> ",
                 ifelse(effect== "=~"," <- ",
                        ifelse (effect== "~~"," ~~ ",
                                ifelse(effect == ":=", ": ","") ) ))
  arrow2 = ifelse(effect2=="~"," -> ",
                  ifelse(effect2== "=~"," <- ",
                         ifelse (effect2=="~~"," ~~ ",
                                 ifelse(effect == ":=", ": ","") ) ))

  # Check if 'label' column exists
  if ("label" %in% colnames(res0)) {

    if(is.null(effect2)){
      res1 = res0 %>%
        filter(op %in% c(effect) ) %>%
        unite(Path, rhs, lhs, sep = arrow) %>%
        select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        select(-vars)

      res = res1

    }else{
      res1 = res0 %>%
        filter(op %in% c(effect) ) %>%
        unite(Path, rhs, lhs, sep = arrow) %>%
        select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        select(-vars)

      res2 = res0 %>%
        filter(op %in% c(effect2) ) %>%
        unite(Path, rhs, lhs, sep = arrow2) %>%
        select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        select(-vars)

      res = bind_rows(res1, res2 )
    }


  } else {
    if(is.null(effect2)){
      res1 = res0 %>%
        filter(op %in% c(effect) ) %>%
        unite(Path, rhs, lhs, sep = arrow) %>%
        select(Path,  est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        select(-vars)

      res = res1

    }else{
      res1 = res0 %>%
        filter(op %in% c(effect) ) %>%
        unite(Path, rhs, lhs, sep = arrow) %>%
        select(Path, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        select(-vars)

      res2 = res0 %>%
        filter(op %in% c(effect2) ) %>%
        unite(Path, rhs, lhs, sep = arrow2) %>%
        select(Path,  est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        select(-vars)

      res = bind_rows(res1, res2 )
    }
  }
  return(res)
}






#' SEM direct or indirect  effect result
#'
#' @param model_sem  lavaan object
#' @param effect  select op
#' @param effect2  select op 2nd option
#'
#' @return repot
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  #library(lavaan) # only needed once per session
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
#' '
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
#' '
#' fits1 <- sem(models1, data=PoliticalDemocracy)
#' fits2 <- sem(models2, data=PoliticalDemocracy)
#' summary(fits1, standardized=TRUE)
#' summary(fits2, standardized=TRUE)
#'
#' parameterEstimates(fits1, ci=F, stand=T)
#' parameterEstimates(fits2, ci=F, stand=T)
#' sem_effect(fits1)
#' sem_effect(fits1, effect="=~")
#' sem_effect(fits1, effect="=~", effect2="~")
#' sem_effect(fits1, effect="~~")
#' sem_effect(fits1, effect=":=")
#'
#' sem_effect(fits2)
#' sem_effect(fits2, effect="=~")
#' sem_effect(fits2, effect="~~")
#' sem_effect(fits2, effect=":=")
#' sem_effect(fits2, effect=":=", effect2="~")
#'  sem_effect(fits)%>% interpretation_de()
#'  # same method
#'  sem_effect(fits)%>% report_sem_de()
#'
#' }
#'
#'

sem_effect = function(model_sem, effect = "~", effect2= NULL, ci=FALSE){

  res0 = parameterEstimates(model_sem, ci=ci, stand=T)
  arrow = ifelse(effect=="~"," -> ",
                 ifelse(effect== "=~"," <- ",
                        ifelse (effect== "~~"," ~~ ",
                                ifelse(effect == ":=", ": ","_") ) ))
  arrow2 = ifelse(effect2=="~"," -> ",
                  ifelse(effect2== "=~"," <- ",
                         ifelse (effect2=="~~"," ~~ ",
                                 ifelse(effect == ":=", ": ","_") ) ))

  # Check if 'label' column exists
  if ("label" %in% colnames(res0)) {

    if(is.null(effect2)){
      res1 = res0 %>%
        filter(op %in% c(effect) ) %>%
        unite(Path, rhs, lhs, sep = arrow) %>%
        select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        select(-vars)

      res = res1

    }else{
      res1 = res0 %>%
        filter(op %in% c(effect) ) %>%
        unite(Path, rhs, lhs, sep = arrow) %>%
        select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        select(-vars)

      res2 = res0 %>%
        filter(op %in% c(effect2) ) %>%
        unite(Path, rhs, lhs, sep = arrow2) %>%
        select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        select(-vars)

      res = bind_rows(res1, res2 )
    }


  } else {
    if(is.null(effect2)){
      res1 = res0 %>%
        filter(op %in% c(effect) ) %>%
        unite(Path, rhs, lhs, sep = arrow) %>%
        select(Path,  est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        select(-vars)

      res = res1

    }else{
      res1 = res0 %>%
        filter(op %in% c(effect) ) %>%
        unite(Path, rhs, lhs, sep = arrow) %>%
        select(Path, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        select(-vars)

      res2 = res0 %>%
        filter(op %in% c(effect2) ) %>%
        unite(Path, rhs, lhs, sep = arrow2) %>%
        select(Path,  est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        select(-vars)

      res = bind_rows(res1, res2 )
    }
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
#' fits2 %>% sem_effect_ie()
#' }
#'
sem_effect_ci = function(model_sem,
                         effect1 ="DE", effect2="IE", label=FALSE, ci= TRUE ){

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

