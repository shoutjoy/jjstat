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
library(dplyr)
library(tidyr)

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
        dplyr::filter(op %in% c(effect)) %>%
        tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
        dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue")

      res = res1

    } else {
      res1 = res0 %>%
        dplyr::filter(op %in% c(effect)) %>%
        tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
        dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue")

      res2 = res0 %>%
        dplyr::filter(op %in% c(effect2)) %>%
        tidyr::unite(Path, rhs, lhs, sep = arrow2) %>%
        dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue")

      res = dplyr::bind_rows(res1, res2)
    }

  } else {
    if(is.null(effect2)){
      res1 = res0 %>%
        dplyr::filter(op %in% c(effect)) %>%
        tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
        dplyr::select(Path, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue")
      res = res1

    } else {
      res1 = res0 %>%
        dplyr::filter(op %in% c(effect)) %>%
        tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
        dplyr::select(Path, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue") %>%
        dplyr::select(-vars)

      res2 = res0 %>%
        dplyr::filter(op %in% c(effect2)) %>%
        tidyr::unite(Path, rhs, lhs, sep = arrow2) %>%
        dplyr::select(Path, est, "std" = std.all, se, z, pvalue) %>%
        p_mark_sig("pvalue")

      res = dplyr::bind_rows(res1, res2)
    }
  }
  return(res)
}







#' SEM direct or indirect  effect result
#'
#' @param model_sem  lavaan object
#' @param effect  select op
#' @param effect2  select op 2nd option
#' @param ci FALSE
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
  library(dplyr)
  library(tidyr)


  res0 = lavaan::parameterEstimates(model_sem, ci= TRUE, stand=T)
  arrow = ifelse(effect=="~"," -> ",
                 ifelse(effect== "=~"," <- ",
                        ifelse (effect== "~~"," ~~ ",
                                ifelse(effect == ":=", ": ","_") ) ))
  arrow2 = ifelse(effect2=="~"," -> ",
                  ifelse(effect2== "=~"," <- ",
                         ifelse (effect2=="~~"," ~~ ",
                                 ifelse(effect == ":=", ": ","_") ) ))

  if(ci){

    # Check if 'label' column exists
    if ("label" %in% colnames(res0)) {

      if(is.null(effect2)){
        res1 = res0 %>%
          dplyr::filter(op == effect) %>%
          tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
          dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue,
                        ci.lower, ci.upper) %>%
          p_mark_sig("pvalue") %>% unite_ci(7,8,"[","]")

        res = res1

      }else{
        res1 = res0 %>%
          dplyr::filter(op == effect) %>%
          tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
          dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue,
                        ci.lower, ci.upper) %>%
          p_mark_sig("pvalue")%>% unite_ci(7,8,"[","]")

        res2 = res0 %>%
          dplyr::filter(op == effect2) %>%
          # filter(op %in% c(effect2) ) %>%
          tidyr::unite(Path, rhs, lhs, sep = arrow2) %>%
          dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue,
                        ci.lower, ci.upper) %>%
          p_mark_sig("pvalue")%>% unite_ci(7,8,"[","]")

        res = bind_rows(res1, res2 )
      }


    } else {
      if(is.null(effect2)){
        res1 = res0 %>%
          dplyr::filter(op == effect) %>%
          tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
          dplyr::select(Path,  est, "std" = std.all, se, z, pvalue,
                        ci.lower, ci.upper) %>%
          p_mark_sig("pvalue")%>% unite_ci(7,8,"[","]")

        res = res1

      }else{
        res1 = res0 %>%
          dplyr::filter(op == effect) %>%
          tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
          dplyr::select(Path, est, "std" = std.all, se, z, pvalue,
                        ci.lower, ci.upper) %>%
          p_mark_sig("pvalue")%>% unite_ci(7,8,"[","]")

        res2 = res0 %>%
          dplyr::filter(op == effect2) %>%
          tidyr::unite(Path, rhs, lhs, sep = arrow2) %>%
          dplyr::select(Path,  est, "std" = std.all, se, z, pvalue,
                        ci.lower, ci.upper) %>%
          p_mark_sig("pvalue")%>% unite_ci(7,8,"[","]")

        res = bind_rows(res1, res2 )
      }
    }



  }else{

    # Check if 'label' column exists
    if ("label" %in% colnames(res0)) {

      if(is.null(effect2)){
        res1 = res0 %>%
          dplyr::filter(op == effect) %>%
          tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
          dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
          p_mark_sig("pvalue")

        res = res1

      }else{
        res1 = res0 %>%
          dplyr::filter(op == effect) %>%
          tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
          dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
          p_mark_sig("pvalue")

        res2 = res0 %>%
          dplyr::filter(op == effect2) %>%
          # filter(op %in% c(effect2) ) %>%
          tidyr::unite(Path, rhs, lhs, sep = arrow2) %>%
          dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
          p_mark_sig("pvalue")

        res = bind_rows(res1, res2 )
      }


    } else {
      if(is.null(effect2)){
        res1 = res0 %>%
          dplyr::filter(op == effect) %>%
          tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
          dplyr::select(Path,  est, "std" = std.all, se, z, pvalue) %>%
          p_mark_sig("pvalue")

        res = res1

      }else{
        res1 = res0 %>%
          dplyr::filter(op == effect) %>%
          tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
          dplyr::select(Path, est, "std" = std.all, se, z, pvalue) %>%
          p_mark_sig("pvalue")

        res2 = res0 %>%
          dplyr::filter(op == effect2) %>%
          tidyr::unite(Path, rhs, lhs, sep = arrow2) %>%
          dplyr::select(Path,  est, "std" = std.all, se, z, pvalue) %>%
          p_mark_sig("pvalue")

        res = bind_rows(res1, res2 )
      }
    }

  }  #ci false
  return(res)
}
# sem_effect = function(model_sem, effect = "~", effect2= NULL, ci=FALSE){
#   library(dplyr)
#   library(tidyr)
#
#
#   res0 = lavaan::parameterEstimates(model_sem, ci= TRUE, stand=T)
#   arrow = ifelse(effect=="~"," -> ",
#                  ifelse(effect== "=~"," <- ",
#                         ifelse (effect== "~~"," ~~ ",
#                                 ifelse(effect == ":=", ": ","_") ) ))
#   arrow2 = ifelse(effect2=="~"," -> ",
#                   ifelse(effect2== "=~"," <- ",
#                          ifelse (effect2=="~~"," ~~ ",
#                                  ifelse(effect == ":=", ": ","_") ) ))
#
#   if(ci){
#
#     # Check if 'label' column exists
#     if ("label" %in% colnames(res0)) {
#
#       if(is.null(effect2)){
#         res1 = res0 %>%
#           dplyr::filter(op == effect) %>%
#           tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
#           dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue,
#                         ci.lower, ci.upper) %>%
#           p_mark_sig("pvalue")
#
#         res = res1
#
#       }else{
#         res1 = res0 %>%
#           dplyr::filter(op == effect) %>%
#           tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
#           dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue,
#                         ci.lower, ci.upper) %>%
#           p_mark_sig("pvalue")
#
#         res2 = res0 %>%
#           dplyr::filter(op == effect2) %>%
#           # filter(op %in% c(effect2) ) %>%
#           tidyr::unite(Path, rhs, lhs, sep = arrow2) %>%
#           dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue,
#                         ci.lower, ci.upper) %>%
#           p_mark_sig("pvalue")
#
#         res = bind_rows(res1, res2 )
#       }
#
#
#     } else {
#       if(is.null(effect2)){
#         res1 = res0 %>%
#           dplyr::filter(op == effect) %>%
#           tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
#           dplyr::select(Path,  est, "std" = std.all, se, z, pvalue,
#                         ci.lower, ci.upper) %>%
#           p_mark_sig("pvalue")
#
#         res = res1
#
#       }else{
#         res1 = res0 %>%
#           dplyr::filter(op == effect) %>%
#           tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
#           dplyr::select(Path, est, "std" = std.all, se, z, pvalue,
#                         ci.lower, ci.upper) %>%
#           p_mark_sig("pvalue")
#
#         res2 = res0 %>%
#           dplyr::filter(op == effect2) %>%
#           tidyr::unite(Path, rhs, lhs, sep = arrow2) %>%
#           dplyr::select(Path,  est, "std" = std.all, se, z, pvalue,
#                         ci.lower, ci.upper) %>%
#           p_mark_sig("pvalue")
#
#         res = bind_rows(res1, res2 )
#       }
#     }
#
#
#
#   }else{
#
#   # Check if 'label' column exists
#   if ("label" %in% colnames(res0)) {
#
#     if(is.null(effect2)){
#       res1 = res0 %>%
#         dplyr::filter(op == effect) %>%
#            tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
#         dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
#         p_mark_sig("pvalue")
#
#       res = res1
#
#     }else{
#       res1 = res0 %>%
#         dplyr::filter(op == effect) %>%
#        tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
#         dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
#         p_mark_sig("pvalue")
#
#       res2 = res0 %>%
#         dplyr::filter(op == effect2) %>%
#         # filter(op %in% c(effect2) ) %>%
#         tidyr::unite(Path, rhs, lhs, sep = arrow2) %>%
#         dplyr::select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
#         p_mark_sig("pvalue")
#
#       res = bind_rows(res1, res2 )
#     }
#
#
#   } else {
#     if(is.null(effect2)){
#       res1 = res0 %>%
#         dplyr::filter(op == effect) %>%
#         tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
#         dplyr::select(Path,  est, "std" = std.all, se, z, pvalue) %>%
#         p_mark_sig("pvalue")
#
#       res = res1
#
#     }else{
#       res1 = res0 %>%
#         dplyr::filter(op == effect) %>%
#         tidyr::unite(Path, rhs, lhs, sep = arrow) %>%
#         dplyr::select(Path, est, "std" = std.all, se, z, pvalue) %>%
#         p_mark_sig("pvalue")
#
#       res2 = res0 %>%
#         dplyr::filter(op == effect2) %>%
#         tidyr::unite(Path, rhs, lhs, sep = arrow2) %>%
#         dplyr::select(Path,  est, "std" = std.all, se, z, pvalue) %>%
#         p_mark_sig("pvalue")
#
#       res = bind_rows(res1, res2 )
#     }
#   }
#
#   }  #ci false
#   return(res)
# }





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
#' fits1 <- sem(models1, data=PoliticalDemocracy)
#'
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
#' fits2 <- sem(models2, data=PoliticalDemocracy)
#'
#' summary(fits1, standardized=TRUE)
#' summary(fits2, standardized=TRUE)
#' fits2 %>% sem_effect_ie()
#' }
#'
sem_effect_ci = function(model_sem,
                         effect1 ="DE", effect2="IE", label=FALSE, ci= TRUE ){
  library(dplyr)
  library(tidyr)
  res = lavaan::parameterEstimates(model_sem, ci=ci, stand=T)%>%
    dplyr::filter(op==":=" & str_detect(lhs, effect1) |str_detect(lhs, effect2))

  if(ci){
    res = res%>%select("Path"=lhs, rhs, est, "std"= std.all,
                       se, z, pvalue, ci.lower, ci.upper)%>%
      p_mark_sig("pvalue" )%>%select(-se, -pvalue)%>%
      Round(3)%>%
      tidyr::unite(CI_95, ci.lower, ci.upper, sep=", ")%>%
      tidyr::unite(z, z, sig, sep=" ")%>%
      dplyr::mutate(CI_95p = paste0("[", CI_95,"]"))%>%select(-CI_95)


  }else{
    res = res%>%
      dplyr::select("Path"=lhs, rhs, est, "std"= std.all, se, z, pvalue)%>%
      p_mark_sig("pvalue")
  }


  if(label){
    res = res
  }else{
    res = res%>% dplyr::select(-rhs)
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
#' @param digits digits=3
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
#' fits1 <- sem(models1, data=PoliticalDemocracy)
#'
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
#' fits2 <- sem(models2, data=PoliticalDemocracy)
#'
#' summary(fits1, standardized=TRUE)
#' summary(fits2, standardized=TRUE)
#' fits2 %>% sem_effect_ie()
#' }
#'
sem_effect_ind = function(model_sem,
                         effect1 ="DE", effect2="IE",
                         digits=3, label=FALSE, ci= TRUE ){
  library(dplyr)
  library(tidyr)
  res = lavaan::parameterEstimates(model_sem, ci=ci, stand=T)%>%
    dplyr::filter(op==":=" & str_detect(lhs, effect1) |str_detect(lhs, effect2))

  if(ci){
    res = res%>%select("Path"=lhs, rhs, est, "std"= std.all,
                       se, z, pvalue, ci.lower, ci.upper)%>%
      p_mark_sig("pvalue", digits = digits )%>%select(-se, -pvalue)%>%
      Round(digits)%>%
      tidyr::unite(CI_95, ci.lower, ci.upper, sep=", ")%>%
      tidyr::unite(z, z, sig, sep=" ")%>%
      dplyr::mutate(CI_95p = paste0("[", CI_95,"]"))%>%select(-CI_95)


  }else{
    res = res%>%
      dplyr::select("Path"=lhs, rhs, est, "std"= std.all, se, z, pvalue)%>%
      Round(digits) %>%
      p_mark_sig("pvalue", digits = digits )
  }


  if(label){
    res = res
  }else{
    res = res%>% dplyr::select(-rhs)
  }
  return(res)
}
