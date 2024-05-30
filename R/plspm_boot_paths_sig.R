#' boot res sig table
#'
#' @param plsres plspm result
#' @param type df, vec, vector
#'
#' @return data, vector
#' @export
#'

plspm_boot_paths_sig <- function(plsres, type= "df") {

  if(length(plsres)==13){
    plsdf = plsres$boot$paths
  }else if(length(plsres)==5 & is.data.frame(plsres)){
    plsdf = plsres
  }else if(length(plsres)==5 & is.list(plsres)){
    plsdf = plsres$paths
  }

  res =  plsdf %>%
    row2col("paths") %>%
    add_t_sig(3,4, unite=TRUE,col=5, ns="")%>%
    round2(3) %>%
    unite_ci(col1 = 'perc.025', col2 = 'perc.975')
  vec =  plsdf %>%
    row2col("paths") %>%
    add_t_sig(3,4, unite=FALSE,col=5, ns="")%>%
    round2(3) %>%
    unite_ci(col1 = 'perc.025', col2 = 'perc.975')%>%
    Unite(2,5)%>%
    pull(Original)

  switch(type, df= res, res= res, vec= vec, vector= vec)
}


#' boot res sig table
#'
#' @param plsres plspm result
#' @param type df, vec, vector
#'
#' @return data, vector
#' @export
#'

plspm_boot_paths_sig_vec <- function(plsres, type= "vec") {

  if(length(plsres)==13){
    plsdf = plsres$boot$paths
  }else if(length(plsres)==5 & is.data.frame(plsres)){
    plsdf = plsres
  }else if(length(plsres)==5 & is.list(plsres)){
    plsdf = plsres$paths
  }

  res =  plsdf %>%
    row2col("paths") %>%
    add_t_sig(3,4, unite=TRUE,col=5, ns="")%>%
    round2(3) %>%
    unite_ci(col1 = 'perc.025', col2 = 'perc.975')
  vec =  plsdf %>%
    row2col("paths") %>%
    add_t_sig(3,4, unite=FALSE,col=5, ns="")%>%
    round2(3) %>%
    unite_ci(col1 = 'perc.025', col2 = 'perc.975')%>%
    Unite(2,5)%>%
    pull(Original)

  switch(type, df= res, res= res, vec= vec, vector= vec)
}
