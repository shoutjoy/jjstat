#' lpa_BIC_plot
#'
#' @param data data
#' @param n_profiles_range 1:9
#'
#' @return pot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' select(iris, -Species)%>%lpa_BIC_plot()
#' }
#'
#'
lpa_BIC_plot <- function(data, n_profiles_range = 1:9){
  library(forcats)

  #long data trnasformation
  to_plot <- lpa_explore_modelfit(data, n_profiles_range = n_profiles_range)%>%
    gather(`Covariance matrix structure`, val, -n_profiles) %>%
    mutate(`Covariance matrix structure` = as.factor(`Covariance matrix structure`), val = abs(val))

  # C- variable arrange
  to_plot$`Covariance matrix structure` <- fct_relevel(
    to_plot$`Covariance matrix structure`,
    "Constrained variance, fixed covariance",
    "Freed variance, fixed covariance",
    "Constrained variance, constrained covariance",
    "Freed variance, freed covariance")

  #plotting
  gg <-ggplot(to_plot, aes(x = n_profiles, y = val,
                           color = `Covariance matrix structure`,
                           group = `Covariance matrix structure`)) +
    geom_line(linewidth = 1) +
    geom_point(size=2) +
    ylab("BIC (smaller value is better)") +
    guides( linewidth="none", alpha="none")+
    theme_bw()

  res= list(to_plot, gg)
  res

}


#' lpa_BIC_plot2
#'
#' @param data data
#' @param n_profiles_range 1:9
#' @param modelNames NULL
#' @param v 0
#' @param point_size 1.5
#' @param v_linewidth  0.6, #profile cut
#' @param l_linewidth 1, #line width
#' @param title "적절한 개수의 profiles 판정 "
#' @param flip TRUE
#' @param line FALSE
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' select(iris, -Species)%>%lpa_BIC_plot2()
#' }
#'
lpa_BIC_plot2 <- function(data,
                          n_profiles_range = 1:9, #범위
                          modelNames=NULL,  #모델선정
                          v= 0, #모델개수한정라인
                          point_size=1.5, #데이터구분점
                          v_linewidth=0.6, #profile cut
                          l_linewidth=1, #line width
                          title="적절한 개수의 profiles 판정 ",
                          flip=TRUE,
                          line=FALSE
){
  library(forcats)

  #long data trnasformation
  if(flip== TRUE){
    to_plot <- lpa_explore_modelfit(data, n_profiles_range = n_profiles_range,
                                    modelNames = modelNames)%>%
      tidyr::pivot_longer(names_to = "Covariance_matrix_str",
                          values_to =  "val", cols = - n_profiles) %>%
      dplyr::mutate("Covariance.matrix.str" = as.factor(Covariance_matrix_str),
                    "val" = val) %>% select(1,4, 3) %>% as.data.frame()

  }else if(flip==FALSE){
    to_plot <- lpa_explore_modelfit(data, n_profiles_range = n_profiles_range,
                                    modelNames = modelNames)%>%
      tidyr::pivot_longer(names_to = "Covariance_matrix_str",
                          values_to =  "val", cols = - n_profiles) %>%
      dplyr::mutate("Covariance.matrix.str" = as.factor(Covariance_matrix_str),
                    "val" = abs(val)) %>% select(1,4, 3) %>% as.data.frame()

  }else if(filp=="none"){
    stop("end revise ")
  }

  #plotting

  if(line==TRUE){
    gg <-ggplot(to_plot, aes(x = n_profiles, y = val

    )) +
      geom_line(aes(group = Covariance.matrix.str,
                    color = Covariance.matrix.str,
                    linetype= Covariance.matrix.str),linewidth = l_linewidth) +
      geom_point(aes(group = Covariance.matrix.str), size=point_size) +# 데이터사이즈
      ylab("BIC (smaller value is better)") +
      xlab("Number of Components(profiles) ")+

      guides( linewidth="none", alpha="none")+
      geom_vline(xintercept = v, colour="red",
                 linetype = "longdash", linewidth=v_linewidth, # cutline
                 alpha=.4)+
      labs(title=title)+
      theme_bw()
  }else if(line==FALSE){
    gg <-ggplot(to_plot, aes(x = n_profiles, y = val

    )) +
      geom_line(aes(group = Covariance.matrix.str,
                    color = Covariance.matrix.str #,
                    # linetype= Covariance.matrix.str
      ),linewidth = l_linewidth) +
      geom_point(aes(group = Covariance.matrix.str),
                 size=point_size) +# 데이터사이즈
      ylab("BIC (smaller value is better)") +
      xlab("Number of Components(profiles) ")+
      guides( linewidth="none", alpha="none")+
      geom_vline(xintercept = v, colour="red",
                 linetype = "longdash", linewidth=v_linewidth, # cutline
                 alpha=.4)+
      labs(title=title)+
      theme_bw()
  }

  res= list(to_plot, gg)
  res
}



#' lpa_BIC_plot3, all plot
#'
#' @param data data
#' @param n_profiles 1:9
#' @param modelNames  NULL, if you want input  c("EEV","EEI","EEE","EII"),
#' @param v 0, abline v = 0
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' select(iris, -Species)%>%lpa_BIC_plot3()
#' }
#'
lpa_BIC_plot3 <-  function(data,
                           n_profiles=1:9,
                           modelNames =NULL,
                           v=0
){
  library(mclust)
  bic_data <- mclustBIC(data, G = n_profiles, modelNames =modelNames)

  gg<- plot(bic_data )
  abline(v=v, lty=2, col="red")
  grid()
  res=list(bic_data, gg)
  res

}

