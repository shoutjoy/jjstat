#' profile_plot2 Brargrahp
#'
#' @param data data
#' @param n_profiles profiles number
#' @param model_name "EEE","EEI", "VVV","VVI",
#' @param view each, pair
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' iris[,-5] %>%profile_plot2(n_profiles = 4, model_name = "EEE")
#'
#' }
#'
#'
profile_plot2 <- function(data, n_profiles=3,
                          model_name=NULL,
                          view="pair"

){
  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes)

  mean_data <-data %>%
    create_profiles_mclust2( n_profiles=n_profiles,
                             model_name = model_name)



  # Mclust(df, G = n_profiles, modelNames = model_name)

  gg  <- #data %>%
    # create_profiles_mclust2( n_profiles=n_profiles, model_name= model_name)
    mean_data %>%
    gather(key, val, -profile) %>%
    ggplot(aes(x = profile, y = val, fill = key, group = key)) +
    geom_col(position = "dodge") +
    # geom_line(aes(x = profile, y = val))+
    ylab("Z-score") +
    xlab("(b)") +
    theme(axis.text.x = element_text(size=14))+
    scale_fill_discrete("") +
    labs(title = "LPA모형추정 잠재Profile 평균의 표준화 점수")+
    theme_bw()

  #비표준화 데이터
  raw_data = Mclust(data, G=n_profiles, modelNames = model_name)
  # raw_data <- mean_data
  raw_mean_data <- raw_data$parameters$mean %>%   as_tibble()

  names(raw_mean_data) = str_c("profile",1:n_profiles)
  raw_mean_data$factor= colnames(data)
  raw_mean_data<-raw_mean_data %>% select(factor,1:ncol(raw_mean_data)-1)

  gg2<-raw_mean_data %>% pivot_longer(
    cols=-factor,
    names_to = "profile",
    values_to = "val" ) %>%
    ggplot(aes(x = profile, y = val, fill = factor, group = factor)) +
    geom_col(position = "dodge") +
    # geom_line(aes(x = profile, y = val))+
    ylab("Raw-score") +
    xlab("(a)") +
    theme(axis.text.x = element_text(size=14))+
    scale_fill_discrete("") +
    labs(title = " LPA모형추정 잠재Profile 평균의 원점수 ")+
    theme_bw()

  ggg<- gridExtra::grid.arrange(gg2, gg, ncol=2)


  if(view=="pair"){
    res=list(std=mean_data, est=raw_mean_data, graph=ggg)
  }else if(view=="each"){
    res=list(std=mean_data, est=raw_mean_data, gg2, gg)
  }
  res

}
