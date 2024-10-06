#' lpa_profile_plot data
#'
#' @param data data
#' @param n_profiles 3
#' @param model_name EEE,EEI,VVI,VVV
#' @param view pair, each
#' @param alpha bargraph alpha
#' @param gtype normal(x: variable), trans(x: profile), each(Separate views)
#' @param angle text.x angle
#' @param size.x text.x size
#' @param size.p Point size
#' @param ncol each ncol default = 2
#' @param fct_reorder NULL Changes the order as you type
#' @param show.legend line legend show
#' @param legend.text.size legend.text.size =16
#' @param legend_position llegend_position default "top"
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#' iris[,-5] %>%profile_plot(n_profiles = 4, model_name = "EEE")
#' #'
#'
#' ######################
#' edlcarename = edLca%>% rename(
#'   Attitude = ATT,
#'   FlowExperience = FLOW,
#'   EaseOfUse = PEOU,
#'   PerceivedUsefulness = PU,
#'   SystemUsage = SUX,
#'   UsageIntent = USE
#' )
#' # edlcarename = edLca%>% rename(
#' #       e_태도 = ATT,
#' #       b_학습몰입 = FLOW,
#' #       d_지각된편리성 = PEOU,
#' #       c_지각된유용성 = PU,
#' #       a_에듀테크 = SUX,
#' #       f_지속사용 = USE
#' #     )
#' edlcarename = edLca%>% rename(
#'   태도 = ATT,
#'   학습몰입 = FLOW,
#'   지각된편리성 = PEOU,
#'   지각된유용성 = PU,
#'   에듀테크 = SUX,
#'   지속사용 = USE
#' )
#' # lpa_profile_plot(edLca, n_profiles = 4, model_name = "EEE", gtype="normal")
#' # lpa_profile_plot(edLca, n_profiles = 4, model_name = "EEE", gtype="trans")
#' # lpa_profile_plot(edLca, n_profiles = 4, model_name = "EEE", gtype="each")
#' f_order=c("에듀테크","학습몰입","지각된유용성","지각된편리성","태도","지속사용")
#' #일반
#' lpa_profile_plot(edlcarename, n_profiles = 4, model_name = "EEE", gtype="normal")
#' #순서적용
#' lpa_profile_plot(edlcarename, n_profiles = 4, model_name = "EEE", gtype="normal",
#'                  fct_reorder= f_order )
#'
#' #일반
#' lpa_profile_plot(edlcarename, n_profiles = 4, model_name = "EEE", gtype="trans")
#' #순서적용
#' lpa_profile_plot(edlcarename, n_profiles = 4, model_name = "EEE", gtype="trans",
#'                  fct_reorder= f_order)
#'
#' # f_order=c("에듀테크","학습몰입","지각된유용성","지각된편리성","태도","지속사용")
#' lpa_profile_plot(edlcarename, n_profiles = 4, model_name = "EEE", gtype="each",
#'                  fct_reorder= f_order)
#'
#' lpa_profile_plot(edlcarename, n_profiles = 4,
#'                  model_name = "EEE", gtype="each",size.x=15, size.p=3,
#'                  fct_reorder= f_order)
#'
#'
#'
#' }
#'
#'

lpa_profile_plot <- function(data, n_profiles = 3,
                             model_name = "EEE",
                             view = "pair", alpha = 0.7,
                             gtype = "normal", angle = 90,
                             size.x = 13, size.p = 2, ncol = 2,
                             fct_reorder = NULL,  # fct_reorder 추가
                             show.legend = FALSE,
                             legend.text.size = 16,
                             legend_position="top",
                             seed = 123) {  # set.seed 추가
  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes)

  # set.seed 추가 (seed 값이 있으면 사용)
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # 프로파일 레이블 정의
  profile_labels <- setNames(paste0("profile", 1:n_profiles), 1:n_profiles)

  # 평균 데이터 생성
  mean_data <- data %>%
    lpa_create_profiles2(n_profiles = n_profiles, model_name = model_name, seed = seed)

  # 'profile' 열이 없을 가능성을 대비하여 새로운 열을 추가하고, profile을 문자형으로 변환
  mean_data <- mean_data %>%
    mutate(profile = as.character(row_number()))  # profile을 문자형으로 변환

  # 표준편차 계산 (profile을 문자형으로 변환)
  sd_data <- data %>%
    lpa_create_profiles2(n_profiles = n_profiles, model_name = model_name, seed = seed) %>%
    group_by(profile) %>%
    summarise(across(everything(), ~ sd(.x, na.rm = TRUE))) %>%
    pivot_longer(-profile, names_to = "key", values_to = "sd_val") %>%
    mutate(profile = as.character(profile))  # profile을 문자형으로 변환

  # 평균 데이터를 길게 변환하고 표준편차 데이터와 병합
  plot_data <- mean_data %>%
    gather(key, val, -profile) %>%
    left_join(sd_data, by = c("profile", "key"))

  # fct_reorder가 NULL이 아닌 경우, key 및 factor의 순서를 변경
  if (!is.null(fct_reorder)) {
    plot_data <- plot_data %>%
      mutate(key = factor(key, levels = fct_reorder))  # key 변수의 순서 재정렬
  }

  # 비표준화 데이터 미리 생성
  raw_data <- Mclust(data, G = n_profiles, modelNames = model_name)
  raw_mean_data <- raw_data$parameters$mean %>% as_tibble()

  names(raw_mean_data) <- str_c("profile", 1:n_profiles)
  raw_mean_data$factor <- colnames(data)
  raw_mean_data <- raw_mean_data %>% dplyr::select(factor, 1:ncol(raw_mean_data) - 1)

  # fct_reorder가 NULL이 아닌 경우 factor 변수의 순서 재정렬
  if (!is.null(fct_reorder)) {
    raw_mean_data <- raw_mean_data %>%
      mutate(factor = factor(factor, levels = fct_reorder))
  }

  # 프로파일의 빈도수 계산
  class_assignments <- raw_data$classification
  Class_freq <- table(class_assignments)

  if (gtype == "normal") {
    #표준화
    # 막대그래프 끝에 각 프로파일마다 다른 모양의 포인트 추가 및 표준편차 그리기
    gg <- plot_data %>%
      ggplot(aes(x = profile, y = val, fill = key, group = key)) +
      geom_col(position = "dodge", alpha = alpha + 0.1) +  # 막대 그래프
      geom_errorbar(aes(ymin = val - sd_val, ymax = val + sd_val),  # 표준편차 추가
                    position = position_dodge(width = 0.9),
                    width = 0.25) +
      geom_point(aes(shape = key), size = 2,
                 position = position_dodge(width = 0.9),
                 show.legend = FALSE) +  # 막대 끝에 포인트 추가
      scale_x_discrete(labels = profile_labels) +  # x축 레이블 변경
      ylab("Z-score") +
      xlab("(b) 표준화 자료(scale)") +
      scale_fill_discrete("") +
      scale_shape_manual(values = c(16, 17, 18, 19, 15, 20)) +  # shape를 다르게 설정
      labs(title = "LPA모형추정 잠재Profile 평균의 표준화 점수") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend.text = element_text(size = legend.text.size),
            legend.position = legend_position)
    #비표준화
    gg2 <- raw_mean_data %>%
      pivot_longer(
        cols = -factor,
        names_to = "profile",
        values_to = "val"
      ) %>%
      ggplot(aes(x = profile, y = val, fill = factor, group = factor)) +
      geom_col(position = "dodge", alpha = alpha + 0.1) +  # 비표준화 막대 그래프
      geom_point(aes(shape = factor), size = 2,
                 position = position_dodge(width = 0.9),
                 show.legend = FALSE) +  # 막대 끝에 포인트 추가
      scale_x_discrete(labels = profile_labels) +  # x축 레이블 변경
      ylab("Raw-score") +
      xlab("(a) 원데이터(Raw)") +
      scale_fill_discrete("") +
      scale_shape_manual(values = c(16, 17, 18, 19, 15, 20)) +  # shape를 다르게 설정
      labs(title = "LPA모형추정 잠재Profile 평균의 원점수") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend.text = element_text(size = legend.text.size),
            legend.position = legend_position)

    ggg <- gridExtra::grid.arrange(gg2, gg, ncol = 2)

  } else if (gtype == "trans") {
    # 평균값과 표준편차를 그리는 그래프 (factor에 따라 라인 및 포인트 추가)
    gg <- plot_data %>%
      ggplot(aes(x = key, y = val, fill = profile, group = profile)) +
      geom_col(position = "dodge", alpha = alpha) +  # 막대 그래프
      geom_errorbar(aes(ymin = val - sd_val, ymax = val + sd_val),  # 표준편차 추가
                    position = position_dodge(width = 0.9),
                    width = 0.25) +  # 표준편차 막대 추가
      geom_line(aes(group = profile, linetype = profile), size = 1) +  # profile에 따른 라인 추가
      geom_point(aes(group = profile, shape = profile),
                 color = "black", size = 3, show.legend = FALSE) +  # profile에 따른 포인트 추가
      ylab("Z-score") +
      xlab("(b) Normalized data") +
      scale_fill_discrete("") +
      labs(title = "LPA모형추정 잠재Profile 평균의 표준화 점수") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend_position=legend_position)

    gg2 <- raw_mean_data %>%
      pivot_longer(
        cols = -factor,
        names_to = "profile",
        values_to = "val"
      ) %>%
      ggplot(aes(x = factor, y = val, fill = profile, group = profile)) +
      geom_col(position = "dodge", alpha = alpha) +  # 비표준화 그래프
      geom_line(aes(group = profile, linetype = profile), size = 1) +  # profile에 따른 라인 추가
      geom_point(aes(group = profile, shape = profile), color = "black",
                 size = 3, show.legend = FALSE) +  # profile에 따른 포인트 추가
      ylab("Raw-score") +
      xlab("(a) Raw data") +
      scale_fill_discrete("") +
      labs(title = "LPA모형추정 잠재Profile 평균의 원점수") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend_position=legend_position)

    ggg <- gridExtra::grid.arrange(gg2, gg, ncol = 2)

  } else if (gtype == "each") {
    # 표준화된 데이터에 대한 그래프 (key에 따른 라인 및 막대 추가)
    gg <- plot_data %>%
      ggplot(aes(x = key, y = val, group = profile)) +
      geom_col(aes(fill = key), position = "dodge", show.legend = show.legend) +
      geom_errorbar(aes(ymin = val - sd_val, ymax = val + sd_val),  # 표준편차 추가
                    position = position_dodge(width = 0.9),
                    width = 0.25) +
      geom_line(aes(group = profile), size = 1, show.legend = FALSE) +  # profile에 따른 라인 추가
      geom_point(aes(group = key, shape = key), color = "black", size = size.p , show.legend = FALSE) +  # key에 따른 포인트 추가
      facet_wrap(~ profile, labeller = labeller(profile = profile_labels)) +  # 프로파일별로 그래프 분리 및 레이블 변경
      ylab("Z-score") +
      xlab("(a) propile Decomposition") +
      scale_fill_discrete("") +
      labs(title = "LPA모형추정 잠재Profile 평균의 표준화 점수") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            strip.text = element_text(size = size.x, face = "bold"))
    #분리된 도표
    gg2 <- raw_mean_data %>%
      pivot_longer(cols = -factor, names_to = "profile", values_to = "val") %>%
      ggplot(aes(x = factor, y = val, group = profile)) +
      geom_col(aes(fill = factor), position = "dodge", show.legend = show.legend) +
      geom_line(aes(group = profile), size = 1, show.legend = FALSE) +  # profile에 따른 라인 추가
      geom_point(aes(group = factor, shape = factor), color = "black", size = size.p, show.legend = FALSE) +  # factor에 따른 포인트 추가
      facet_wrap(~ profile, labeller = labeller(profile = profile_labels)) +  # 프로파일별로 그래프 분리 및 레이블 변경
      ylab("Raw-score") +
      xlab("(b)Profile Overlap") +
      scale_fill_discrete("") +
      labs(title = "LPA모형추정 잠재Profile 평균의 원점수") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            strip.text = element_text(size = size.x, face = "bold"))

    # 두 그래프를 결합
    ggg <- gridExtra::grid.arrange(gg2, gg, ncol = ncol)
  }

  # 결과 반환
  if (view == "pair") {
    res <- list(std = mean_data, est = raw_mean_data,
                graph = ggg,
                plot_data = data.frame(mean_data),
                Class_freq = Class_freq)  # Class 빈도수 포함
  } else if (view == "each") {
    x11()
    print(gg2)
    x11()
    print(gg)
    res <- list(std = mean_data, est = raw_mean_data,
                plot_data = mean_data,
                Class_freq = Class_freq)  # Class 빈도수 포함
  }
  res
}
# lpa_profile_plot <- function(data, n_profiles=3,
#                              model_name="EEE",
#                              view="pair"
#
# ){
#   library(tidyverse, warn.conflicts = FALSE)
#   library(mclust)
#   library(hrbrthemes)
#
#   mean_data <-data %>%
#     lpa_create_profiles2( n_profiles=n_profiles,
#                           model_name = model_name)
#
#
#
#   # Mclust(df, G = n_profiles, modelNames = model_name)
#
#   gg  <- #data %>%
#     # create_profiles_mclust2( n_profiles=n_profiles, model_name= model_name)
#     mean_data %>%
#     gather(key, val, -profile) %>%
#     ggplot(aes(x = profile, y = val, fill = key, group = key)) +
#     geom_col(position = "dodge") +
#     # geom_line(aes(x = profile, y = val))+
#     ylab("Z-score") +
#     xlab("(b)") +
#     scale_fill_discrete("") +
#     labs(title = "LPA모형추정 잠재Profile 평균의 표준화 점수")+
#     theme_bw()+
#     theme(axis.text = element_text(size=14))
#
#   #비표준화 데이터
#   raw_data = Mclust(data, G=n_profiles, modelNames = model_name)
#   # raw_data <- mean_data
#   raw_mean_data <- raw_data$parameters$mean %>%   as_tibble()
#
#   names(raw_mean_data) = str_c("profile",1:n_profiles)
#   raw_mean_data$factor= colnames(data)
#   raw_mean_data<-raw_mean_data %>% dplyr::select(factor,1:ncol(raw_mean_data)-1)
#
#   gg2<-raw_mean_data %>% pivot_longer(
#     cols=-factor,
#     names_to = "profile",
#     values_to = "val" ) %>%
#     ggplot(aes(x = profile, y = val, fill = factor, group = factor)) +
#     geom_col(position = "dodge") +
#     # geom_line(aes(x = profile, y = val))+
#     ylab("Raw-score") +
#     xlab("(a)") +
#     scale_fill_discrete("") +
#     labs(title = " LPA모형추정 잠재Profile 평균의 원점수 ")+
#     theme_bw()+
#     theme(axis.text = element_text(size=14))
#
#   ggg<- gridExtra::grid.arrange(gg2, gg, ncol=2)
#
#
#   if(view=="pair"){
#     res=list(std=mean_data, est=raw_mean_data, graph=ggg)
#   }else if(view=="each"){
#     res=list(std=mean_data, est=raw_mean_data, gg2, gg)
#   }
#   res
#
# }
