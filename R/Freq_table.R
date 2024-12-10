#' Frequency analysis table
#'
#' @param data data.frame
#' @param ... vars If you input varabale names "var1", "var2" ,...
#' @param prop proportion value, default TRUE
#' @param plot plot defualt false
#' @param angle  angle = 0
#' @param size_text default 3, bargraph text
#' @param size_axis default 12, axis test
#' @param vjust default -0.5
#' @param hjust default -0.5
#' @param yadd default 0.8
#' @param legend.position legend.position="" none. top, bottom, left, right
#' @param reorder reorder Sorting Graph Arrays
#' @param wt weight
#' @param remove_rows Enter the number of rows to hide and they will be removed
#' @param add_row_sum default FALSE, Sum excluding removed rows
#' @param xlab xalb = "Category",
#' @param flip FALSE
#' @param na.rm TRUE
#' @param size_axis_title default 12,
#' @param sort_bar TRUE(default)
#' @param type default  "res",  all, res=data, g=plot=graph
#'
#' @return Freqency table
#' @export
#'
#' @examples
#' \dontrun{
#' Mtcars = mtcars
#' Mtcars$am = factor(Mtcars$am, levels=c(0,1), labels= c("automatic","manual" ))
#' Mtcars$vs  = factor(Mtcars$vs, levels=c(0,1), labels= c("V-shaped","straight" ))
#' Mtcars$cyl  = factor(Mtcars$cyl, levels=c(4,6,8), labels= c("cyl-4","cyl-6","cyl-8"))
#'
#' #vector variables
#' Freq_table(c("a","b","c","a","b","a","c","c","c"))
#' Freq_table(c("a","b","c","a","b","a","c","c","c"), prop=TRUE)
#' Freq_table(c("a","b","c","a","b","a","c","c","c"), plot=T)
#'
#'
#' Mtcars %>% Freq_table("am")
#' Mtcars %>% Freq_table("am", prop=F)
#' Mtcars %>% Freq_table("vs", plot=FALSE)
#'
#' Mtcars %>% Freq_table("vs")
#' Mtcars %>% Freq_table("vs","am")
#'
#' Mtcars %>% Freq_table("vs","am","cyl", type="all")
#'
#' #graph
#' Mtcars %>% Freq_table("vs","am","cyl", type="g")
#'
#'##only graph
#' Mtcars %>% Freq_table("vs","am","cyl", angle=90, type="g")
#' Mtcars %>% Freq_table("vs","am","cyl", angle=90, reorder = TRUE, type="g")
#'
#' Mtcars %>% Freq_table("vs","am","cyl", angle=90, type="g")+ylim(0,14)
#' Mtcars %>% Freq_table("vs","am","cyl", angle=90, reorder = TRUE, type="g")+ylim(0,14)
#'
#' Mtcars %>% Freq_table("vs","am") %>% arrange(am)
#' Mtcars %>% Freq_table("vs","am") %>% arrange(vs)
#' Mtcars %>% Freq_table("vs","am") %>% arrange(Freq)
#'
#' ##when the data has already been aggregated once
#' df_sample <- tribble(
#'   ~name,    ~gender,   ~ sum,
#'   "Max",    "male",       10,
#'   "Sandra", "female",      1,
#'   "Susan",  "female",      4)
#' df_sample
#'
#' #weight frequency
#' df_sample %>% unCount(sel = "sum") %>% Freq_table("gender")
#'
#' # this function
#' df_sample %>% Freq_table("gender", wt="sum")
#' #'
#' # 데이터 생성
#' df <- data.frame(
#'   term = c("", "10-20분", "20-30분", "30분 이상", "5-10분", "5분 이내"),
#'   Freq = c(728, 222, 77, 120, 319, 135)
#' )
#'
#' # 기본 그래프
#' Freq_table(df, term, prop = TRUE, remove_rows = 1, add_row_sum = FALSE, plot = TRUE)
#'
#' # xlab과 flip 옵션 사용
#' Freq_table(df, term, prop = TRUE, remove_rows = 1, add_row_sum = TRUE, xlab = "응답 시간", flip = TRUE, plot = TRUE)
#' #'
#'
#' #빈도분석 및 그래프
#' Freq_table(eduteck2$Q9, prop=TRUE, remove_rows=1, add_row_sum=TRUE, vjust=-0.5,
#'            plot=T, size_axis = 14, flip=FALSE, yadd=130, xlab="Eduteck Time", size_text = 6) #'
#' }
#'
#'
Freq_table <- function(data, ...,
                       prop = FALSE,
                       plot = FALSE,
                       angle = 0,
                       size_text = 4,
                       size_axis = 10,
                       vjust = -0.05,
                       hjust = 0.5,
                       yadd = 100,
                       legend.position = "",
                       reorder = FALSE,
                       wt = NULL,
                       remove_rows = NULL,
                       add_row_sum = FALSE,
                       na.rm = TRUE, # 결측값 처리 여부 추가
                       xlab = "Category",
                       flip = FALSE,
                       size_axis_title = 12,
                       sort_bar = TRUE,
                       type = "res") {

  # 데이터가 벡터 또는 리스트일 경우 데이터프레임으로 변환
  if (!is.data.frame(data)) {
    data <- data.frame(term = unlist(data))  # 벡터나 리스트를 데이터프레임으로 변환
  }

  # 열 이름 확인 및 설정
  if (!"term" %in% colnames(data)) {
    colnames(data)[1] <- "term"
  }

  # NA 및 빈 문자열 처리 (옵션으로 제어)
  if (na.rm) {
    data <- data %>% mutate(term = ifelse(term == "" | is.na(term), "무응답", term))
  }

  # 테이블 생성 및 비율 계산
  res <- data %>%
    count(term, name = "Freq") %>% # "Freq"로 열 이름 지정
    mutate("prop(%)" = Freq / sum(Freq) * 100)

  # 선택된 행 제거
  if (!is.null(remove_rows)) {
    res <- res[-remove_rows, ]
    total_freq <- sum(res$Freq)
    res <- res %>%
      mutate("prop(%)" = Freq / total_freq * 100)
  }

  # 합계 행 추가
  if (add_row_sum) {
    total_freq <- sum(res$Freq)
    res <- res %>%
      add_row(term = "합계", Freq = total_freq, `prop(%)` = 100)
  }

  # 그래프 데이터 저장
  graph_data <- res

  if (sort_bar) {
    graph_data <- graph_data %>%
      mutate(term = fct_reorder(term, Freq))
  }

  # 그래프 생성
  g <- graph_data %>%
    filter(term != "합계") %>%
    ggplot(aes(x = term, y = Freq, fill = term)) +
    geom_bar(stat = "identity") +
    ylim(0, max(graph_data$Freq) + yadd) +
    geom_text(aes(label = paste0(Freq, " (", round(`prop(%)`, 2), "%)")),
              vjust = vjust, size = size_text) +
    theme_bw() +
    labs(x = xlab, y = "Frequency") +
    theme(axis.text.x = element_text(angle = angle, size = size_axis),
          axis.title = element_text(size = size_axis_title),
          legend.position = legend.position)

  if (flip) {
    g <- g + coord_flip()
  }

  # 그래프 출력
  if (plot) {
    print(g)
  }

  # 결과 반환
  all <- list(result = res, graph = g)

  switch(type,
         all = all,
         res = res,
         graph = g)
}


# Freq_table <- function(data, ...,
#                        prop = FALSE,
#                        plot = FALSE,
#                        angle = 0,
#                        size_text = 4,
#                        size_axis = 10,
#                        vjust = -0.05,
#                        hjust = 0.5,
#                        yadd = 100,
#                        legend.position = "",
#                        reorder = FALSE,
#                        wt = NULL,
#                        remove_rows = NULL,
#                        add_row_sum = FALSE,
#                        xlab = "Category",
#                        flip = FALSE,
#                        size_axis_title = 12,
#                        sort_bar = TRUE,
#                        type = "res") {
#
#   # 행 복원
#   if (!is.null(wt)) {
#     data <- unCount(data, sel = wt)
#   }
#
#   # 데이터 프레임 확인 및 열 이름 설정
#   if (is.data.frame(data)) {
#     select_vars <- c(...)
#     res <- data[, c(...)] %>%
#       table() %>%
#       as.data.frame() %>%
#       mutate("prop(%)" = Freq / sum(Freq) * 100)
#     colnames(res)[1] <- "term" # 첫 번째 열 이름을 "term"으로 설정
#   } else {
#     data <- data.frame(term = data) # 벡터 데이터를 데이터프레임으로 변환
#     res <- data %>%
#       table() %>%
#       as.data.frame() %>%
#       mutate("prop(%)" = Freq / sum(Freq) * 100)
#     colnames(res) <- c("term", "Freq", "prop(%)")
#   }
#
#   # 선택된 행 제거
#   if (!is.null(remove_rows)) {
#     res <- res[-remove_rows, ]
#     total_freq <- sum(res$Freq)
#     res <- res %>%
#       mutate("prop(%)" = Freq / total_freq * 100)
#   }
#
#   # 그래프 데이터 저장 (add_row_sum 이전 상태)
#   graph_data <- res
#
#   # 합계 행 추가
#   if (add_row_sum) {
#     total_freq <- sum(res$Freq)
#     res <- res %>%
#       add_row(term = "합계", Freq = total_freq, `prop(%)` = 100)
#   }
#
#   # 그래프용 데이터 생성
#   if (!prop) {
#     graph_data <- graph_data %>% dplyr::select(-`prop(%)`)
#     graph_data <- graph_data %>%
#       mutate(LABEL = paste0("n=", Freq))
#   } else {
#     graph_data <- graph_data %>%
#       mutate(LABEL = paste0(Freq, "(", round(`prop(%)`, 2), " %)"))
#   }
#
#   # 그래프 데이터에서 "합계" 행 제외
#   graph_data <- graph_data %>%
#     filter(term != "합계") %>%
#     unite(x_var, where(is.factor), where(is.character), remove = FALSE)
#
#   # 축 레이블 정리: 범주 이름만 포함
#   graph_data <- graph_data %>%
#     mutate(x_var_clean = term) # 범주 이름 정확히 유지
#
#   # 막대 정렬
#   if (sort_bar) {
#     graph_data <- graph_data %>% mutate(x_var_clean = fct_reorder(x_var_clean, Freq))
#   }
#
#   # 그래프 생성
#   g <- graph_data %>%
#     ggplot(aes(x = x_var_clean, y = Freq)) +
#     geom_bar(stat = "identity", aes(fill = x_var_clean)) +
#     ylim(0, max(graph_data$Freq) + yadd) +
#     theme_bw() +
#     labs(x = xlab, y = "Frequency") +
#     geom_text(aes(label = LABEL),
#               vjust = ifelse(flip, 0.5, vjust),
#               hjust = ifelse(flip, hjust, 0.5),
#               size = size_text) +
#     theme(axis.text.x = element_text(angle = angle,
#                                      size = size_axis, face = "bold"),
#           axis.title = element_text(size = size_axis_title),
#           legend.position = legend.position)
#
#   # 플립 옵션
#   if (flip) {
#     g <- g + coord_flip()
#   }
#
#   # 그래프 출력
#   if (plot) {
#     print(g)
#   }
#
#   # 결과와 그래프 반환
#   all <- list(result = res %>% tibble::tibble(),
#               graph_data = graph_data %>% tibble::tibble(), g = g)
#
#   switch(type,
#          all = all,
#          res = res %>% tibble::tibble(),
#          data = res %>% tibble::tibble(),
#          graph_data = graph_data %>% tibble::tibble(),
#          g = g,
#          plot = g,
#          graph = g)
# }


# Freq_table <- function(data, ...,
#                        prop = FALSE,
#                        plot = FALSE,
#                        angle = 0,
#                        size_text = 4,
#                        size_axis = 10,
#                        vjust= -0.5,
#                        yadd = 1,
#                        legend.position = "",
#                        reorder = FALSE,
#                        wt = NULL,
#                        type="res") {
#
#   #when the data has already been aggregated once
#   if(is.null(wt)){
#     data <- data
#   }else{
#     data <- unCount(data, sel = wt) #add real count rows
#   }
#
#   # Check if data is a data frame
#   if (is.data.frame(data)) {
#     select_vars <- c(...)
#     res <- data[, c(...)] %>%
#       table() %>%
#       as.data.frame() %>%
#       mutate("prop(%)" = Freq / sum(Freq) * 100)
#   } else {
#     # If data is not a data frame, convert it to one
#     data <- data.frame(data)
#     select_vars <- "term"
#     res <- data %>%
#       table() %>%
#       as.data.frame() %>%
#       mutate("prop(%)" = Freq / sum(Freq) * 100)
#   }
#
#   if (length(select_vars) == 1) {
#     colnames(res) <- c(select_vars, "Freq", "prop(%)" )
#   }
#   if (!prop) {
#     res <- res %>% dplyr::select(-`prop(%)`)
#     LABEL <- paste("n=", res$Freq)
#   } else {
#     LABEL <- paste0(res$Freq, " (", round(res$`prop(%)`, 2), " %)")
#   }
#
#
#
#
#
#   #plot-----
#   # Select and unite factor and character variables
#   Res <- res %>%
#     unite(x_var, where(is.factor), where(is.character))
#
#   # Plot x-variable reorder
#   if (reorder) {
#     Res <- Res %>% mutate(x_var = fct_reorder(x_var, desc(Freq)))
#   }
#
#   # Make graph
#   g <- Res %>%
#     ggplot(aes(x = x_var, y = Freq)) +
#     geom_bar(stat = "identity", aes(fill = x_var)) +
#     ylim(0, max(Res$Freq)+ yadd)+
#   theme_bw() +
#     geom_text(aes(label = LABEL), vjust = vjust, size = size_text) +
#     theme(axis.text.x = element_text(angle = angle,
#                                      size = size_axis, face = "bold"),
#           legend.position = legend.position)
#
#
#   # Plot output setting
#   if (plot) {
#     # Output graph
#     print(g)
#   }
#
#   all = list(result = res %>%tibble::tibble(), g = g)
#
#
#   switch(type,
#          all = all,
#          res = res%>%tibble::tibble(),
#          data = res%>%tibble::tibble(),
#          g = g,
#          plot = g,
#          graph = g
#   )
# }
