#' Frequency analysis table
#'
#' @param data data.frame
#' @param ... data.frame col names
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
#' @param na_treatment "무응답", # 결측값 대체값
#' @param size_axis_title default 12,
#' @param sort_bar TRUE(default)
#' @param type default  "res",  all, res=data, g=plot=graph
#'
#' @return Freqency table
#' @export
#'
#' @examples
#' \dontrun{
#' #사용방법
#' eduteck_s%>% Freq_table("Q9", prop=TRUE, add_row_sum = TRUE)
#'
#' eduteck_s$Q9 %>% Freq_table(prop=TRUE, add_row_sum = TRUE, na.rm= TRUE)
#' eduteck_s$Q9 %>% Freq_table(prop=TRUE, add_row_sum = TRUE, na.rm=FALSE)
#'
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
                       na.rm = TRUE,
                       na_treatment = "무응답",
                       remove_rows = NULL,
                       add_row_sum = FALSE,
                       xlab = "Category",
                       flip = FALSE,
                       size_axis_title = 12,
                       sort_bar = TRUE,
                       type = "res") {

  # 행 복원: 가중치를 사용하여 데이터 확장
  if (!is.null(wt)) {
    if (!(wt %in% colnames(data))) {
      stop("가중치로 사용할 열이 데이터에 없습니다: ", wt)
    }
    data <- unCount(data, sel = wt)
  }

  # 데이터가 벡터인 경우 데이터프레임으로 변환
  if (!is.data.frame(data)) {
    data <- data.frame(term = data, stringsAsFactors = FALSE)
  } else {
    select_vars <- c(...)
    if (length(select_vars) > 0) {
      colname_to_use <- select_vars[1] # 첫 번째 선택된 열 사용
      if (!(colname_to_use %in% colnames(data))) {
        stop("선택한 열이 데이터에 없습니다: ", colname_to_use)
      }
      data <- data %>% dplyr::rename(term = !!rlang::sym(colname_to_use)) # 열 이름을 'term'으로 변경
    } else {
      stop("열 이름을 선택하세요. 예: Freq_table(data, 'colname')")
    }
  }

  # `term` 열 확인
  if (!("term" %in% colnames(data))) {
    stop("데이터에 'term' 열이 존재하지 않습니다. 열 이름을 확인하세요.")
  }

  # NA 및 빈 문자열 처리
  if (na.rm) {
    data <- data %>%
      dplyr::mutate(term = ifelse(term == "" | is.na(term), NA, term)) %>%
      tidyr::drop_na(term) # 결측값 제거
  } else {
    data <- data %>%
      dplyr::mutate(term = ifelse(term == "" | is.na(term), na_treatment, term))
  }

  # 빈도표 생성
  res <- data %>%
    dplyr::count(term) %>%
    dplyr::rename(Freq = n) %>%
    dplyr::mutate("prop(%)" = Freq / sum(Freq) * 100)

  # 선택된 행 제거
  if (!is.null(remove_rows)) {
    res <- res[-remove_rows, ]
    total_freq <- sum(res$Freq)
    res <- res %>%
      dplyr::mutate("prop(%)" = Freq / total_freq * 100)
  }

  # 그래프 데이터 저장 (add_row_sum 이전 상태)
  graph_data <- res

  # 합계 행 추가
  if (add_row_sum) {
    total_freq <- sum(res$Freq)
    res <- res %>%
      tibble::add_row(term = "합계", Freq = total_freq, `prop(%)` = 100)
  }

  # 그래프용 데이터 생성
  if (!prop) {
    graph_data <- graph_data %>% dplyr::select(-`prop(%)`)
    graph_data <- graph_data %>%
      dplyr::mutate(LABEL = paste0("n=", Freq))
  } else {
    graph_data <- graph_data %>%
      dplyr::mutate(LABEL = paste0(Freq, "(", round(`prop(%)`, 2), " %)"))
  }

  # 그래프 데이터에서 "합계" 행 제외
  graph_data <- graph_data %>%
    dplyr::filter(term != "합계") %>%
    dplyr::mutate(x_var_clean = term)

  # 막대 정렬
  if (sort_bar) {
    graph_data <- graph_data %>%
      dplyr::mutate(x_var_clean = forcats::fct_reorder(x_var_clean, Freq))
  }

  # 그래프 생성
  g <- ggplot2::ggplot(graph_data, ggplot2::aes(x = x_var_clean, y = Freq)) +
    ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = x_var_clean)) +
    ggplot2::ylim(0, max(graph_data$Freq) + yadd) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = xlab, y = "Frequency") +
    ggplot2::geom_text(ggplot2::aes(label = LABEL),
                       vjust = ifelse(flip, 0.5, vjust),
                       hjust = ifelse(flip, hjust, 0.5),
                       size = size_text) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle,
                                                       size = size_axis, face = "bold"),
                   axis.title = ggplot2::element_text(size = size_axis_title),
                   legend.position = legend.position)

  # 플립 옵션
  if (flip) {
    g <- g + ggplot2::coord_flip()
  }

  # 그래프 출력
  if (plot) {
    print(g)
  }

  # 결과와 그래프 반환
  all <- list(result = res %>% tibble::tibble(),
              graph_data = graph_data %>% tibble::tibble(), g = g)

  switch(type,
         all = all,
         res = res %>% tibble::tibble(),
         data = res %>% tibble::tibble(),
         graph_data = graph_data %>% tibble::tibble(),
         g = g,
         plot = g,
         graph = g)
}
