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
                       na.rm = TRUE, # 결측값 처리 여부
                       na_treatment = "무응답", # 결측값 대체값
                       xlab = "Category",
                       flip = FALSE,
                       size_axis_title = 12,
                       sort_bar = TRUE,
                       type = "res") {

  # 데이터가 벡터 또는 리스트일 경우 데이터프레임으로 변환
  if (is.vector(data) || is.factor(data)) {
    data <- data.frame(term = as.character(data))  # 벡터나 리스트를 데이터프레임으로 변환
  } else if (is.data.frame(data)) {
    # 데이터프레임일 경우 첫 번째 열을 "term"으로 설정
    if (!"term" %in% colnames(data)) {
      colnames(data)[1] <- "term"
    }
  } else {
    stop("Invalid data type. Input must be a vector, factor, or data.frame.")
  }

  # NA 및 빈 문자열 처리 (옵션으로 제어)
  if (na.rm) {
    # 결측값 제거
    data <- data %>% filter(term != "" & !is.na(term))
  } else {
    # 결측값을 지정된 값으로 대체
    data <- data %>% mutate(term = ifelse(term == "" | is.na(term), na_treatment, term))
  }

  # 테이블 생성
  res <- as.data.frame(table(data$term))
  colnames(res) <- c("term", "Freq")

  # 비율 계산은 prop = TRUE인 경우에만 실행
  if (prop) {
    total_freq <- sum(res$Freq)
    res <- res %>%
      mutate("prop(%)" = Freq / total_freq * 100)
  }

  # 선택된 행 제거
  if (!is.null(remove_rows)) {
    # 유효한 row 번호 확인
    if (all(remove_rows > 0 & remove_rows <= nrow(res))) {
      res <- res[-remove_rows, ]
      if (prop) {  # 비율을 계산하는 경우에만 갱신
        total_freq <- sum(res$Freq)
        res <- res %>%
          mutate("prop(%)" = Freq / total_freq * 100)
      }
    } else {
      stop("Invalid row numbers provided in 'remove_rows'. Check input.")
    }
  }

  # term 열을 문자형으로 변환
  res <- res %>% mutate(term = as.character(term))

  # 합계 행 추가
  if (add_row_sum) {
    if (prop) {
      res <- res %>%
        add_row(term = "합계", Freq = sum(res$Freq), `prop(%)` = 100)
    } else {
      res <- res %>%
        add_row(term = "합계", Freq = sum(res$Freq))
    }
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
    geom_text(aes(label = paste0(Freq, if (prop) paste0(" (", round(`prop(%)`, 2), "%)") else "")),
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


