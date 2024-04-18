#' data.frame to table
#'
#' @param data data.frame
#' @param sel select column
#' @param type mat(matrix), df(data.frame)
#'
#' @return table
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(am = c(0, 1, 0, 1),
#'                    vs = c(0, 0, 1, 1),
#'                    Freq = c(12, 6, 7, 7))
#' data
#' #make  table
#' to_table(data)
#'
#' mtcars %>%select(am, vs) %>% table()
#' mtcars %>%select(am, vs) %>% table() %>%data.frame()
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table()
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table()%>%addmargins()
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table()%>%add_ratio()
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table()%>%add_ratio()%>%as.matrix()%>%addmargins()
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table()%>%add_ratio_df()
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table()%>%add_sum()
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table(sel="vs")
#'
#' mtcars %>%select(am, cyl) %>% table()
#' mtcars %>%select(am, cyl) %>% table() %>%data.frame()
#' mtcars %>%select(am, cyl) %>% table() %>%data.frame() %>%to_table()
#' mtcars %>%select(am, cyl) %>% table() %>%data.frame() %>%to_table()%>%add_ratio()
#' mtcars %>%select(am, cyl) %>% table() %>%data.frame() %>%to_table()%>%add_ratio_df()
#' mtcars %>%select(cyl, am) %>% table() %>%data.frame() %>%to_table() %>%add_sum()
#'
#' }
to_table <- function(data, sel = ncol(data)-1, type="mat") {
  # 데이터 재구조화
  transformed_data <- spread(data, key = all_of(sel), value = Freq)
  transformed_data <- transformed_data%>%
    dplyr::select(-1)
  # 행 이름 변경
  rownames(transformed_data) <- unique(data[[1]])
  colnames(transformed_data) <- unique(data[[2]])

  res_mat = transformed_data%>% as.matrix()

  switch(type,
         df = res,
         mat = res_mat)

}
