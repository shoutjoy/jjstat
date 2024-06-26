#' data.frame to table
#'
#' @param data data.frame
#' @param sel select column (names_to)
#' @param value values_to
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
#'
#' mtcars %>%select(am, vs) %>% table() %>%data.frame()
#'
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table()
#'
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table()%>%addmargins()
#'
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table()%>%add_ratio()
#'
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%
#' to_table()%>%add_ratio()%>%as.matrix()%>%addmargins()
#'
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table()%>%add_ratio_df()
#'
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table()%>%add_sum()
#'
#' mtcars %>%select(am, vs) %>% table() %>%data.frame() %>%to_table(sel="vs")
#'
#' mtcars %>%select(am, cyl) %>% table()
#'
#' mtcars %>%select(am, cyl) %>% table() %>%data.frame()
#'
#' mtcars %>%select(am, cyl) %>% table() %>%data.frame() %>%to_table()
#'
#' mtcars %>%select(am, cyl) %>% table() %>%data.frame() %>%to_table()%>%add_ratio()
#'
#' mtcars %>%select(am, cyl) %>% table() %>%data.frame() %>%to_table()%>%add_ratio_df()
#'
#' mtcars %>%select(cyl, am) %>% table() %>%data.frame() %>%to_table() %>%add_sum()
#'
#' mat = matrix(c(10, 50, 10, 28, 22, 16, 68,12, 14, 124, 116, 120),
#' ncol = 4,
#' dimnames = list(c("20s","30s","40s"),
#'                 c("A","B","C","D")))
#' mat
#'
#' mat  %>% long_df("age","brand","count") %>%to_table()
#' # mean data to table
#' aggregate(len ~ ., data = ToothGrowth, mean) %>% xtabs(formula = len ~.)
#' #this is jjstat method
#' aggregate(len ~ ., data = ToothGrowth, mean) %>% to_table()
#'
#' aggregate(len ~ ., data = ToothGrowth, mean) %>% to_table("dose","len")
#'
#' #same method
#' aggregate(len ~ ., data = ToothGrowth, mean) %>% xtabs(len ~ supp + dose, data =.)
#'
#' }
to_table <- function(data, sel = ncol(data)-1, value = ncol(data), type = "mat") {
  data<-data.frame(data)
    # Restructuring dataRestructuring data
  transformed_data <- spread(data, key = all_of(sel), value = all_of(value) )
  transformed_data <- transformed_data%>%dplyr::select(-1)

  # change row name
  rownames(transformed_data) <- unique(data[[1]])
  colnames(transformed_data) <- unique(data[[2]])

  res_mat = transformed_data%>% as.matrix()

  switch(type,
         df = res,
         mat = res_mat)

}


