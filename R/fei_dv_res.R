#' T종속변수의 빈도를 세는 함수 (에듀테크 연구보고용 )
#'
#' @param df df
#' @param title 변수명
#' @param x_size  20 사이즈로 조절
#' @param label_text  막대그래프 끄트
#' @param x_range 범위조절
#' @param type  res all, freq, console, md, gg, graph na_Count, cleandata
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#'
#' eduteck_s$Q14_5 %>% fei_dv_res("자기주도 공부력 향상")
#'
#'
#' }
fei_dv_res = function(df,title ="",
                      x_size = 20,
                      label_text = 5,
                      x_range= 100,
                      type="res"
){
  # df_var에서 ""을 먼저 제거 하여 데이터 구성

  df_var = na_remover(df)
  na_Count = na_remover(df, type="na_count")

  console = Freq_table(df_var, prop=TRUE) %>%
    add_row_sum() %>%
    move_rows(4,1,4, 2, 5,3) %>%
    Freq_table_apa(paste0(title,"에 관한 "))
  # Freq_table_colnames()

  markdown = Freq_table(df_var, prop=TRUE) %>%
    add_row_sum() %>%
    Freq_table_colnames()%>%
    move_rows(4,1,4, 2, 5,3) %>%
    md(caption =title)


  gg = Freq_table(df_var, prop=TRUE) %>%
    table_df_bar(size.x= x_size, size.text = label_text,
                 x.title=title, add_range = x_range, flip=FALSE)

  res = list(console= console, markdown=markdown, na_Count= na_Count, graph=gg)

  #결과

  switch(type, res= res, all = res, freq= console, console= console,
         md = markdown, gg = gg , graph =gg,
         na_Count=na_Count,
         cleandata=df_var
  )

}
