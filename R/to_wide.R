#' pivot_wider
#'
#' @param data long format data
#' @param names_from   names_from
#' @param values_from values_from
#' @param names_prefix  names_prefix +variable
#' @param names_sep sep
#' @param names_sort  colnames sort
#' @param add_id Select Repeating Groups to create IDs, line in column names or column numbers
#' @param rm_cols Remove variables that interfere with wide format by creating individual IDs
#'
#' @return  wide data
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' mtcars[1:3, 1:7]
#' #this is error
#' mtcars[1:4, 1:5] %>% to_long() %>% wide_df()
#'
#' mtcars[1:3, 1:7] %>% add_id()%>% to_wide()
#' mtcars[1:3, 1:7] %>% to_wide(add_id=TRUE)

#' mtcars[1:3, 1:7] %>% to_wide(cols= 3:7)
#' mtcars[1:3, 1:7] %>% to_wide(fix=1:2)
#' mtcars[1:3, 1:7] %>% to_wide(fix= 1:3, rownames_to_column=F)
#'
#' mtcars[1:3, 1:7] %>% to_wide() %>% mutate(ik = "bc", io = "a")
#' mtcars[1:3, 1:7] %>% to_wide() %>% mutate(ik = "bc", io = "a")%>% to_wide()
#'
#' mtcars[1:3, 1:7] %>% to_wide() %>%
#'   mutate(ik = "bc", io = "a")%>%
#'   to_wide(names_from = c("names","ik"))
#'
#' mtcars[1:3, 1:7] %>% to_wide() %>%
#'   mutate(ik = rownames, io = "a")%>%
#'   to_wide(names_from = c("ik"))%>%pall()
#'
#' mtcars[1:3, 1:7] %>% to_wide() %>%
#'   mutate(ik = "bc", io = "a")%>%
#'   to_wide(names_from = 2)
#'
#'
#'
#' #일반적인 데이터에서  롱포맷으로 바꾸기
#' data <- data.frame(
#'   weight = c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14,
#'              4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69,
#'              6.31, 5.12, 5.54, 5.50, 5.37, 5.29, 4.92, 6.15, 5.80, 5.26),
#'   group = c(rep("ctrl", 10), rep("trt1", 10), rep("trt2", 10))
#' )
#' data
#' data%>%add_rep_id("group")
#'
#' data%>%add_rep_id("group")%>%jjstat::as_trt("id") %>%wide_df("group", "weight")
#'
#' data  %>%wide_df("group", "weight", add_id="group")
#'
#'
#' #개별적인 그룹을 만들어진 경우
#' data2 <- data.frame(
#'   weight = c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14,
#'              4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69,
#'              6.31, 5.12, 5.54, 5.50, 5.37, 5.29, 4.92, 6.15, 5.80, 5.26),
#'   ids = c(paste0("C", 1:10), paste0("T",1:10), paste0("S", 1:10))
#' )
#'
#' data2
#'
#' data2 %>%jjstat::mutate_col(group= c(rep("ctrl", 10),
#'                  rep("trt1", 10), rep("trt2", 10)), col=2)
#'
#' # 그룹을 나누어주는 아이디 생성
#' data2 %>%
#' jjstat::mutate_col(group= c(rep("ctrl", 10),
#'         rep("trt1", 10), rep("trt2", 10)), col=2)%>%
#'         add_rep_id("group")
#'
#' # 개별적인 그룹을 만드는 열이 존재하는 경우 --> 문제발생
#' data2 %>%
#'     jjstat::mutate_col(group= c(rep("ctrl", 10),
#'           rep("trt1", 10), rep("trt2", 10)), col=2)%>%
#'           wide_df("group", "weight", add_id="group")
#'
#' #개별적인 그룹을 만드는 열을 제거하는 경우 --> 문제해결
#' data2 %>%
#'   jjstat::mutate_col(group= c(rep("ctrl", 10),
#'   rep("trt1", 10), rep("trt2", 10)),  col=2)%>%
#'   wide_df("group", "weight", add_id="group", rm_cols="ids") %>%print(n=Inf)
#' #'
#' data3= data.frame(
#'   weight=c(1:9),
#'   group=c(rep("a",3),rep("b",3),rep("c",3))
#' )
#' data3
#' data3 %>% to_wide("group", "weight", add_id=2)

#'
#' }
to_wide = function(data,
                   names_from = "names",
                   values_from = "values",
                   names_prefix = "",
                   names_sep = "_",
                   names_sort = FALSE,
                   rm_cols= NULL,
                   add_id = NULL){

  ### # Use the pivot_wider() function to convert the data to a wider format.
  if(is.null(add_id)){


    res = data %>%
      dplyr::select(-{{rm_cols}})%>%
      tidyr::pivot_wider(
        names_from = all_of(names_from),
        values_from =  all_of(values_from),
        names_sep = names_sep,
        names_prefix = names_prefix,
        names_sort = names_sort)

  }else{

    res = data%>%
      dplyr::select(-{{rm_cols}})%>%
      add_rep_id(grp = add_id) %>%
      tidyr::pivot_wider(
        names_from = all_of(names_from),
        values_from =  all_of(values_from),
        names_sep = names_sep,
        names_prefix = names_prefix,
        names_sort = names_sort)

  }
  res
}
