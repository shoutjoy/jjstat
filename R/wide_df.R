#' pivot_wider
#'
#' @param data long format data
#' @param names_from   names_from
#' @param values_from values_from
#' @param names_prefix  names_prefix +variable
#' @param names_sep sep
#' @param names_sort  colnames sort
#' @param add_id  default FALSE
#'
#' @return  wide data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' mtcars[1:3, 1:7]
#' mtcars[1:3, 1:7] %>% long_df()
#' mtcars[1:3, 1:7] %>% long_df(cols= 3:7)
#' mtcars[1:3, 1:7] %>% long_df(fix=1:2)
#' mtcars[1:3, 1:7] %>% long_df(fix= 1:3, rownames_to_column=F)
#'
#' mtcars[1:3, 1:7] %>% long_df() %>% mutate(ik = "bc", io = "a")
#' mtcars[1:3, 1:7] %>% long_df() %>% mutate(ik = "bc", io = "a")%>% wide_df()
#' mtcars[1:3, 1:7] %>% long_df() %>%
#'   mutate(ik = "bc", io = "a")%>%
#'   wide_df(names_from = c("names","ik"))
#' mtcars[1:3, 1:7] %>% long_df() %>%
#'   mutate(ik = rownames, io = "a")%>%
#'   wide_df(names_from = c("ik"))%>%pall()
#'
#' mtcars[1:3, 1:7] %>% long_df() %>%
#'   mutate(ik = "bc", io = "a")%>%
#'   wide_df(names_from = 2)
#'
#' }
#'
wide_df = function(data,
                   names_from = "names",
                   values_from = "values",
                   names_prefix = "",
                   names_sep = "_",
                   names_sort=FALSE,
                   add_id=FALSE){

  ### # Use the pivot_wider() function to convert the data to a wider format.
  if(add_id){
    res = data %>%add_id() %>%
      pivot_wider(
        names_from = all_of(names_from),
        values_from =  all_of(values_from),
        names_sep = names_sep,
        names_prefix = names_prefix,
        names_sort = names_sort
      )

  }else{
    res = data %>%
      pivot_wider(
        names_from = all_of(names_from),
        values_from =  all_of(values_from),
        names_sep = names_sep,
        names_prefix = names_prefix,
        names_sort = names_sort
      )
  }


  res
}


#' pivot_wider
#'
#' @param data long format data
#' @param names_from   names_from
#' @param values_from values_from
#' @param names_prefix  names_prefix +variable
#' @param names_sep sep
#' @param names_sort  colnames sort
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
#' mtcars[1:3, 1:7] %>% to_wide() %>% mutate(ik = "bc", io = "a")%>% wide_df()
#' mtcars[1:3, 1:7] %>% to_wide() %>%
#'   mutate(ik = "bc", io = "a")%>%
#'   wide_df(names_from = c("names","ik"))
#' mtcars[1:3, 1:7] %>% to_wide() %>%
#'   mutate(ik = rownames, io = "a")%>%
#'   wide_df(names_from = c("ik"))%>%pall()
#'
#' mtcars[1:3, 1:7] %>% to_wide() %>%
#'   mutate(ik = "bc", io = "a")%>%
#'   wide_df(names_from = 2)
#'
#' }
to_wide = function(data,
                   names_from = "names",
                   values_from = "values",
                   names_prefix = "",
                   names_sep = "_",
                   names_sort=FALSE,
                   add_id= TRUE){

  ### # Use the pivot_wider() function to convert the data to a wider format.
  if(add_id){
    res = data %>%add_id() %>%
      pivot_wider(
        names_from = all_of(names_from),
        values_from =  all_of(values_from),
        names_sep = names_sep,
        names_prefix = names_prefix,
        names_sort = names_sort
      )

    res = res%>%tibble::column_to_rownames("id")


  }else{
    res = data %>%
      pivot_wider(
        names_from = all_of(names_from),
        values_from =  all_of(values_from),
        names_sep = names_sep,
        names_prefix = names_prefix,
        names_sort = names_sort
      )
  }


  res

}
