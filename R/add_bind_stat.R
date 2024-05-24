
#' Functions that item parceling generate multiple pieces of data,
#'
#' @param data data.fram
#' @param ...  colname, cols
#' @param fun fun  mean sd
#' @param type data, res --data.fram , res, cols_check
#'
#'
#' @return data.fram
#' @export
#'
#' @examples
#' \dontrun{
#'
#' #Common item parceling methods (group by average )
#' mtcars %>% mutate(
#' Mean_col1 = (mpg + cyl)/2 ,
#' Mean_col2 = (disp + hp)/2 ,
#' Mean_col3 = (drat + wt )/2 ,
#' Mean_col4 = (qsec + vs + am)/3 )
#'
#' #Methods for item parceling using functions
#' add_bind_stat(mtcars,
#' c("Mean_col1","mpg", "cyl"),
#' c("Mean_col2", "disp", "hp"),
#' c("Mean_col3", "drat", "wt"),
#' c("Mean_col4", "qsec", "vs", "am") )
#'
#'
#'
#' add_bind_stat(mtcars,
#'    c("Mean_col1", 1:2),
#'    c("Mean_col2", 3:4),
#'    c("Mean_col3", 5:6),
#'    c("Mean_col4", 7:9))
#' #'  ##col_check
#' #'
#' add_bind_stat(mtcars,
#'               c("Mean_col1", "mpg:cyl"),
#'               c("Mean_col2", "disp:hp"),
#'               c("Mean_col3", "drat:wt"),
#'               c("Mean_col4", "qsec:am"))
#' }
#'
add_bind_stat <- function(data, ..., fun = mean, type="res") {
  term <- list(...)
  col_names <- vector("list", length(term))  #Initialize the list
  cols <- vector("list", length(term))       #Initialize the list
  cols_check <- vector("list", length(term))#Initialize the list
  # colnas <- vector("list", length(term))
  # Colsnames = colnames(data)

  for (i in seq_along(term)) {
    cols_check[[i]] <- term[[i]][-1]
  }

  if( unique(is.na(cols_check %>% unlist())) ){
    cols_check  = cols_check%>%unlist()
  }else{

    #  cols_check <- cols_check%>%unlist() %>% as.numeric()
    #suppres sWarnings  message
    cols_check <- suppressWarnings(cols_check%>%unlist() %>% as.numeric())
  }

  #   if (any(length(cols_check) <= 1 )) {
  #   cols_check <- NULL
  # } else if (length(cols_check) > 1) {
  #  cols_check <- unlist(cols_check) %>% as.numeric()
  #  }

  for (i in seq_along(term)) {
    col_names[[i]] <- term[[i]][1]  # Assign a value to the list
    cols[[i]] <- term[[i]][-1]      # Assign a value to the list

    if(unique(is.na(unlist(cols_check)))){
      cols[[i]] <- term[[i]][-1]     # Assign a value to the list

    }else{

      #  cols[[i]] <- colnames( data[ suppressWarnings(as.numeric(cols[[i]])) ] )
      cols[[i]] <- colnames( data[ as.numeric(cols[[i]]) ] )
    }




    if (is.character(cols[[i]]) && length(cols[[i]]) == 1 && grepl(":", cols[[i]])) {
      range_vars <- unlist(strsplit(cols[[i]], ":"))
      cols[[i]] <- names(data)[which(names(data) == range_vars[1]):which(names(data) == range_vars[2])]
    }



    data[[col_names[[i]]]] = add_stat(data,
                                      cols[[i]] %>% unlist(),
                                      col_name = col_names[[i]] %>% unlist(),
                                      fun = fun,
                                      type = "col")
  }

  # return(data)
  res= list(data, cols_check)

  switch(type,
         data= data ,
         all= res,
         res = data,
         cols_check= cols_check
         )

}



#' Functions that generate multiple pieces of data
#'
#' @param data data.fram
#' @param ...  colname, cols
#' @param fun fun  mean sd
#' @param type data, res --data.fram , res, cols_check
#'
#'
#' @return data.fram
#' @export
#'
#' @examples
#' \dontrun{
#'
#' #Common item parceling methods (group by average )
#' mtcars %>% mutate(
#' Mean_col1 = (mpg + cyl)/2 ,
#' Mean_col2 = (disp + hp)/2 ,
#' Mean_col3 = (drat + wt )/2 ,
#' Mean_col4 = (qsec + vs + am)/3 )
#'
#' #Methods for item parceling using functions
#' bind_add_stat(mtcars,
#' c("Mean_col1","mpg", "cyl"),
#' c("Mean_col2", "disp", "hp"),
#' c("Mean_col3", "drat", "wt"),
#' c("Mean_col4", "qsec", "vs", "am") )
#'
#'
#'
#' bind_add_stat(mtcars,
#'    c("Mean_col1", 1:2),
#'    c("Mean_col2", 3:4),
#'    c("Mean_col3", 5:6),
#'    c("Mean_col4", 7:9))
#'  ##col_check
#' #'  '
#' bind_add_stat(mtcars,
#'               c("Mean_col1", "mpg:cyl"),
#'               c("Mean_col2", "disp:hp"),
#'               c("Mean_col3", "drat:wt"),
#'               c("Mean_col4", "qsec:am"))
#'
#' }
#'
bind_add_stat <- function(data, ..., fun = mean, type="res") {
  term <- list(...)
  col_names <- vector("list", length(term))  #Initialize the list
  cols <- vector("list", length(term))       #Initialize the list
  cols_check <- vector("list", length(term))#Initialize the list
  # colnas <- vector("list", length(term))
  # Colsnames = colnames(data)

  for (i in seq_along(term)) {
    cols_check[[i]] <- term[[i]][-1]
  }

  if( unique(is.na(cols_check %>% unlist())) ){
    cols_check  = cols_check%>%unlist()
  }else{

    #  cols_check <- cols_check%>%unlist() %>% as.numeric()
    #suppres sWarnings  message
    cols_check <- suppressWarnings(cols_check%>%unlist() %>% as.numeric())
  }


  for (i in seq_along(term)) {
    col_names[[i]] <- term[[i]][1]  # Assign a value to the list
    cols[[i]] <- term[[i]][-1]      # Assign a value to the list

    if(unique(is.na(unlist(cols_check)))){
      cols[[i]] <- term[[i]][-1]     # Assign a value to the list

    }else{

      #  cols[[i]] <- colnames( data[ suppressWarnings(as.numeric(cols[[i]])) ] )
      cols[[i]] <- colnames( data[ as.numeric(cols[[i]]) ] )
    }

    if (is.character(cols[[i]]) && length(cols[[i]]) == 1 && grepl(":", cols[[i]])) {
      range_vars <- unlist(strsplit(cols[[i]], ":"))
      cols[[i]] <- names(data)[which(names(data) == range_vars[1]):which(names(data) == range_vars[2])]
    }


    data[[col_names[[i]]]] = add_stat(data,
                                      cols[[i]] %>% unlist(),
                                      col_name = col_names[[i]] %>% unlist(),
                                      fun = fun,
                                      type = "col")
  }

  # return(data)
  res= list(data, cols_check)

  switch(type,
         data= data ,
         all= res,
         res = data,
         cols_check= cols_check
  )

}
#
# add_stat <- function(data,
#                      ...,
#                      fun = mean,
#                      col_name = NULL,
#                      na.rm = TRUE,
#                      margin = 1,
#                      type="data") {
#
#
#   # 선택한 열 중에 존재하지 않는 열이 있을 경우, 해당 열을 col_name으로 지정
#   if (any(is.na(match(c(...), colnames(data))))) {
#     cat("\n\n Undefined columns selected\n\n")
#
#     vars = c(...)
#     index = match(vars, colnames(data))%>%is.na()  %>% which()
#     col_name = vars[index]
#     selected_cols = vars[-index]
#
#     data[[col_name]] <- apply(data[,selected_cols],
#                               margin, fun, na.rm = na.rm)
#     # return(data)
#   }else{
#     if (!is.null(col_name)) {
#       selected_cols <- data[, c(...), drop = FALSE]
#       data[[col_name]] <- apply(selected_cols,
#                                 margin, fun, na.rm = na.rm)
#       # return(data)
#     }else{
#       selected_cols <- data[, c(...), drop = FALSE]
#       col_name <- paste0("V_",paste0(..., collapse = "_"))
#       data[[col_name]] <- apply(selected_cols, margin, fun, na.rm = na.rm)
#     }
#   }
#
#   #  return(data)
#   switch(type,
#          data = data,
#          col = data[[col_name]])
#
#
# }
