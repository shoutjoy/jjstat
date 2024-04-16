
#' longdata transfomation
#'
#' @param data widedata
#' @param names_to name
#' @param values_to value
#' @param cols colrange
#' @param fix  column fixed (thereofore not using cols )
#' @param rowname defailt accent
#'
#' @return longdata
#' @export
#'
#' @examples
#' \dontrun{
#'
#' mtcars[,1:4] %>%long_df("car", "value")
#'
#'  mtcars[,1:6] %>%long_df("car", "value", rownames_to_column = F, cols = c(1,3:6))
#'
#' }
long_df = function(data,
                   rowname = "accent",
                   names_to = "speaker",
                   values_to = "freq",
                   cols = NULL,
                   fix = NULL,
                   rownames_to_column = TRUE){

  if(is.null(cols)){
    cols_selection  = data%>% keep(is.numeric) %>% colnames()
  }else{
    cols_selection = cols
  }


  colName = colnames(data)
  rowName = rownames(data) #accent

  if(rownames_to_column){
    data1 = data %>%
            data.frame() %>%
              rownames_to_column()

    colnames(data1)= c(rowname, colName)

    data2 <- data1%>%
      pivot_longer(names_to = names_to,
                   values_to = values_to,
                   cols = cols_selection)
  }else{

    data1 = data %>% data.frame()

    if(is.null(fix)){

      data2 <- data1%>%
        pivot_longer(names_to = names_to,
                     values_to = values_to,
                     cols = cols_selection)
    }else{
      data2 <- data1%>%
        pivot_longer(names_to = names_to,
                     values_to = values_to,
                     cols = -fix)
    }
 }
  data2
}



#' longdata transfomation
#'
#' @param data widedata
#' @param names_to name
#' @param values_to value
#' @param cols colrange
#' @param fix  column fixed (thereofore not using cols )
#' @param rowname defai;t acce
#'
#' @return longdata
#' @export
#'
#' @examples
#' \dontrun{
#'
#' mtcars[,1:4] %>%to_long("car", "value")
#' mtcars[,1:4] %>%to_long("car", "value", fix="cyl")
#' mtcars[,1:4] %>%to_long("car", "value", fix= 1:2)
#' mtcars[,1:4] %>%to_long("car", "value", fix= c(1))
#' mtcars[,1:4] %>%to_long("car", "value", fix= c(2))
#' mtcars[,1:4] %>%to_long("car", "value", fix=c("cyl","mpg"))
#' mtcars[,1:6] %>%long_df("car", "value", rownames_to_column = F, cols = c(1,3:6))
#'
#'
#'
#'
#'
#'
#' }
to_long = function(data,
                   cols = NULL,
                   names_to = "names",
                   values_to = "vlaues",
                   rowname ="rows",
                   fix = NULL,
                   rownames_to_column=FALSE){

  if(is.null(cols)){
    cols_selection  = data%>% keep(is.numeric) %>% colnames()
  }else{
    cols_selection = cols
  }

  colName = colnames(data)
  rowName = rownames(data) #accent
  # colnames0 = colnames(data)
  if(rownames_to_column){
    data1 = data %>% data.frame() %>%
      rownames_to_column(rowname)

    colnames(data1)= c(rowname, colName)

    data2 <- data1%>%
      pivot_longer(names_to = names_to,
                   values_to = values_to,
                   cols = cols_selection
                   )
  }else{

    data1 = data %>% data.frame()

    if(is.null(fix)){

      data2 <- data1%>%
        pivot_longer(names_to = names_to,
                     values_to = values_to,
                     cols = cols_selection)
    }else{
      data2 <- data1%>%
        pivot_longer(names_to = names_to,
                     values_to = values_to,
                     cols = -fix)
    }


  }
  data2
}

