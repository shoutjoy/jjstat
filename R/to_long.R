#' longdata transfomation
#'
#' @param data widedata
#' @param names_to name
#' @param values_to value
#' @param cols colrange
#' @param fix  column fixed (thereofore not using cols )
#' @param rowname defai;t acce
#' @param rownames_to_column rownames_to_column=FALSE
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
                   names_to = "names",
                   values_to = "values",
                   cols = NULL,
                   fix = NULL,
                   rownames_to_column=FALSE,
                   rowname ="rows"){

  data = data %>% data.frame()

  if(is.null(cols)){
    cols_selection  = dplyr::select(data, where(is.numeric)) %>% colnames()

  }else{
    cols_selection = cols
  }

  colName = colnames(data)
  rowName = rownames(data) #accent
  # colnames0 = colnames(data)
  if(rownames_to_column){
    data1 = data %>% data.frame() %>%
      tibble::rownames_to_column(rowname)

    colnames(data1)= c(rowname, colName)
    # If you don't need to create rownames_to_columns
    if(is.null(fix)){

      data2 <- data1%>%
        tidyr::pivot_longer(names_to = names_to,
                            values_to = values_to,
                            cols = cols_selection)
    }else{
      index =  match(rowname, colnames(data1))
      data2 <- data1%>%
        # dplyr::select(-rownames) %>%
        tidyr::pivot_longer(names_to = names_to,
                            values_to = values_to,
                            cols = -c( {index}, all_of(fix))
        )
    }

  }else{

    data1 = data %>% data.frame()
    # If you don't need to create rownames_to_columns
    if(is.null(fix)){

      data2 <- data1%>%
        tidyr::pivot_longer(names_to = names_to,
                            values_to = values_to,
                            cols = cols_selection)
    }else{
      data2 <- data1%>%
        tidyr::pivot_longer(names_to = names_to,
                            values_to = values_to,
                            cols = -all_of(fix))
    }


  }
  data2
}
