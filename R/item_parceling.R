#' item_parceling: A Function to Generate Multiple Data Parcels
#'
#' @param data A data.frame containing the source data.
#' @param ... Triples specifying the name of the new column, followed by the column names or indices to be used for parceling.
#' @param fun A function to apply for the parceling operation (default is `mean`).
#' @param type Type of output: `"data"` (default data frame), `"res"` (data frame), `"cols_check"` (column checks).
#'
#' @return A data frame or other result based on the specified type.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Common item parceling methods (group by average)
#' mtcars %>% mutate(
#'   Mean_col1 = (mpg + cyl) / 2,
#'   Mean_col2 = (disp + hp) / 2,
#'   Mean_col3 = (drat + wt) / 2,
#'   Mean_col4 = (qsec + vs + am) / 3
#' )
#'
#' # Using indices for parceling
#' item_parceling(mtcars,
#'   c("Mean_col1", 1:2),
#'   c("Mean_col2", 3:4),
#'   c("Mean_col3", 5:6),
#'   c("Mean_col4", 7:9)
#' )
#'
#' # Using ranges in column names
#' item_parceling(mtcars,
#'   c("Mean_col1", "mpg:cyl"),
#'   c("Mean_col2", "disp:hp"),
#'   c("Mean_col3", "drat:wt"),
#'   c("Mean_col4", "qsec:am")
#'
#'
#' # Methods for item parceling using functions
#' item_parceling(mtcars,
#'   c("Mean_col1", "mpg", "cyl"),
#'   c("Mean_col2", "disp", "hp"),
#'   c("Mean_col3", "drat", "wt"),
#'   c("Mean_col4", "qsec", "vs", "am"),
#'   fun = mean
#' )
#'
#' # Using the sum function
#' item_parceling(mtcars,
#'   c("Sum_col1", "mpg", "cyl"),
#'   c("Sum_col2", "disp", "hp"),
#'   fun = sum
#' )
#'
#' # Using the median function
#' item_parceling(mtcars,
#'   c("Median_col1", "drat", "wt"),
#'   c("Median_col2", "qsec", "vs"),
#'   fun = median
#' )
#'
#' # Using a custom function
#' custom_function <- function(x) {
#'   return(max(x) - min(x)) # Range calculation
#' }
#'
#' item_parceling(mtcars,
#'   c("Range_col1", "mpg", "cyl"),
#'   c("Range_col2", "disp", "hp"),
#'   fun = custom_function
#' )
#'
#' # Using column indices for parceling
#' item_parceling(mtcars,
#'   c("Sum_col1", 1:2),
#'   c("Sum_col2", 3:4),
#'   fun = sum
#' )
#'
#' # Using ranges in column names
#' item_parceling(mtcars,
#'   c("Range_col1", "mpg:cyl"),
#'   c("Range_col2", "disp:hp"),
#'   fun = custom_function
#' )
#' }
#'
item_parceling <- function(data, ..., fun = mean, type = "res") {
  term <- list(...)
  col_names <- vector("list", length(term))
  cols <- vector("list", length(term))
  cols_check <- vector("list", length(term))

  for (i in seq_along(term)) {
    cols_check[[i]] <- term[[i]][-1]
  }

  if (unique(is.na(unlist(cols_check)))) {
    cols_check <- unlist(cols_check)
  } else {
    cols_check <- suppressWarnings(as.numeric(unlist(cols_check)))
  }

  for (i in seq_along(term)) {
    col_names[[i]] <- term[[i]][1]
    cols[[i]] <- term[[i]][-1]

    if (unique(is.na(unlist(cols_check)))) {
      cols[[i]] <- term[[i]][-1]
    } else {
      cols[[i]] <- colnames(data)[as.numeric(cols[[i]])]
    }

    if (is.character(cols[[i]]) && length(cols[[i]]) == 1 && grepl(":", cols[[i]])) {
      range_vars <- unlist(strsplit(cols[[i]], ":"))
      cols[[i]] <- names(data)[which(names(data) == range_vars[1]):which(names(data) == range_vars[2])]
    }

    data[[col_names[[i]]]] <- apply(data[cols[[i]]], 1, fun, na.rm = TRUE)
  }

  res <- list(data, cols_check)

  switch(type,
         data = data,
         all = res,
         res = data,
         cols_check = cols_check
  )
}


#'
#' #' item_parceling of Functions that generate multiple pieces of data
#' #'
#' #' @param data data.fram
#' #' @param ...  colname, cols
#' #' @param fun fun  mean sd
#' #' @param type data, res --data.fram , res, cols_check
#' #'
#' #'
#' #' @return data.frame
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #'
#' #' #Common item parceling methods (group by average )
#' #' mtcars %>% mutate(
#' #' Mean_col1 = (mpg + cyl)/2 ,
#' #' Mean_col2 = (disp + hp)/2 ,
#' #' Mean_col3 = (drat + wt )/2 ,
#' #' Mean_col4 = (qsec + vs + am)/3 )
#' #'
#' #' #Methods for item parceling using functions
#' #' item_parceling(mtcars,
#' #' c("Mean_col1","mpg", "cyl"),
#' #' c("Mean_col2", "disp", "hp"),
#' #' c("Mean_col3", "drat", "wt"),
#' #' c("Mean_col4", "qsec", "vs", "am") )
#' #'
#' #'
#' #'
#' #' item_parceling(mtcars,
#' #'    c("Mean_col1", 1:2),
#' #'    c("Mean_col2", 3:4),
#' #'    c("Mean_col3", 5:6),
#' #'    c("Mean_col4", 7:9))
#' #'  ##col_check
#' #' #'  '
#' #' item_parceling(mtcars,
#' #'               c("Mean_col1", "mpg:cyl"),
#' #'               c("Mean_col2", "disp:hp"),
#' #'               c("Mean_col3", "drat:wt"),
#' #'               c("Mean_col4", "qsec:am"))
#' #'
#' #' }
#' #'
#' item_parceling <- function(data, ..., fun = mean, type="res") {
#'   term <- list(...)
#'   col_names <- vector("list", length(term))  #Initialize the list
#'   cols <- vector("list", length(term))       #Initialize the list
#'   cols_check <- vector("list", length(term))#Initialize the list
#'   # colnas <- vector("list", length(term))
#'   # Colsnames = colnames(data)
#'
#'   for (i in seq_along(term)) {
#'     cols_check[[i]] <- term[[i]][-1]
#'   }
#'
#'   if( unique(is.na(cols_check %>% unlist())) ){
#'     cols_check  = cols_check%>%unlist()
#'   }else{
#'
#'     #  cols_check <- cols_check%>%unlist() %>% as.numeric()
#'     #suppres sWarnings  message
#'     cols_check <- suppressWarnings(cols_check%>%unlist() %>% as.numeric())
#'   }
#'
#'
#'   for (i in seq_along(term)) {
#'     col_names[[i]] <- term[[i]][1]  # Assign a value to the list
#'     cols[[i]] <- term[[i]][-1]      # Assign a value to the list
#'
#'     if(unique(is.na(unlist(cols_check)))){
#'       cols[[i]] <- term[[i]][-1]     # Assign a value to the list
#'
#'     }else{
#'
#'       #  cols[[i]] <- colnames( data[ suppressWarnings(as.numeric(cols[[i]])) ] )
#'       cols[[i]] <- colnames( data[ as.numeric(cols[[i]]) ] )
#'     }
#'
#'     if (is.character(cols[[i]]) && length(cols[[i]]) == 1 && grepl(":", cols[[i]])) {
#'       range_vars <- unlist(strsplit(cols[[i]], ":"))
#'       cols[[i]] <- names(data)[which(names(data) == range_vars[1]):which(names(data) == range_vars[2])]
#'     }
#'
#'
#'     data[[col_names[[i]]]] = add_stat(data,
#'                                       cols[[i]] %>% unlist(),
#'                                       col_name = col_names[[i]] %>% unlist(),
#'                                       fun = fun,
#'                                       type = "col")
#'   }
#'
#'   # return(data)
#'   res= list(data, cols_check)
#'
#'   switch(type,
#'          data= data ,
#'          all= res,
#'          res = data,
#'          cols_check= cols_check
#'   )
#'
#' }
