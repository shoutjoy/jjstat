#' Automated Likert scale conversion functions
#'
#' @param data data.frame, vector
#' @param ...  coding levels
#' @param range data col range
#'
#' @return transforamtion data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' # test
#' jut1_custom <- c("그렇다", "매우 그렇다", "보통이다",
#'                  "그렇지 않다", "전혀 그렇지 않다")
#' jut1_custom
#' jut1_custom%>%rev()
#'
#'
#' jut1_custom <- c("그렇다", "매우 그렇다", "보통이다", "그렇지 않다", "전혀 그렇지 않다")
#'
#' auto_coding(jut1_custom, custom=jut1_custom)
#' auto_coding(jut1_custom, custom=jut1_custom, rev=TRUE)
#'
#'
#' # Example data
#' vec1 <- c("그렇다", "그렇다", "매우 그렇다", "그렇다", "매우 그렇다", "매우 그렇다",
#'           "매우 그렇다", "그렇다", "그렇다", "그렇지 않다", "그렇다", "그렇다",
#'           "매우 그렇다", "그렇다", "그렇다", "그렇다", "그렇다", "매우 그렇다",
#'           "전혀 그렇지 않다", "보통이다", "그렇다", "매우 그렇다", "매우 그렇다",
#'           "보통이다", "그렇다", "매우 그렇다", "그렇다", "그렇다", "매우 그렇다",
#'           "매우 그렇다", "그렇다", "그렇다", "그렇다", "그렇다", "보통이다",
#'           "그렇다", "그렇다", "그렇다", "매우 그렇다", "보통이다", "매우 그렇다",
#'           "매우 그렇다", "그렇다", "그렇다", "그렇지 않다", "그렇다", "그렇다",
#'           "그렇다", "보통이다", "그렇다", "그렇다", "보통이다", "그렇지 않다",
#'           "그렇지 않다", "매우 그렇다", "보통이다", "그렇다", "매우 그렇다",
#'           "그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇다", "그렇다",
#'           "그렇다", "매우 그렇다", "그렇다", "보통이다", "그렇다", "그렇다",
#'           "그렇다", "매우 그렇다", "보통이다", "그렇다", "그렇다", "그렇지 않다",
#'           "그렇지 않다", "그렇지 않다", "그렇지 않다", "매우 그렇다", "매우 그렇다",
#'           "그렇지 않다", "그렇다", "전혀 그렇지 않다", "그렇다", "그렇다",
#'           "그렇다", "보통이다", "그렇다", "그렇지 않다", "그렇다", "보통이다",
#'           "매우 그렇다")
#'
#' auto_coding(vec1, custom=jut1_custom, rev=FALSE)
#' auto_coding(vec1, custom=jut1_custom, rev=TRUE)
#'
#'
#'
#' # Example data
#' data1 <- data.frame(
#'   V1 = c("그렇다", "그렇다", "매우 그렇다", "그렇다", "매우 그렇다",
#'          "매우 그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇지 않다",
#'          "그렇다", "그렇다", "매우 그렇다", "그렇다", "그렇다",
#'          "그렇다", "그렇다", "매우 그렇다",  "전혀 그렇지 않다", "보통이다",
#'          "그렇다", "매우 그렇다", "매우 그렇다", "보통이다", "그렇다",
#'          "매우 그렇다", "그렇다", "그렇다", "매우 그렇다",   "매우 그렇다",
#'          "그렇다", "그렇다", "그렇다", "그렇다", "보통이다",
#'          "그렇다", "그렇다", "그렇다", "매우 그렇다", "보통이다",
#'          "매우 그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇지 않다",
#'          "그렇다", "그렇다",
#'          "그렇다", "보통이다", "그렇다", "그렇다", "보통이다", "그렇지 않다",
#'          "그렇지 않다", "매우 그렇다", "보통이다", "그렇다", "매우 그렇다",
#'          "그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇다", "그렇다",
#'          "그렇다", "매우 그렇다", "그렇다", "보통이다", "그렇다", "그렇다",
#'          "그렇다", "매우 그렇다", "보통이다", "그렇다", "그렇다", "그렇지 않다",
#'          "그렇지 않다", "그렇지 않다", "그렇지 않다", "매우 그렇다", "매우 그렇다",
#'          "그렇지 않다", "그렇다", "전혀 그렇지 않다", "그렇다", "그렇다",
#'          "그렇다", "보통이다", "그렇다", "그렇지 않다", "그렇다", "보통이다",
#'          "매우 그렇다", "보통이다")
#' )
#'
#' auto_coding(data1)
#' data1
#'
#' # Example data
#' data2 <- data.frame(
#'   V1 = c("그렇다", "그렇다", "매우 그렇다", "그렇다", "매우 그렇다",
#'          "매우 그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇지 않다",
#'          "그렇다", "그렇다", "매우 그렇다", "그렇다", "그렇다",
#'          "그렇다", "그렇다", "매우 그렇다",  "전혀 그렇지 않다", "보통이다",
#'          "그렇다", "매우 그렇다", "매우 그렇다", "보통이다", "그렇다",
#'          "매우 그렇다", "그렇다", "그렇다", "매우 그렇다",   "매우 그렇다",
#'          "그렇다", "그렇다", "그렇다", "그렇다", "보통이다",
#'          "그렇다", "그렇다", "그렇다", "매우 그렇다", "보통이다",
#'          "매우 그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇지 않다",
#'          "그렇다", "그렇다"),
#'   v2=c( "그렇다", "보통이다", "그렇다", "그렇다", "보통이다", "그렇지 않다",
#'         "그렇지 않다", "매우 그렇다", "보통이다", "그렇다", "매우 그렇다",
#'         "그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇다", "그렇다",
#'         "그렇다", "매우 그렇다", "그렇다", "보통이다", "그렇다", "그렇다",
#'         "그렇다", "매우 그렇다", "보통이다", "그렇다", "그렇다", "그렇지 않다",
#'         "그렇지 않다", "그렇지 않다", "그렇지 않다", "매우 그렇다", "매우 그렇다",
#'         "그렇지 않다", "그렇다", "전혀 그렇지 않다", "그렇다", "그렇다",
#'         "그렇다", "보통이다", "그렇다", "그렇지 않다", "그렇다", "보통이다",
#'         "매우 그렇다", "보통이다")
#' )
#' data2
#' auto_coding(data2)
#'
#'
#' }
auto_coding <- function(data, ..., range = 1:ncol(data),
                        custom=NULL, rev=FALSE, data.fram= FALSE) {
  #First checks if it's a dataframe
  if(is.data.frame(data)){
    #Check if there is only one column and change it to a vector
    if(ncol(data)==1 ){
      data <- as.vector(data)
    }
  }
  ### If input is a vector
  if (is.vector(data)) {
    # null custom
    if(is.null(custom)){
      #reverse
      if(rev){
        default_levels <- rev(c("전혀 그렇지 않다", "그렇지 않다",
                                "보통이다", "그렇다", "매우 그렇다"))
      }else{
        default_levels <- c("전혀 그렇지 않다", "그렇지 않다",
                            "보통이다", "그렇다", "매우 그렇다")
      }

    }else{
      #exist custom
      if(rev){ #reverse
        default_levels <- rev(custom)
      }else{
        default_levels <- custom
      }
    }

    # Extract level parameter
    levels <- list(...)$levels
    if (is.null(levels)) {
      levels <- default_levels
    }
    # Mapping levels to numbers
    mapping <- setNames(seq_along(levels), levels)
    # Convert data
    result <- as.numeric(sapply(data, function(value) {
      if (!is.null(mapping[value])) {
        return(mapping[value])
      } else {
        return(NA)  # Treat unmapped values as NA
      }
    }))

    if(data.fram){
      return(data.frame(result))
    }else{
      return(result)
    }


  } else {  # If input is not a vector###################

    if(is.null(custom)){
      #reverse
      if(rev){
        default_levels <- rev(c("전혀 그렇지 않다", "그렇지 않다",
                                "보통이다", "그렇다", "매우 그렇다"))
      }else{
        default_levels <- c("전혀 그렇지 않다", "그렇지 않다",
                            "보통이다", "그렇다", "매우 그렇다")
      }

    }else{
      if(rev){
        default_levels <- rev(custom)
      }else{
        default_levels <- custom
      }
    }
    # Extract level parameter
    levels <- list(...)$level
    if (is.null(levels)) {
      levels <- default_levels
    }

    # Mapping levels to numbers
    mapping <- setNames(seq_along(levels), levels)

    # Convert a specified column
    data[, range] <- lapply(data[, range], function(column) {
      as.numeric(sapply(column, function(value) {
        if (!is.null(mapping[value])) {
          return(mapping[value])
        } else {
          return(NA)  # Treat unmapped values as NA
        }
      }))#numeric
    })

    return(data)
  }
}
