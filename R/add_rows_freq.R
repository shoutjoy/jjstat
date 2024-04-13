#' Functions that generate rows with a frequency
#'
#' @param data data
#' @param sel frequency column
#' @param remove raw freq remove
#' @param value frequency 1
#' @param type res, all
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dff=data.frame(
#'   trt=c("r","s"),
#'   con = c("f","m"),
#'   fre=c(4,5))
#'
#' dff
#' dff %>%add_rows_freq()
#' dff %>%add_rows_freq(value=F)
#' dff %>%add_rows_freq(3)
#' dff %>%add_rows_freq("fre")
#'
#' }
#'
#'
add_rows_freq = function(data,
                         sel = ncol(data) ,
                         remove=TRUE,
                         value=TRUE,
                         type="res"){
  data<- data
  Rows = rep(seq_len(nrow(data)), data[[sel]])
  #generate data
  expanded_data <- data[Rows,]

  if(remove){

    if(value){
      expanded_data <- expanded_data%>%select(-all_of(sel))%>%
        mutate(value =1)%>%
        data.frame()
    }else{
      expanded_data <- expanded_data%>%select(-all_of(sel))%>%
        # mutate(value =1)%>%
        data.frame()
    }


  }else{
    expanded_data <- expanded_data  %>%data.frame()
  }

  all = list( Rows, expanded_data)

  switch(type, all=all, res = expanded_data )
}
