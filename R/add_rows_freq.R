#' Functions that generate rows with a frequency
#'
#' @param data data
#' @param sel frequency column
#' @param fix final select column
#' @param type res, all
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' dff = data.frame(
#'   trt = c("r","s","z"),
#'   con = c("f","m","l"),
#'   num = c(10,0,2),
#'   fre = c(4,0,5) )
#' dff
#' dff %>%add_rows_freq()
#' dff %>%add_rows_freq()
#' dff %>%add_rows_freq(fix=1)
#' dff %>%add_rows_freq("res", fix=1:2)
#' dff %>%add_rows_freq("res", fix="con")
#' dff %>%add_rows_freq("all")
#' dff %>%add_rows_freq("real")
#' dff %>%add_rows_freq("zero")
#' dff %>%add_rows_freq("zero", fix="con")
#'
#' }
#'
#'
add_rows_freq <- function(data,
                          type = "res",
                          sel = ncol(data),
                          fix = NULL) {
  # value add
  data <- data %>%data.frame()
  data <- data %>%
    dplyr::mutate(value = ifelse(data[, sel] == 0 | data[, sel] == 1, 1,
                                 data[, sel]),
                  binary = ifelse(data[[sel]] == 0, 0, 1)
    )

  # Generate by value when creating rows
  Rows = rep(seq_len(nrow(data)), data$value)
  expanded_data0 <- data[Rows, ]
  #For data retention
  expanded_data<-expanded_data0
  #result
  all =  expanded_data
  res =  dplyr::bind_cols(expanded_data %>%purrr::discard(is.numeric),
                          bin= expanded_data0$binary)
  real  = expanded_data %>%
    dplyr::filter(binary != 0) %>%
    purrr::discard(is.numeric)

  real_zero =  expanded_data %>%
    rowid_to_column("id") %>%
    dplyr::mutate(zero = ifelse(binary==1,"","*"))%>%
    tidyr::unite(id, id, zero,sep="")%>%
    purrr::discard(is.numeric) %>%
    column_to_rownames("id")

  # column selection
  if(is.null(fix)){
    all = all
    res = res
    real = real
    real_zero =  real_zero
  }else{
    all = all %>% dplyr::select(fix)
    res = res %>% dplyr::select(fix)
    real = real %>% dplyr::select(fix)
    real_zero =  real_zero %>% dplyr::select(fix)
  }

  switch(type,
         all = all,
         res = res,
         real = real,
         zero = real_zero
  )
}
