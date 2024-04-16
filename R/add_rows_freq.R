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
#' dff %>%add_rows_freq(sel=4)
#' dff %>%add_rows_freq(sel="fre")
#' dff %>%add_rows_freq(sel=3)
#' dff %>%add_rows_freq(sel="num")
#' dff %>%add_rows_freq(fix=1)
#' dff %>%add_rows_freq(type= "res", fix=1:2)
#' dff %>%add_rows_freq(type= "res", fix="con")
#' dff %>%add_rows_freq(type= "all")
#' dff %>%add_rows_freq(type= "real")
#' dff %>%add_rows_freq(type= "zero")
#' dff %>%add_rows_freq(type= "zero", fix="con")
#'
#'
#' # Making frequency data into real data
#' dff %>%to_long()%>% add_rows_freq()
#'  dff_real =  dff %>%to_long()%>% add_rows_freq()
#'  dff_real
#'
#'
#'  freq_data =  mtcars %>% select(am, vs) %>% table()
#'  freq_data%>% data.frame()
#'  freq_data%>% data.frame() %>% add_rows_freq(type ="zero")
#'  freq_data%>% data.frame() %>% add_rows_freq(type ="zero") %>%table()
#'
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
#' dff %>%unCount()
#' dff %>%unCount()
#' dff %>%unCount(fix=1)
#' dff %>%unCount(type= "res", fix=1:2)
#' dff %>%unCount(type= "res", fix="con")
#' dff %>%unCount(type= "all")
#' dff %>%unCount(type= "real")
#' dff %>%unCount(type= "zero")
#' dff %>%unCount(type= "zero", fix="con")
#'
#' # Making frequency data into real data
#' dff %>%unCount()%>% add_rows_freq()
#'  dff_real =  dff %>%unCount()%>% add_rows_freq()
#'  dff_real
#'
#'  freq_data =  mtcars %>% select(am, vs) %>% table()
#'  freq_data%>% data.frame()
#'  freq_data%>% data.frame() %>% unCount(type ="zero")
#'  freq_data%>% data.frame() %>% unCount(type ="zero") %>%table()
#'
#' }
#'

unCount <- function(data,sel = ncol(data),
                          type = "res",
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

