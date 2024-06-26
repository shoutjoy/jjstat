#' exceldata to dataframe
#'
#' @param type "clipboard" to table data. dataframe, tibble
#' @param x "clipboard"
#' @param header  header=FALSE
#' @return paste data.frame
#' @export
#
#' @examples
#' #first you must copy data (Ctrl+C)
#' ## then excute below ctrl+Enter
#' df = datapaste()
#' df = datapaste("tibble")
#' df = datapaste("table")
#' df = datapaste("data.frame")
#'
#'
#' datapaste("datapasta")
#' New = data.frame(row.names = c("1", "2", "3", "4", "5", "6"),
#'                    응답자 = c("응답1", "응답2", "응답3", "응답4", "응답5", "응답6"),
#'                    role_v_inst1 = c(5, 5, 5, 5, 4, 5),
#'                    role_v_inst2 = c(5, 5, 5, 5, 5, 5),
#'                    role_v_inst3 = c(4, 4, 5, 5, 4, 4),
#'                    role_v_inst4 = c(5, 4, 5, 5, 3, 4))
#'
#'
datapaste <- function(type="matrix", x="clipboard", header=TRUE){

table = read.table(file = x, sep = "\t", header = header)
data.frame = read.table(file = x, sep = "\t", header = header) %>% data.frame()
tibble = read.table(file = x, sep = "\t", header = header) %>%  tibble::tibble()
matrix = read.table(file = x, sep = "\t", header = header) %>%  as.matrix()

switch(type,
       table = table,
       data.frame = data.frame,
       tibble =  tibble,
       matrix = matrix,
       df = make_df_text(data.frame),
       mat = make_mat_text(data.frame),

       )

}
