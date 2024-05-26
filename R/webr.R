#' data to web
#'
#' @param data data.frame
#' @param digits round 3
#' @param font_size 14
#' @param border_width 1
#' @param border_color "gray"
#' @param file file = "output.html"
#' @param show TRUE: web FALSE" console
#'
#' @return web
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Example usage
#' webr(mtcars)
#' webr(mtcars, font_size=20)
#' }
#'
webr <- function(data,show= TRUE, digits = 3, font_size = 18, border_width = 1, border_color = "#ccc", file = "output.html") {
  # 데이터프레임 확인
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # 데이터 타입에 따라 처리
  for (i in 1:ncol(data)) {
    if (is.numeric(data[[i]])) {
      data[[i]] <- round(data[[i]], digits)
    }
  }

  # HTML 테이블 생성
  html_rows <- lapply(1:nrow(data), function(i) {
    paste0("<tr><td>", paste(data[i, ], collapse="</td><td>"), "</td></tr>")
  })
  html_table <- paste0(
    "<table style='border-collapse: collapse; border: ", border_width, "px solid ", border_color, ";'>",
    "<thead style='position: sticky; top: 0; background-color: white;'>",
    "<tr><th>", paste(names(data), collapse="</th><th>"), "</th></tr>",
    "</thead>",
    "<tbody>",
    paste(html_rows, collapse="\n"),
    "</tbody>",
    "</table>"
  )

  # CSS 스타일 적용
  style <- sprintf("
    <style>
      table, th, td {
        font-size: %dpx;
        padding: 4px;
      }
    </style>
  ", font_size)

  # HTML 파일 생성
  html_content <- paste0(
    "<html><head><title>Data Table Output</title>",
    style,
    "</head><body>",
    html_table,
    "</body></html>"
  )

  # 임시 HTML 파일 생성 및 열기
  temp_file <- tempfile(fileext = ".html")
  writeLines(html_content, temp_file)

if(show){
  browseURL(temp_file)
}else{
    data
  }


}
