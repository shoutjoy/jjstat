#' Functions that output to a web browser
#'
#' @param data data
#' @param digits default 3
#' @param font_size default 14
#' @param file file name default "output.html
#' @param out TRUE browser
#' @param tibble show data
#'
#' @return web output
#' @export
#'
#' @examples
#'
#' \dontrun{
#' mtcars%>%web()
#'
#' }
#'
web <- function(data, out=TRUE, tibble=TRUE,  digits = 3, font_size = 14, file = "output.html") {
  # library(htmlTable)
  # library(htmltools)

  # 데이터프레임이나 행렬인지 확인
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("data must be a data frame or matrix")
  }

  # Rounding dataframes
  data <- jjstat::Round(data, digits)

  # Convert a dataframe to an HTML table
  html_table <- htmlTable::htmlTable(data)

  # Adding CSS classes to apply styles
  style <- sprintf("
    <style>
      .custom-table table, .custom-table th, .custom-table td {
        font-size: %dpx;
      }
    </style>
  ", font_size)

  # Generate HTML file content
  html_content <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$title("Data Table Output"),
      htmltools::HTML(style)
    ),
    htmltools::tags$body(
      htmltools::div(class = "custom-table", htmltools::HTML(html_table))
    )
  )


  # Create a temporary HTML file
  html_file <- tempfile(fileext = ".html")
  writeLines(as.character(html_content), html_file)

  if(out){
    # Open an HTML file with a web browser
    browseURL(html_file)
  }else{
    if(tibble){
    tibble::tibble(data)

    }else{
    data.frame(data)

    }
  }

}
