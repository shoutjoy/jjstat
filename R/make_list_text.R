#' make_list_text
#'
#' @param data list data
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #
#'
#' # Example data (characters)
#' data1 <- list(
#'   Speical = c("FieldGoals", "OtherTDs"),
#'   Rushing = c("YardsRushAtt", "RushYards", "RushFirstDown"),
#'   Passing = c("YardsPassComp", "PassYards", "PassFirstDown"),
#'   Offense = c("rush1", "pass1"),
#'   Scoring = c("PointsGame", "OffensTD", "TDGame")
#' )
#' make_list_text(data1)
#'
#' # Example data (numeric)
#' data2 <- list(
#'   Speical = 7:8,
#'   Rushing = 1:3,
#'   Passing = 4:6,
#'   Offense = 18:19,
#'   Scoring = 9:11
#' )
#' make_list_text(data2)
#'
#'
#'
#' # Example data (numeric)
#' list(7:8, 1:3, 4:6, 18:19, 9:11)%>%make_list_text()
#' #
#' #
#' }
#'
#'
make_list_text <- function(data) {
  # 이름이 없는 리스트에 이름 부여
  if (is.null(names(data))) {
    names(data) <- paste0("V", seq_along(data))
  }

  result <- lapply(names(data), function(key) {
    values <- data[[key]]
    if (is.numeric(values)) {
      items <- paste(values, collapse = ', ')
    } else {
      items <- paste0('"', values, '"', collapse = ', ')
    }
    paste0(key, ' = c(', items, ')')
  })

  result_text <- paste(result, collapse = ',\n  ')

  # Newlist
  newlist_output <- paste0('Newlist = list(\n  ', result_text, '\n)')

  # Output in original list form
  original_output <- lapply(names(data), function(key) {
    values <- data[[key]]
    if (is.numeric(values)) {
      items <- paste(values, collapse = ' ')
    } else {
      items <- paste0('"', values, '"', collapse = ' ')
    }
    paste0('$', key, '\n[1] ', items)
  })

  original_output_text <- paste(original_output, collapse = '\n\n')

  # 최종 출력
  cat(newlist_output, "\n\n", original_output_text)
}
