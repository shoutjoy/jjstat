#' add_pca_scores
#'
#' @param data data
#' @param ... select variable
#'
#' @return  add data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' data(offense)
#'
#' nfl_path = matrix(
#'   c(0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 1, 1, 0, 0,
#'     1, 0, 0, 1, 0),
#'   nrow = 5, ncol = 5, byrow = TRUE )
#'
#' rownames(nfl_path) <- c("Speical", "Rushing", "Passing", "Offense", "Scoring")
#' colnames(nfl_path) <- c("Speical", "Rushing", "Passing", "Offense", "Scoring")
#' nfl_path
#' Offense = offense %>% add_pca_scores(rush1 = c(1:3), pass1 = c(4:6))
#'
#'Offense =  add_pca_scores(offense,
#'                rush1 = c("YardsRushAtt", "RushYards", "RushFirstDown"),
#'                pass1 = c("YardsPassComp", "PassYards", "PassFirstDown"))
#'
#'
#' nfl_blocks2 = list(7:8, 1:3, 4:6,  18:19, 9:11)
#'
#' plspm_blocks_match(data=Offense, blocks=nfl_blocks2,
#'                    paths=nfl_path)
#'
#' plspm_blocks_match(data=Offense, blocks=nfl_blocks2,
#'                    paths=nfl_path)%>%make_list_text()
#'
#' nfl_blocks2 = list(
#'   Speical = c("FieldGoals", "OtherTDs"),
#'   Rushing = c("YardsRushAtt", "RushYards", "RushFirstDown"),
#'   Passing = c("YardsPassComp", "PassYards", "PassFirstDown"),
#'   Offense = c("rush1", "pass1"),
#'   Scoring = c("PointsGame", "OffensTD", "TDGame")
#' )
#' nfl_blocks2
#' nfl_mode2 =c("B","A","A","A","A")
#' nfl_pls2 = plspm(Data=Offense, nfl_path, nfl_blocks2, modes = nfl_mode2)
#' nfl_pls2%>%summary()
#'
#' nfl_pls_boot2 = plspm_sem(Data=Offense, nfl_path, nfl_blocks2, modes = nfl_mode)
#' x11()
#' nfl_pls_boot2 %>%plspm_path_coefs_plot()
#' nfl_pls_boot2 %>%plspm_semPaths2()
#'
#'
#' }
#'
add_pca_scores <- function(data, ...) {
  args <- list(...)
  for (name in names(args)) {
    cols <- args[[name]]
    if (is.character(cols)) {
      selected_cols <- data[, cols, drop = FALSE]
    } else if (is.numeric(cols)) {
      selected_cols <- data[, cols]
    } else {
      stop("Columns should be specified as either numeric indices or character names.")
    }
    pca_result <- plsdepot::nipals(selected_cols)
    data[[name]] <- pca_result$scores[, 1]
  }
  return(data)
}
