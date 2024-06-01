#' Extracting the layout matrix of a plspm model
#'
#' @param plspm_model sempaths
#'
#' @return mat
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data(offense)
#' offense %>%str()
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
#'
#' nfl_blocks = list(7:8, 1:3, 4:6,1:6, 9:11)
#'
#' plspm_blocks_match(data=offense, blocks=nfl_blocks,
#'                    paths=nfl_path)
#'
#' nfl_mode =c("B","A","A","A","A")
#'
#' nfl_pls = plspm(Data=offense, nfl_path, nfl_blocks, modes = nfl_mode)
#' nfl_pls%>%summary()
#'
#' #########
#' nfl_pls_boot = plspm_sem(Data=offense, nfl_path, nfl_blocks, modes = nfl_mode)
#'
#' #Layouts extracted from mockups
#' plspm_semPaths2(nfl_pls_boot,sizeLat = 6)%>%
#'   plspm_factor_layout()
#'
#' #Make it input material
#' plspm_semPaths2(nfl_pls_boot,sizeLat =6)%>%
#'   plspm_factor_layout() %>%layout_mat(text=T)
#'
#' #Organizing materials for input
#' plspm_semPaths2(nfl_pls_boot,sizeLat =6)%>%
#'   plspm_factor_layout() %>%layout_mat()%>%
#'   mat_text_arrange()
#' }
#'
#'
#'
plspm_factor_layout <- function(plspm_model) {
  # Generate the SEM path diagram using plspm_semPaths2
  semPaths_data <- plspm_model
  #colnames
  rowName =semPaths_data$graphAttributes$Nodes$labels

  # Extract the layout matrix
  layout_matrix <- semPaths_data$layout
  rownames(layout_matrix)= rowName
  return(data.frame(layout_matrix))
}


#' Extracting the layout matrix of a plspm model
#' @param plspm_model sempaths
#'
#' @return mat
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data(offense)
#' offense %>%str()
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
#'
#' nfl_blocks = list(7:8, 1:3, 4:6,1:6, 9:11)
#'
#' plspm_blocks_match(data=offense, blocks=nfl_blocks,
#'                    paths=nfl_path)
#'
#' nfl_mode =c("B","A","A","A","A")
#'
#' nfl_pls = plspm(Data=offense, nfl_path, nfl_blocks, modes = nfl_mode)
#' nfl_pls%>%summary()
#'
#' #########
#' nfl_pls_boot = plspm_sem(Data=offense, nfl_path, nfl_blocks, modes = nfl_mode)
#'
#' #Layouts extracted from mockups
#' plspm_semPaths2(nfl_pls_boot,sizeLat = 6)%>%
#'   plspm_factor_layout()
#'
#' #Make it input material
#' plspm_semPaths2(nfl_pls_boot,sizeLat =6)%>%
#'   plspm_factor_layout() %>%layout_mat(text=T)
#'
#' #Organizing materials for input
#' plspm_semPaths2(nfl_pls_boot,sizeLat =6)%>%
#'   plspm_factor_layout() %>%layout_mat()%>%
#'   mat_text_arrange()
#' }
#'
#'
#'
factor_layout <- function(plspm_model) {
  # Generate the SEM path diagram using plspm_semPaths2
  semPaths_data <- plspm_model
  #colnames
  rowName =semPaths_data$graphAttributes$Nodes$labels

  # Extract the layout matrix
  layout_matrix <- semPaths_data$layout
  rownames(layout_matrix)= rowName
  return(data.frame(layout_matrix))
}

