#' Showing significance using PLSPM regression
#'
#' @param data plspm data
#' @param unite TRUE
#' @param rhs from
#' @param lhs to
#' @param digits round 3
#'
#' @return data result
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(plspm)
#' data(satisfaction)
#'
#' # path matrix
#' IMAG = c(0,0,0,0,0,0)
#' EXPE = c(1,0,0,0,0,0)
#' QUAL = c(0,1,0,0,0,0)
#' VAL = c(0,1,1,0,0,0)
#' SAT = c(1,1,1,1,0,0)
#' LOY = c(1,0,0,0,1,0)
#' sat_path = rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)
#'
#' # blocks of outer model
#' sat_blocks = list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)
#' sat_mod=c(rep("A", ncol(sat_path)))
#' # apply plspm
#' satpls = plspm(satisfaction, sat_path, sat_blocks, modes = sat_mod,
#'                scaled = FALSE)
#'
#' #default
#' satpls%>%plspm_paths_sig()
#' satpls$inner_model %>% plspm_paths_sig()
#'
#' satpls %>%plspm_paths_sig(unite=FALSE, lhs="row", rhs="col")
#' satpls$inner_model  %>%innermodel_sig(unite=FALSE, lhs="row", rhs="col")
#'
#' satpls %>%plspm_paths_sig(unite=FALSE, lhs="row", rhs="col")
#' satpls$inner_model %>% plspm_paths_sig(unite=FALSE, lhs="row", rhs="col")
#'
#' #'
#' }
#'
plspm_paths_sig <- function(data, unite=FALSE,
                            rhs ="endogenous", lhs ="exogenous",digits=3) {
  # Check if data inherits from 'plspm'
  if (inherits(data, "plspm")) {
    data <- data[["inner_model"]]
  }

  # Create an empty dataframe
  df <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(df) <- c("endo", "exo", "Estimate", "Std. Error", "t value", "Pr(>|t|)")

  # Iterate over the list and add rows to the dataframe
  for (i in seq_along(data)) {
    # Extract values from each list element
    endo <- names(data)[i]
    exo <- rownames(data[[i]])
    values <- data[[i]]

    # Combine endo and exo into a dataframe
    new_df <- data.frame(endo = rep(endo, length(exo)),
                         exo = exo,
                         Estimate = values[, "Estimate"],
                         `Std. Error` = values[, "Std. Error"],
                         `t value` = values[, "t value"],
                         `Pr(>|t|)` = values[, "Pr(>|t|)"])

    # Append the new dataframe to the main dataframe
    df <- rbind(df, new_df)%>%Round(digits)
  }

  colnames(df) <- c("endo", "exo", "Est", "SE", "t", "p.value")

  if(unite){
    df = df%>% dplyr::filter(exo !="Intercept") %>%
      p_mark_sig(unite=TRUE, unite_col="Est", ns="")%>%
      tidyr::unite(Path, exo, endo, sep=" -> ") %>%
      dplyr::mutate(p.value = ifelse(p.value< 0.001, "<.001",p.value))

  }else{
    df = df%>% dplyr::filter(exo !="Intercept") %>%
      p_mark_sig(unite=FALSE, unite_col="Est")%>%
      # unite(Path, exo, endo, sep=" -> ") %>%
      dplyr::mutate(p.value = ifelse(p.value< 0.001, "<.001", round(p.value, digits )))

    df = df %>%dplyr::rename(!!rhs := endo, !!lhs := exo) %>%data.frame()
    # colnames(df)[1] <- endo
    # colnames(df)[2] <- exo
  }
  return(df)
}

#' Showing significance using PLSPM regression
#'
#' @param data plspm data
#' @param unite TRUE
#' @param rhs from
#' @param lhs to
#' @param digits round 3
#'
#' @return data result
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(plspm)
#' data(satisfaction)
#'
#' # path matrix
#' IMAG = c(0,0,0,0,0,0)
#' EXPE = c(1,0,0,0,0,0)
#' QUAL = c(0,1,0,0,0,0)
#' VAL = c(0,1,1,0,0,0)
#' SAT = c(1,1,1,1,0,0)
#' LOY = c(1,0,0,0,1,0)
#' sat_path = rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)
#'
#' # blocks of outer model
#' sat_blocks = list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)
#' sat_mod=c(rep("A", ncol(sat_path)))
#' # apply plspm
#' satpls = plspm(satisfaction, sat_path, sat_blocks, modes = sat_mod,
#'                scaled = FALSE)
#'
#' #default
#' satpls%>%plspm_paths_sig()
#' satpls$inner_model %>%plspm_paths_sig()
#'
#' satpls %>%plspm_paths_sig(unite=FALSE, lhs="row", rhs="col")
#' satpls$inner_model  %>%innermodel_sig(unite=FALSE, lhs="row", rhs="col")
#'
#' satpls %>%plspm_paths_sig(unite=FALSE, lhs="row", rhs="col")
#' satpls$inner_model %>%plspm_paths_sig(unite=FALSE, lhs="row", rhs="col")
#'
#' #'
#' }
#'
plspm_inner_model_sig <- function(data, unite=FALSE,
                                    rhs ="endogenous", lhs ="exogenous",
                                    digits=3) {
  # Check if data inherits from 'plspm'
  if (inherits(data, "plspm")) {
    data <- data[["inner_model"]]
  }

  # Create an empty dataframe
  df <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(df) <- c("endo", "exo", "Estimate", "Std. Error", "t value", "Pr(>|t|)")

  # Iterate over the list and add rows to the dataframe
  for (i in seq_along(data)) {
    # Extract values from each list element
    endo <- names(data)[i]
    exo <- rownames(data[[i]])
    values <- data[[i]]

    # Combine endo and exo into a dataframe
    new_df <- data.frame(endo = rep(endo, length(exo)),
                         exo = exo,
                         Estimate = values[, "Estimate"],
                         `Std. Error` = values[, "Std. Error"],
                         `t value` = values[, "t value"],
                         `Pr(>|t|)` = values[, "Pr(>|t|)"])

    # Append the new dataframe to the main dataframe
    df <- rbind(df, new_df)%>%Round(digits)
  }

  colnames(df) <- c("endo", "exo", "Est", "SE", "t", "p.value")

  if(unite){
    df = df%>% dplyr::filter(exo !="Intercept") %>%
      p_mark_sig(unite=TRUE, unite_col="Est", ns="")%>%
      tidyr::unite(Path, exo, endo, sep=" -> ") %>%
      dplyr::mutate(p.value = ifelse(p.value< 0.001, "<.001",p.value))

  }else{
    df = df%>% dplyr::filter(exo !="Intercept") %>%
      p_mark_sig(unite=FALSE, unite_col="Est")%>%
      # unite(Path, exo, endo, sep=" -> ") %>%
      dplyr::mutate(p.value = ifelse(p.value< 0.001, "<.001", round(p.value, digits )))

    df = df %>%dplyr::rename(!!rhs := endo, !!lhs := exo) %>%data.frame()
    # colnames(df)[1] <- endo
    # colnames(df)[2] <- exo
  }
  return(df)
}
