
#' plspm_semptools
#'
#' @param semPaths_plot plot data
#' @param pls_boot boot data
#' @param position layout  position
#' @param point point position
#' @param set_push each push
#' @param set_spread each spread
#' @param nrow default 3
#' @param ncol default 3
#' @param loading_position 0.5
#'
#' @return  plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' #########################################################
#' pathmodel = "
#' 자기효능감 =~ C_S1 +  C_S2 + C_S3 + C_S4 + C_S5
#' 진로태도 =~ B02 + B04 + B10 + B14
#' 진로동기 =~ A_M1 + A_M2 + A_M3
#' 진로준비 =~ D_P1 + D_P2 + D_P3
#'
#' 진로동기 ~ H1*자기효능감
#' 진로태도 ~  H2*자기효능감
#' 진로준비 ~ H5*진로태도 + H4*진로동기 + H3*자기효능감
#' "
#' pathmodel
#'
#' plspm_semPaths2(jutpls_boot,   curve=1, layout="tree2",rotation=1,
#'                 sizeMan= 10, color=list(lat="gray10",man="gray98"),
#'                 edge.label.cex=0.7, mar=c(5,5,5,5),
#'                 edge.label.position=0.55,
#'                 sizeLat= 8)%>%
#'   semptools::set_sem_layout(
#'     indicator_order =  plspm_indicator_order(pathmodel),
#'     indicator_factor =  plspm_indicator_factor(pathmodel),
#'     factor_layout=  matrix(
#'       c(  NA, '진로태도', NA,
#'           '자기효능감', NA, '진로준비',
#'           NA, '진로동기', NA),
#'       nrow = 3, ncol = 3, byrow = TRUE),
#'     factor_point_to= matrix(
#'       c(  NA, 'up', NA,
#'           'left', NA, 'right',
#'           NA, 'down', NA),
#'       nrow = 3, ncol = 3, byrow = TRUE),
#'     indicator_push <- c(진로태도 = 2, 진로동기 = 2, 자기효능감=1.5, 진로준비 =1.5),
#'     indicator_spread <- c(자기효능감 = 3, 진로태도 = 2, 진로동기 =2, 진로준비 = 2)
#'   )%>%  semptools::set_curve(c("진로준비~~ 진로태도" = 0.5))%>%
#'   semptools::set_curve(c("진로준비~~ 진로동기" = 0))%>%
#'   plspm_change_nodeLabels("B02","진로결정",
#'                           "B04", "진로독립",
#'                           "B10", "진로계획",
#'                           "B14", "진로성향",
#'                           "A_M1","진로정체",
#'                           "A_M2","진로통찰",
#'                           "A_M3","진로탄력",
#'                           "C_S1","자기평가",
#'                           "C_S2","직업정보",
#'                           "C_S3","계획수립",
#'                           "C_S4","목표설정",
#'                           "C_S5","해결방식",
#'                           "D_P1","진로준비",
#'                           "D_P2","도구준비",
#'                           "D_P3","목표달성"
#'   )
#' # second ##################################
#' plspm_indicator_order(pathmodel)
#' plspm_indicator_factor(pathmodel)
#'
#'
#' #옵션함수
#' plspm_boot_ind_order(jutpls_boot)
#' plspm_boot_ind_factor(jutpls_boot)
#'
#' plspm_boot_factor_layout(jutpls_boot)
#' plspm_boot_factor_point_to(jutpls_boot, ncol=3)
#'
#' plspm_ind_push(jutpls_boot)
#' plspm_ind_spread(jutpls_boot, set= c(자기효능감=4,진로태도=3, 진로동기=3))
#'
#' ##plot
#' plspm_semPaths2(jutpls_boot,   curve=1, layout="tree2",rotation=1,
#'                 sizeMan= 10, color=list(lat="gray10",man="gray98"),
#'                 edge.label.cex=0.7, mar=c(5,5,5,5),
#'                 edge.label.position=0.55, edge.color= "tomato4",
#'                 sizeLat= 8)%>%
#'   semptools::set_sem_layout(
#'     indicator_order = plspm_boot_ind_order(jutpls_boot),
#'     indicator_factor = plspm_boot_ind_factor(jutpls_boot),
#'     factor_layout= plspm_boot_factor_layout(jutpls_boot, ncol=3),
#'     factor_point_to= plspm_boot_factor_point_to(jutpls_boot, ncol=3),
#'     indicator_push <- plspm_ind_push(jutpls_boot),
#'     indicator_spread <-  plspm_ind_spread(jutpls_boot, set= c(자기효능감=4,진로태도=3, 진로동기=3)))%>%
#'   plspm_curve(c("진로준비~ 진로동기" =0))%>%
#'   plspm_change_nodeLabels("B02","진로결정",
#'                           "B04", "진로독립",
#'                           "B10", "진로계획",
#'                           "B14", "진로성향",
#'                           "A_M1","진로정체",
#'                           "A_M2","진로통찰",
#'                           "A_M3","진로탄력",
#'                           "C_S1","자기평가",
#'                           "C_S2","직업정보",
#'                           "C_S3","계획수립",
#'                           "C_S4","목표설정",
#'                           "C_S5","해결방식",
#'                           "D_P1","진로준비",
#'                           "D_P2","도구준비",
#'                           "D_P3","목표달성"
#'   )
#'
#' #'
#' #' #we want #########################################
#' plspm_semPaths2(jutpls_boot,   curve=1, layout="tree2",rotation=1,
#'                 sizeMan= 10, color=list(lat="gray10",man="gray98"),
#'                 edge.label.cex=0.7, mar=c(5,5,5,5),
#'                 edge.label.position=0.55, edge.color= "tomato4",
#'                 sizeLat= 8) %>%
#'   plspm_semptools(jutpls_boot)%>%
#'   plspm_curve(c("진로준비~ 진로동기" =0))%>%
#'   plspm_change_nodeLabels("B02","진로결정",
#'                           "B04", "진로독립",
#'                           "B10", "진로계획",
#'                           "B14", "진로성향",
#'                           "A_M1","진로정체",
#'                           "A_M2","진로통찰",
#'                           "A_M3","진로탄력",
#'                           "C_S1","자기평가",
#'                           "C_S2","직업정보",
#'                           "C_S3","계획수립",
#'                           "C_S4","목표설정",
#'                           "C_S5","해결방식",
#'                           "D_P1","진로준비",
#'                           "D_P2","도구준비",
#'                           "D_P3","목표달성"
#'   )
#' #'
#'
#'
#' }
#'
plspm_semptools= function(semPaths_plot,
                          pls_boot,
                          position= NULL,
                          point= NULL,
                          set_push =2.5,
                          set_spread=2.5,
                          nrow= 3,  ncol= 3,
                          loading_position = 0.5){

  pplot = semptools::set_sem_layout(semPaths_plot = semPaths_plot,
          indicator_order = plspm_boot_ind_order(pls_boot),
            indicator_factor = plspm_boot_ind_factor(pls_boot),
            factor_layout = plspm_boot_factor_layout(pls_boot,
                                                     position= position,
                                                     nrow=nrow,
                                                     ncol=ncol),
             factor_point_to = plspm_boot_factor_point_to(pls_boot,
                                                          point= point,
                                                          nrow=nrow,
                                                          ncol=ncol),
    indicator_push = plspm_ind_push(pls_boot, set= set_push),
      indicator_spread = plspm_ind_spread(pls_boot,
                                          set= set_spread),
                                    loading_position = loading_position)

  plot(pplot)
  return(pplot)
}





#' plspm_indicator_order
#'
#' @param model_syntax  lavaan
#'
#' @return text
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' mod <-
#'   'f1 =~ x01 + x02 + x03 + x06
#'     f3 =~ x08 + x09 + x10 + x03
#'     f2 =~ x04 + x05 + x06 + x07
#'     f4 =~ x11 + x12 + x13 + x14
#'     f3 ~ f1 + f2
#'     f4 ~ f3
#'    '
#' plspm_indicator_order(mod)
#'
#' plspm_indicator_factor(mod)
#' }
#'
plspm_indicator_order <- function(model_syntax) {
  ptable <- lavaan::lavParseModelString(model_syntax,
                                        as.data.frame. = TRUE)
  ptable2 <- ptable[ptable$op == "=~", ]
  ptable2 <- ptable2[!duplicated(ptable2$rhs), ]
  if (nrow(ptable2) == 0) {
    stop("No factor loadings found.")
  }
  out <- ptable2$rhs
  names(out) <- ptable2$lhs
  out
}

#' plspm_indicator_factor
#'
#' @param model_syntax lavaan
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' mod <-
#'   'f1 =~ x01 + x02 + x03 + x06
#'     f3 =~ x08 + x09 + x10 + x03
#'     f2 =~ x04 + x05 + x06 + x07
#'     f4 =~ x11 + x12 + x13 + x14
#'     f3 ~ f1 + f2
#'     f4 ~ f3
#'    '
#' plspm_indicator_order(mod)
#'
#' plspm_indicator_factor(mod)
#' }
#'
plspm_indicator_factor <- function(model_syntax) {
  ptable <- lavaan::lavParseModelString(model_syntax,
                                        as.data.frame. = TRUE)
  ptable2 <- ptable[ptable$op == "=~", ]
  ptable2 <- ptable2[!duplicated(ptable2$rhs), ]
  if (nrow(ptable2) == 0) {
    stop("No factor loadings found.")
  }
  out <- ptable2$rhs
  names(out) <- ptable2$lhs
  res= names(out)
  res
}




#' plspm_indicator_order
#'
#' @param model_syntax  lavaan
#'
#' @return text
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' mod <-
#'   'f1 =~ x01 + x02 + x03 + x06
#'     f3 =~ x08 + x09 + x10 + x03
#'     f2 =~ x04 + x05 + x06 + x07
#'     f4 =~ x11 + x12 + x13 + x14
#'     f3 ~ f1 + f2
#'     f4 ~ f3
#'    '
#' plspm_indicator_order(mod)
#'
#' plspm_indicator_factor(mod)
#' }
#'
plspm_ind_order <- function(model_syntax) {
  ptable <- lavaan::lavParseModelString(model_syntax,
                                        as.data.frame. = TRUE)
  ptable2 <- ptable[ptable$op == "=~", ]
  ptable2 <- ptable2[!duplicated(ptable2$rhs), ]
  if (nrow(ptable2) == 0) {
    stop("No factor loadings found.")
  }
  out <- ptable2$rhs
  names(out) <- ptable2$lhs
  out
}

#' plspm_indicator_factor
#'
#' @param model_syntax lavaan
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' mod <-
#'   'f1 =~ x01 + x02 + x03 + x06
#'     f3 =~ x08 + x09 + x10 + x03
#'     f2 =~ x04 + x05 + x06 + x07
#'     f4 =~ x11 + x12 + x13 + x14
#'     f3 ~ f1 + f2
#'     f4 ~ f3
#'    '
#' plspm_indicator_order(mod)
#'
#' plspm_indicator_factor(mod)
#' }
#'
plspm_ind_factor <- function(model_syntax) {
  ptable <- lavaan::lavParseModelString(model_syntax,
                                        as.data.frame. = TRUE)
  ptable2 <- ptable[ptable$op == "=~", ]
  ptable2 <- ptable2[!duplicated(ptable2$rhs), ]
  if (nrow(ptable2) == 0) {
    stop("No factor loadings found.")
  }
  out <- ptable2$rhs
  names(out) <- ptable2$lhs
  res= names(out)
  res
}





#' plspm_ind_order2
#'
#' @param model_syntax lavaan
#'
#' @return input text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' mod <-
#'   'f1 =~ x01 + x02 + x03 + x06
#'     f3 =~ x08 + x09 + x10 + x03
#'     f2 =~ x04 + x05 + x06 + x07
#'     f4 =~ x11 + x12 + x13 + x14
#'     f3 ~ f1 + f2
#'     f4 ~ f3
#'    '
#' plspm_ind_order2(mod)
#'
#' }
#'
plspm_ind_order2 <- function(model_syntax) {
  # Parse the model string into a parameter table
  ptable <- lavaan::lavParseModelString(model_syntax, as.data.frame. = TRUE)

  # Filter the table for latent variable definitions (op == "=~")
  ptable2 <- ptable[ptable$op == "=~", ]

  # Ensure there are no duplicate indicators
  ptable2 <- ptable2[!duplicated(ptable2$rhs), ]

  # Check if any factor loadings are found
  if (nrow(ptable2) == 0) {
    stop("No factor loadings found.")
  }

  # Extract indicators and latent variable names
  indicators <- ptable2$rhs
  latent_vars <- ptable2$lhs

  # Create a named vector in the order they appear in the model syntax
  unique_latent_vars <- unique(latent_vars)
  ordered_result <- unlist(lapply(unique_latent_vars, function(lv) {
    inds <- indicators[latent_vars == lv]
    setNames(inds, rep(lv, length(inds)))
  }))

  # Create the output string using cat
  cat('ind_order = c(',
      paste(paste0(names(ordered_result), ' = "', ordered_result, '"'), collapse = ', '),
      ')', sep = '\n')
}





#' plspm_indicator_order
#'
#' @param plsres_boot  plsres_boot
#'
#' @return plsres_boot
#' @export
#'
#' @examples
#' \dontrun{
#' #'
# # Example usage with provided data structure
# plsres_boot <- list(
#   model = list(
#     blocks = list(
#       자기효능감 = c(111, 112, 113, 114, 115),
#       진로동기 = c(105, 106, 107),
#       진로태도 = c(17, 19, 25, 29),
#       진로준비 = c(116, 117, 118)
#     )
#   )
# )
#
# # Call the function
# result <- plspm_boot_ind_factor(plsres_boot)
# print(result)
#' }
#'
plspm_boot_ind_order = function(plsres_boot){
  # Extracting block names and mvs_names
  block_names <- names(plsres_boot$model$blocks)
  mvs_names <- plsres_boot$model$gens$mvs_names

  # Initializing result vectors
  block_result <- c()
  mvs_result <- c()

  # Initialize index for mvs_names
  mvs_index <- 1

  # Loop through each block
  for (block in block_names) {
    # Get the number of elements in the current block
    num_elements <- length(plsres_boot$model$blocks[[block]])

    # Repeat the block name 'num_elements' times and append to block_result
    block_result <- c(block_result, rep(block, num_elements))

    # Append the corresponding mvs_names to mvs_result
    mvs_result <- c(mvs_result, mvs_names[mvs_index:(mvs_index + num_elements - 1)])

    # Update the mvs_index
    mvs_index <- mvs_index + num_elements
  }

  # Combine block_result and mvs_result into a named vector
  names(mvs_result) <- block_result

  return(mvs_result)
}




#' plspm_indicator_factor
#'
#' @param plsres_boot  plsres_boot
#'
#' @return plsres_boot
#' @export
#'
#' @examples
#' \dontrun{
#' plspm_indicator_factor(nfl_pls_boot)
#' }
plspm_boot_ind_factor = function(plsres_boot){
  # Extracting block names
  block_names <- names(plsres_boot$model$blocks)

  # Initializing result vector
  res <- c()

  # Loop through each block
  for (block in block_names) {
    # Get the number of elements in the current block
    num_elements <- length(plsres_boot$model$blocks[[block]])

    # Repeat the block name 'num_elements' times and append to result vector
    res <- c(res, rep(block, num_elements))
  }

  return(res)
}




#' plspm_ind_push
#'
#' @param pls_boot pls_boot
#' @param set  set=1.5 or c(set= c(2,2,1.5,1.5))
#'
#' @return vector
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # 예시 사용법
#' jutpls_boot <- list(model = list(gens = list(lvs_names = c("자기효능감", "진로동기", "진로태도", "진로준비"))))
#'
#' # 기본값 사용하는 경우
#' result_default <- plspm_ind_push(jutpls_boot)
#' print(result_default)
#'
#' # 이름과 값이 있는 경우
#' result_named <- plspm_ind_push(jutpls_boot, set = c(진로동기=2, 자기효능감=3, 진로준비=1.5, 진로태도=2.5))
#' print(result_named)
#'
#' # 값만 있는 경우
#' result_unnamed <- plspm_ind_push(jutpls_boot, set = c(2, 2, 1.5, 1.5))
#' print(result_unnamed)
#'
#' # 부분적으로 값이 있는 경우
#' result_partial <- plspm_ind_push(jutpls_boot, set = c(진로동기=2, 자기효능감=3))
#' print(result_partial)
#'
#' }
#'
#'
#'
plspm_ind_push <- function(pls_boot, set = 2) {
  # 이름 추출
  lvs_names <- pls_boot$model$gens$lvs_names

  # 기본값 설정
  values <- rep(1.5, length(lvs_names))
  names(values) <- lvs_names

  if (is.null(names(set))) {
    # set이 이름 없는 벡터일 경우
    if (length(set) == 1) {
      values <- rep(set, length(lvs_names))
    } else if (length(set) != length(lvs_names)) {
      stop("Length of 'set' must match the length of 'lvs_names'")
    } else {
      values <- set
    }
  } else {
    # set이 이름 있는 벡터일 경우
    for (name in names(set)) {
      if (name %in% lvs_names) {
        values[name] <- set[name]
      }
    }
  }

  # 결과 데이터프레임 생성
  result <- setNames(as.data.frame(t(values)), lvs_names)

  return(unlist(result))
}



#' plspm_ind_push
#'
#' @param pls_boot pls_boot
#' @param set  set=1.5 or c(set= c(2,2,1.5,1.5))
#'
#' @return vector
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시 사용법
#' jutpls_boot <- list(model = list(gens = list(lvs_names = c("자기효능감", "진로동기", "진로태도", "진로준비"))))
#'
#' # 기본값 사용하는 경우
#' result_default <- plspm_ind_spread(jutpls_boot)
#' print(result_default)
#'
#' # 이름과 값이 있는 경우
#' result_named <- plspm_ind_spread(jutpls_boot, set = c(진로동기=2, 자기효능감=3, 진로준비=1.5, 진로태도=2.5))
#' print(result_named)
#'
#' # 값만 있는 경우
#' result_unnamed <- plspm_ind_spread(jutpls_boot, set = c(2, 2, 1.5, 1.5))
#' print(result_unnamed)
#'
#' # 부분적으로 값이 있는 경우
#' result_partial <- plspm_ind_spread(jutpls_boot, set = c(진로동기=2, 자기효능감=3))
#' print(result_partial)
#'
#' }
#'
#'
#'
plspm_ind_spread <- function(pls_boot, set = 2) {
  # 이름 추출
  lvs_names <- pls_boot$model$gens$lvs_names

  # 기본값 설정
  values <- rep(1.5, length(lvs_names))
  names(values) <- lvs_names

  if (is.null(names(set))) {
    # set이 이름 없는 벡터일 경우
    if (length(set) == 1) {
      values <- rep(set, length(lvs_names))
    } else if (length(set) != length(lvs_names)) {
      stop("Length of 'set' must match the length of 'lvs_names'")
    } else {
      values <- set
    }
  } else {
    # set이 이름 있는 벡터일 경우
    for (name in names(set)) {
      if (name %in% lvs_names) {
        values[name] <- set[name]
      }
    }
  }

  # 결과 데이터프레임 생성
  result <- setNames(as.data.frame(t(values)), lvs_names)

  return(unlist(result))
}




#' plspm_ind_push
#'
#' @param pls_boot pls_boot
#' @param set  set=1.5 or c(set= c(2,2,1.5,1.5))
#'
#' @return vector
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # 예시 사용법
#' jutpls_boot <- list(model = list(gens = list(lvs_names = c("자기효능감", "진로동기", "진로태도", "진로준비"))))
#'
#' # 기본값 사용하는 경우
#' result_default <- plspm_ind_push(jutpls_boot)
#' print(result_default)
#'
#' # 이름과 값이 있는 경우
#' result_named <- plspm_ind_push(jutpls_boot, set = c(진로동기=2, 자기효능감=3, 진로준비=1.5, 진로태도=2.5))
#' print(result_named)
#'
#' # 값만 있는 경우
#' result_unnamed <- plspm_ind_push(jutpls_boot, set = c(2, 2, 1.5, 1.5))
#' print(result_unnamed)
#'
#' # 부분적으로 값이 있는 경우
#' result_partial <- plspm_ind_push(jutpls_boot, set = c(진로동기=2, 자기효능감=3))
#' print(result_partial)
#'
#' }
#'
#'
#'
plspm_boot_ind_push <- function(pls_boot, set = 2) {
  # 이름 추출
  lvs_names <- pls_boot$model$gens$lvs_names

  # 기본값 설정
  values <- rep(1.5, length(lvs_names))
  names(values) <- lvs_names

  if (is.null(names(set))) {
    # set이 이름 없는 벡터일 경우
    if (length(set) == 1) {
      values <- rep(set, length(lvs_names))
    } else if (length(set) != length(lvs_names)) {
      stop("Length of 'set' must match the length of 'lvs_names'")
    } else {
      values <- set
    }
  } else {
    # set이 이름 있는 벡터일 경우
    for (name in names(set)) {
      if (name %in% lvs_names) {
        values[name] <- set[name]
      }
    }
  }

  # 결과 데이터프레임 생성
  result <- setNames(as.data.frame(t(values)), lvs_names)

  return(unlist(result))
}


#' plspm_boot_factor_layout
#'
#' @param plsres_boot bootresult
#' @param positions position change
#' @param nrow 4,
#' @param ncol 5
#' @param ... add c()
#'
#' @return matrix
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시 호출
#' plsres_boot <- list(model = list(gens = list(lvs_names = c("LV1", "LV2", "LV3", "LV4"))))
#' result <- plspm_boot_factor_layout(plsres_boot)
#' print(result)
#'
#' # 예시 호출
#' plsres_boot <- list(model = list(gens = list(lvs_names = c("LV1", "LV2", "LV3", "LV4", "LV5"))))
#' plspm_boot_factor_layout(plsres_boot)
#'
#' # 예시 호출
#' plsres_boot <- list(model = list(gens = list(lvs_names = c("LV1", "LV2", "LV3", "LV4", "LV5", "LV6", "LV7","LV8"))))
#' plspm_boot_factor_layout(plsres_boot )

#'
#' }
#'
#'
plspm_boot_factor_layout <- function(plsres_boot, positions = NULL, nrow = 4, ncol = 5,
                                     ...) {
  # Extract the latent variable names
  lvs_names <- plsres_boot$model$gens$lvs_names

  # Number of latent variables
  n_lvs <- length(lvs_names)

  # Initialize a matrix with NA values
  mat <- matrix(NA, nrow = nrow, ncol = ncol)

  # Default positions
  default_positions <- list(c(2, 1), c(1, 2), c(3, 2), c(2, 3),
                            c(1, 4), c(3, 4), c(2, 5), c(4, 5))

  # Add additional positions from ...
  additional_positions <- list(...)
  if (length(additional_positions) > 0) {
    default_positions <- c(default_positions, additional_positions)
  }

  # Use provided positions or fall back to default
  if (!is.null(positions)) {
    for (name in names(positions)) {
      pos <- positions[[name]]
      mat[pos[1], pos[2]] <- name
    }
  }

  # Assign remaining positions using defaults
  index <- 1
  for (i in seq_len(n_lvs)) {
    if (is.null(positions) || !(lvs_names[i] %in% names(positions))) {
      while (index <= length(default_positions) && !is.na(mat[default_positions[[index]][1], default_positions[[index]][2]])) {
        index <- index + 1
      }
      if (index <= length(default_positions)) {
        pos <- default_positions[[index]]
        mat[pos[1], pos[2]] <- lvs_names[i]
        index <- index + 1
      }
    }
  }

  # Remove columns that contain only NA values
  mat <- mat[, colSums(is.na(mat)) < nrow(mat)]
  # Remove rows that contain only NA values
  mat <- mat[rowSums(is.na(mat)) < ncol(mat), ]
  return(mat)
}

#' plspm_boot_factor_point_to
#'
#' @param plsres_boot  plsres_boot
#' @param point  point point = list(up = c(2,1))
#' @param positions positions positions = list(up = c(2,1))
#' @param nrow 3
#' @param ncol 3
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' plspm_boot_factor_point_to(jutpls_boot)
#' plspm_boot_factor_point_to(satpls_boot)
#' # plspm_factor_point_mat(jutpls_boot, point=list())
#' # point <- list(up = c(1, 2))
#' plspm_boot_factor_point_to(jutpls_boot, point = list(up = c(2,1)))
#' plspm_boot_factor_point_to(jutpls_boot, point = list(up = c(2,1), right =c(1,2)))
#' #'
#' }
#'
#'
#'

plspm_boot_factor_point_to <- function(plsres_boot, point = NULL,
                                       positions = NULL, nrow = 4, ncol = 5) {
  # Extract the latent variable names
  lvs_names <- plsres_boot$model$gens$lvs_names

  # Number of latent variables
  n_lvs <- length(lvs_names)

  # Initialize a matrix with NA values
  mat <- matrix(NA, nrow = nrow, ncol = ncol)

  # Default positions
  default_positions <- list(c(2, 1), c(1, 2), c(3, 2), c(2, 3),
                            c(1, 4), c(3, 4), c(2, 5), c(4, 5))
  # Assign positions based on input or default
  if (!is.null(positions)) {
    for (name in names(positions)) {
      pos <- positions[[name]]
      mat[pos[1], pos[2]] <- name
    }
  }

  # Determine points
  points <- rep(NA, n_lvs)
  if (n_lvs >= 2) {
    points[1] <- "left"
    points[n_lvs] <- "right"
    if (n_lvs >= 3) {
      for (i in 2:(n_lvs - 1)) {
        points[i] <- ifelse(i %% 2 == 0, "up", "down")
      }
    }
  }

  # Fill the matrix based on the positions and points
  point_index <- 1
  for (i in seq_len(n_lvs)) {
    if (is.null(positions) || !(lvs_names[i] %in% names(positions))) {
      while (point_index <= length(default_positions) && !is.na(mat[default_positions[[point_index]][1], default_positions[[point_index]][2]])) {
        point_index <- point_index + 1
      }
      if (point_index <= length(default_positions)) {
        pos <- default_positions[[point_index]]
        mat[pos[1], pos[2]] <- points[i]
        point_index <- point_index + 1
      }
    }
  }

  # Apply user-defined point replacements
  if (!is.null(point)) {
    for (p in names(point)) {
      pos <- point[[p]]
      mat[pos[1], pos[2]] <- p
    }
  }

  # Remove columns that contain only NA values
  mat <- mat[, colSums(is.na(mat)) < nrow(mat)]
  # Remove rows that contain only NA values
  mat <- mat[rowSums(is.na(mat)) < ncol(mat), ]
  return(mat)
}


#' plspm_boot_ind_spread <- function(pls_boot, set = 2) {
#'   # 이름 추출
#'   lvs_names <- pls_boot$model$gens$lvs_names
#'
#'   # 기본값 설정
#'   values <- rep(1.5, length(lvs_names))
#'   names(values) <- lvs_names
#'
#'   if (is.null(names(set))) {
#'     # set이 이름 없는 벡터일 경우
#'     if (length(set) == 1) {
#'       values <- rep(set, length(lvs_names))
#'     } else if (length(set) != length(lvs_names)) {
#'       stop("Length of 'set' must match the length of 'lvs_names'")
#'     } else {
#'       values <- set
#'     }
#'   } else {
#'     # set이 이름 있는 벡터일 경우
#'     for (name in names(set)) {
#'       if (name %in% lvs_names) {
#'         values[name] <- set[name]
#'       }
#'     }
#'   }
#'
#'   # 결과 데이터프레임 생성
#'   result <- setNames(as.data.frame(t(values)), lvs_names)
#'
#'   return(unlist(result))
#' }
#'
#' #' plspm_boot_factor_layout
#' #'
#' #' @param plsres_boot  plsres_boot
#' #' @param positions  positions coord
#' #' @param nrow 3
#' #' @param ncol 3 number of lantent 4 is ncol = 3
#' #'
#' #' @return data
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' \dontrun{
#' #' #'
#' #' plspm_boot_factor_layout(jutpls_boot)
#' #' plspm_boot_factor_layout(jutpls_boot, list(진로동기 = c(2, 1), 자기효능감 = c(1, 2)))
#' #' plspm_boot_factor_layout(satpls_boot)
#' #' plspm_boot_factor_layout(satpls_boot, list( VAL=c(2, 3), SAT= c(1, 4), LOY=c(3, 4)))
#'
#' #' }
#' #'
#' #'
#' plspm_boot_factor_layout <- function(plsres_boot, positions = NULL, nrow = 3, ncol = 4) {
#'   # Extract the latent variable names
#'   lvs_names <- plsres_boot$model$gens$lvs_names
#'
#'   # Number of latent variables
#'   n_lvs <- length(lvs_names)
#'
#'   # Initialize a matrix with NA values
#'   mat <- matrix(NA, nrow = nrow, ncol = ncol)
#'
#'   # Default positions
#'   default_positions <- list(
#'     c(2, 1), c(1, 2), c(3, 2), c(2, 3),
#'     c(1, 4), c(3, 4)
#'   )
#'
#'   # Use provided positions or fall back to default
#'   if (!is.null(positions)) {
#'     for (name in names(positions)) {
#'       pos <- positions[[name]]
#'       mat[pos[1], pos[2]] <- name
#'     }
#'   }
#'   # Assign remaining positions using defaults
#'   index <- 1
#'   for (i in seq_len(n_lvs)) {
#'     if (is.null(positions) || !(lvs_names[i] %in% names(positions))) {
#'       while (index <= length(default_positions) && !is.na(mat[default_positions[[index]][1],
#'                                                               default_positions[[index]][2]])) {
#'         index <- index + 1
#'       }
#'       if (index <= length(default_positions)) {
#'         pos <- default_positions[[index]]
#'         mat[pos[1], pos[2]] <- lvs_names[i]
#'         index <- index + 1
#'       }
#'     }
#'   }
#'   return(mat)
#' }
