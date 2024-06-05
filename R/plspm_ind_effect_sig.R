#' plspm_ind_effect_sig
#'
#' @param boot_data bootstrap data
#' @param from from
#' @param through  era
#' @param to  to
#' @param digits 3
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 함수 호출 예시
#' plspm_ind_effect_sig(
#'   boot_data = satpls_boot,
#'   from = "IMAG",
#'   through = c("EXPE", "QUAL", "SAT"),
#'   to = "VAL"
#' )
#'
#' }
#'
#'
#'
plspm_ind_effect_sig <- function(boot_data, from, through, to, digits = 3) {
  # 1. 부트스트랩 데이터에서 경로 추출 및 열 추가
  path_data <- boot_data$boot$paths %>%
    rownames_to_column("relationships")

  # 경로 설정
  paths <- c(from, through, to)

  # 초기 변수 설정
  est_values <- numeric()
  se_values <- numeric()
  result_paths <- character()

  # 경로별로 추정치와 표준오차 계산
  for (i in seq_along(paths)[-length(paths)]) {
    start <- paths[i]
    end <- paths[i + 1]

    path_info <- path_data %>%
      filter(relationships == paste(start, "->", end))

    est <- path_info$Original
    se <- path_info$`Std.Error`

    est_values <- c(est_values, est)
    se_values <- c(se_values, se)
    result_paths <- c(result_paths, paste(start, "->", end))
  }

  # 전체 경로 계산
  total_est <- prod(est_values)

  # 소벨 테스트를 위한 표준오차 계산
  sobel_se <- sqrt(sum((se_values / est_values)^2)) * total_est

  # t값과 p값 계산
  t_value <- total_est / sobel_se
  p_value <- 2 * (1 - pnorm(abs(t_value)))

  # 결과 생성
  result <- data.frame(
    path = paste(paths, collapse = " -> "),
    est = round(total_est, digits),
    se = round(sobel_se, digits),
    t = round(t_value, digits),
    p = round(p_value, digits),
    sig = ifelse(p_value < 0.05, "Yes", "No")
  )

  return(result)
}




#' plspm_ind_effect
#'
#' @param boot_model  boot_model
#' @param from  from
#' @param through  through
#' @param to  to
#' @param type  res, all
#' @param digits 3
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Example usage of the function with the provided PLS-PM model
#' plspm_ind_effec(
#'   boot_model = jutpls_boot,
#'   from = "자기효능감",
#'   through = c("진로동기", "진로태도"),
#'   to = "진로준비"
#' )
#'
#' plspm_ind_effec(
#'   boot_model = jutpls_boot,
#'   from = "자기효능감",
#'   through = c("진로동기", "진로태도"),
#'   to = "진로준비", type="all"
#' )
#'
#' plspm_ind_effec(
#'   boot_model = jutpls_boot,
#'   from = "자기효능감",
#'   through = c("진로동기"),
#'   to = "진로준비", type="res"
#' )


#'
#' }
#'
#'
plspm_ind_effect <- function(boot_model, from, through, to, type="res", digits=3) {
  # Extract the bootstrapped paths
  boot_paths <- boot_model$boot$paths %>% rownames_to_column("relationships")

  # Initialize total indirect effect
  total_indirect_effect <- 0

  # Initialize a list to store individual path effects and their statistics
  path_effects <- list()

  # Calculate the indirect effect for each path
  for (i in 1:length(through)) {
    path1_name <- paste(from, "->", through[i])
    path2_name <- paste(through[i], "->", to)

    path1 <- boot_paths[boot_paths$relationships == path1_name, "Original"]
    path2 <- boot_paths[boot_paths$relationships == path2_name, "Original"]
    SE_a <- round(boot_paths[boot_paths$relationships == path1_name, "Std.Error"], digits)
    SE_b <- round(boot_paths[boot_paths$relationships == path2_name, "Std.Error"], digits)

    ind_ab <- round(path1 * path2, digits)
    sqrt_se <- round(sqrt(path2^2 * SE_a^2 + path1^2 * SE_b^2), digits)
    Z_sobel <- round(ind_ab / sqrt_se, digits)
    p_value <- round(2 * (1 - pnorm(abs(Z_sobel))), digits)  # Two-tailed test

    path <- paste(from, "->", through[i], "->", to)

    path_effects[[path]] <- list(
      path = path,
      Est = ind_ab,
      SE_a = SE_a,
      SE_b = SE_b,
      SE = sqrt_se,
      z = Z_sobel,
      p.value = p_value
    ) %>% jjstat::p_mark_sig()

    total_indirect_effect <- total_indirect_effect + ind_ab
  }

  # Convert the list of path effects to a tibble
  ind_effect <- bind_rows(lapply(path_effects, as_tibble), .id = "path")%>%
    dplyr::select(-SE_a,  -SE_b,)


  # Extract the direct effect
  direct_path_name <- paste(from, "->", to)
  direct_effect <- boot_paths[boot_paths$relationships == direct_path_name, "Original"]

  direct_effect_se <- boot_paths[boot_paths$relationships == direct_path_name, "Std.Error"]
  # Calculate the total effect
  boot_effect = boot_model$boot$total.efs%>%row2col("relationships")
  total_effect <- total_indirect_effect + direct_effect
  total_effect_se <- boot_effect[boot_effect$relationships == direct_path_name, "Std.Error"]


  de = data.frame(effect = "de", path = direct_path_name, Est = direct_effect, SE= direct_effect_se)%>%
    mutate(z = Est/SE,
           sig = ifelse(abs(z) == "", ns,
                        ifelse(abs(z) > 3.29, "***",
                               ifelse(abs(z) > 2.58, "**",
                                      ifelse(abs(z) > 1.96, "*", "ns"))))) %>%
    dplyr::select(effect, path, Est, SE, z, sig)

  te = data.frame(effect = "te", path = direct_path_name, Est = total_effect, SE = total_effect_se)%>%
    mutate(z = Est/SE,
           sig = ifelse(abs(z) == "", ns,
                        ifelse(abs(z) > 3.29, "***",
                               ifelse(abs(z) > 2.58, "**",
                                      ifelse(abs(z) > 1.96, "*", "ns"))))) %>%
    dplyr::select(effect, path, Est, SE, z, sig)

  ie = cbind(effect="ie", ind_effect%>%
               dplyr::select(path, Est, SE, z ) )%>%
    mutate(
      sig = ifelse(abs(z) == "", ns,
                   ifelse(abs(z) > 3.29, "***",
                          ifelse(abs(z) > 2.58, "**",
                                 ifelse(abs(z) > 1.96, "*", "ns")))))



  ########
  dt =  bind_rows(direct_effect = Round(de, digits),
                  indirect_effect = Round(ie, digits),
                  total_effect = Round(te, digits) )%>%
    mutate(path= format(path, justify="left"))
  # Return the results
  All = list(ind_effect = ind_effect, effect = dt)
  Res = ind_effect


  switch(type, res= Res, all= All)

}
