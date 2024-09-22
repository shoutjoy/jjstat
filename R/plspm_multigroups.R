
#' plspm에서 3개 그룹을 비교하는 경우, p보정 방법 포함
#'
#' @param plspm_boot bootstap data
#' @param group group variable set ="grade"
#' @param method bootstrap, permutation
#' @param type all, grp1, grp2, grp3, ...
#' @param adjust default FALSE
#' @param adjust.method "bonferroni", "holm", "hochberg", "BH", "BY"
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 사용 예시
#'
#' #구조모형
#' jutpath1 = plspm_paths(
#'   row_names = c("SelfEff","CarMot","CarAtt","CarAct"),
#'   relationship = list(
#'     path(from="SelfEff", to=c("CarMot","CarAtt","CarAct")),
#'     path("CarMot", c("CarAct")),
#'     path("CarAtt", c("CarAct"))
#'   )  )
#' jutpath1
#'
#' jutpath1 %>% plspm_paths2lav()%>%cat()
#'
#'
#' # 측정 모형
#' jut_blocks <- plspm_blocks(
#'   SelfEff = item("sef01", "sef02", "sef03", "sef04", "sef05"),
#'   CarMot = item("mot01","mot02","mot03"),
#'   #   CarAtt = item( "att01", "att02","att03","att04","att05" ),
#'   CarAtt = item("att03","att04","att05" ),
#'   CarAct = item("act01", "act02","act03")
#' )
#' jut_blocks
#'
#' jut_blocks %>%plspm_blocks2lav()%>%cat()
#'
#'
#'
#' # 데이터 분석 : bootstrap
#' jutpls_boot = plspm_sem(Data= jut7c,
#'                         path_matrix = jutpath1,
#'                         blocks = jut_blocks,seed=19988,
#'                         br = 5000, summary= TRUE)
#' # p.adjust() 함수: R에서 제공하는 함수로, 선택된 보정 방법에 맞춰 p-value를 보정한다. p.adjust() 함수는 다양한 다중 비교 보정 방법을 지원한다.
#' # "bonferroni": 보수적인 Bonferroni 보정.
#' # "holm": Holm 보정.
#' # "hochberg": Hochberg 보정.
#' # "BH": Benjamini-Hochberg 보정.
#' # "BY": Benjamini-Yekutieli 보정.
#' # Benjamini-Hochberg 보정을 적용하여 결과 조회
#' plspm_multigroups(jutpls_boot, group = jut7c$LCA_class,
#'                   type = "all", adjust = TRUE, adjust.method = "bonferroni")
#'
#' plspm_multigroups(jutpls_boot, group = jut7c$LCA_class,
#'                   type = "all", adjust = FALSE, adjust.method = "bonferroni")
#'
#' plspm_multigroups(jutpls_boot, group = jut7c$LCA_class, method="permutation",
#'                   type = "all", adjust = FALSE, adjust.method = "bonferroni")
#'
#' plspm_multigroups(jutpls_boot, group = jut7c$LCA_class, type = "grp1")
#' plspm_multigroups(jutpls_boot, group = jut7c$LCA_class, type = "grp2")
#' plspm_multigroups(jutpls_boot, group = jut7c$LCA_class, type = "grp3")
#'
#' #정한 방법
#' jut_lca_comparison <- plspm_multigroups(jutpls_boot, group = jut7c$LCA_class)
#' jut_lca_comparison$Class1_vs_Class2$test
#' jut_lca_comparison$Class1_vs_Class3$test
#' jut_lca_comparison$Class2_vs_Class3$test
#'
#'
#' plspm_multigroups(jutpls_boot, group = jut7c$LCA_class,
#'                   type = "all", adjust = TRUE, adjust.method = "holm")
#'
#' plspm_multigroups(jutpls_boot, group = jut7c$LCA_class,
#'                   type = "all", adjust = FALSE, adjust.method = "holm")
#'
#' plspm_multigroups(jutpls_boot, group = jut7c$LCA_class,
#'                   type = "all", adjust = TRUE, adjust.method = "hochberg")
#' plspm_multigroups(jutpls_boot, group = jut7c$LCA_class,
#'                   type = "all", adjust = FALSE, adjust.method = "hochberg")
#'
#' plspm_multigroups(jutpls_boot, group = jut7c$LCA_class,
#'                   type = "all", adjust = TRUE, adjust.method = "BH")
#'
#' plspm_multigroups(jutpls_boot, group = jut7c$LCA_class,
#'                   type = "all", adjust = TRUE, adjust.method = "BY")
#' #'
#' #'
#' }
plspm_multigroups <- function(plspm_boot, group,
                              method = "bootstrap", type = "all",
                              adjust = FALSE, adjust.method = "bonferroni") {
  # 그룹의 레벨 확인
  group_levels <- levels(as.factor(group))

  if (length(group_levels) < 3) {
    stop("3개 이상의 그룹이 필요합니다.")
  }

  # 모든 쌍별 비교 조합 생성
  group_combinations <- combn(group_levels, 2, simplify = FALSE)

  # 결과 저장할 리스트 생성
  comparison_results <- list()

  # 총 비교 횟수 계산 (Bonferroni 보정을 위해)
  num_comparisons <- length(group_combinations)

  # 보정 방법에 따라 p.adjust() 함수 적용
  p.adjust.methods <- c("bonferroni", "holm", "hochberg", "BH", "BY")

  # 각 그룹 쌍별로 plspm.groups() 실행
  for (comb in group_combinations) {
    group1 <- comb[1]
    group2 <- comb[2]

    # 그룹 필터링
    group_subset <- factor(group[group %in% c(group1, group2)])

    # plspm.groups() 실행
    result <- plspm.groups(plspm_boot, group = group_subset, method = method)

    # plspm.groups() 결과를 데이터프레임으로 변환
    comparison_df <- as.data.frame(result$test)

    # 선택된 보정 방법에 따라 p-value 보정 및 adj.p.sig 추가
    if (adjust) {
      if (adjust.method %in% p.adjust.methods) {
        comparison_df$adj.p <- p.adjust(comparison_df$p.value, method = adjust.method)
        comparison_df$adj.p.sig <- ifelse(comparison_df$adj.p < 0.05, "yes", "no")
        cat(adjust.method, "보정이 적용되었습니다.\n")
      } else {
        stop("유효하지 않은 보정 방법입니다. 사용 가능한 방법: ", paste(p.adjust.methods, collapse = ", "))
      }
    } else {
      # adjust = FALSE일 경우 adj.p와 adj.p.sig를 생성하지 않음
      comparison_df$adj.p <- NULL  # NA로 설정하거나 이 부분을 생략할 수 있음
      comparison_df$adj.p.sig <- NULL
    }

    # 결과 저장 (데이터프레임 형식으로 저장)
    comparison_results[[paste0(group1, "_vs_", group2)]] <- comparison_df
  }

  # 각 그룹 결과를 저장
  grp1 <- comparison_results[[paste0(group_levels[1], "_vs_", group_levels[2])]]
  grp2 <- comparison_results[[paste0(group_levels[1], "_vs_", group_levels[3])]]
  grp3 <- comparison_results[[paste0(group_levels[2], "_vs_", group_levels[3])]]

  # switch를 이용한 선택적 결과 반환
  return(switch(type,
                all = comparison_results,
                grp1 = grp1,
                grp2 = grp2,
                grp3 = grp3))
}
