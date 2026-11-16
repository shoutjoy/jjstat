#' CFA HTMT 기준 초과 요인쌍과 문항 제외 후보 점검
#'
#' 각 문항을 하나씩 제외한 HTMT를 비교하고 동일요인/다른요인 상관과
#' 교차적재 수정지수를 제공합니다. 모형이나 원자료를 변경하지 않습니다.
#'
#' @param fit 단일 집단·단일 수준의 lavaan CFA/SEM 적합 객체. 원자료가 필요합니다.
#' @param latent NULL이면 모든 요인쌍. 한 요인이면 해당 요인을 포함하는 쌍,
#'   여러 요인이면 지정한 요인들 사이의 쌍을 점검합니다.
#' @param cutoff HTMT 기준값. 기본 0.9. 이 값 이상이면 기준 초과로 분류합니다.
#' @param htmt2 FALSE는 산술평균 HTMT, TRUE는 기하평균 HTMT2.
#'   기본값은 cfa2()와 동일한 FALSE입니다.
#' @param min_items 문항 제외 후 요인마다 남아야 하는 최소 문항 수. 기본 2.
#' @param only_exceed 기본 FALSE: latent로 선택한 요인쌍의 모든 문항을 현재
#'   HTMT와 관계없이 점검합니다. TRUE로 명시하면 현재 HTMT가 cutoff 이상인
#'   쌍만 후보를 반환합니다. cutoff는 기본적으로 표시/판정 기준입니다.
#' @param missing 상관 계산 시 결측 처리: everything(기본; cfa2와 동일),
#'   pairwise.complete.obs 또는 complete.obs. complete.obs는 최초에 선택한
#'   전체 다문항 요인 문항에 대한 동일 완전사례를 모든 후보에 사용합니다.
#' @param print_result TRUE이면 기준 초과 쌍과 점검 안내를 출력합니다.
#' @param plot TRUE이면 cfa_htmt_candidates_plot()으로 그래프를 즉시 표시합니다.
#'   기본 FALSE. TRUE이면 그래프를 표시하고 전체 후보표를 보이지 않게 반환합니다.
#'   그 그래프는
#'   \code{attr(result, "plot")}으로 다시 사용할 수 있습니다.
#'
#' @return 후보별 tibble. factor1/factor2는 요인쌍, latent/Item은 제외할 문항의
#'   소속과 이름입니다. htmt_current/htmt_if_dropped는 제외 전후 값,
#'   delta는 이후-이전(음수이면 개선), improvement는 이전-이후입니다.
#'   mean_within/mean_between은 해당 문항의 동일요인/다른요인 평균 절대상관,
#'   cross_loading_mi/cross_loading_sepc는 상대 요인으로의 교차적재 수정지수와
#'   표준화 기대변화량입니다. max_htmt_after/n_exceed_after/n_invalid_after는
#'   같은 문항을 제외했을 때 전체 요인쌍의 최댓값/기준 초과 수/계산불가 수입니다.
#'   status와 recommendation은 제외 가능 여부와 탐색 결과입니다.
#'   속성 baseline_pairs, excluded_factors, item_correlations, settings,
#'   diagnostic_notes에서 전체 현황, 제외 요인, 원 상관 및 계산 설정을 확인합니다.
#'   반환값은 일반 22열 tibble입니다. \code{print(result, n = Inf, width = Inf)}로
#'   전체 행과 열을 표시할 수 있습니다.
#'
#' @details
#' 단일 문항 요인과 고차요인은 HTMT 계산에서 제외합니다. 문항이 여러 요인에
#' 교차적재된 모형은 독립된 문항 블록이 아니므로 오류로 안내합니다.
#' 원자료 Pearson 상관의 절댓값을 사용해 cfa2/lav_htmt와 비교할 수 있게 합니다.
#' 순서형 문항도 이 함수에서는 Pearson 상관을 사용하며 polychoric HTMT가 아닙니다.
#' HTMT2는 lav_htmt와 동일하게 기하평균에서 0 상관을 제외합니다.
#' 결측/상수 문항 등으로 유한한 상관이나 양의 분모가 없으면 NA를 유지합니다.
#'
#' 후보는 한 문항씩 독립적으로 제외하는 탐색 계산이며 누적 삭제나 CFA 재적합
#' 결과가 아닙니다. 높은 다른요인 상관, 낮은 동일요인 상관, 역문항 코딩,
#' 내용 중복 및 교차적재를 검토하십시오. 수정지수는 인과적 진단이 아닙니다.
#' 기준을 맞추기 위한 문항 삭제를 자동 권고하지 않습니다. 내용타당도와 요인별
#' 문항 수를 검토하고 실제 수정 모형을 재적합하여 HTMT, 적합도, CR/AVE 및
#' 표본 안정성을 확인해야 합니다. 두 문항이 남아도 모형 식별이 보장되지는 않습니다.
#'
#' @references
#' Henseler, Ringle & Sarstedt (2015). A new criterion for assessing discriminant
#' validity in variance-based structural equation modeling.
#' \url{https://search.r-project.org/CRAN/refmans/semTools/html/htmt.html}
#' \url{https://lavaan.ugent.be/tutorial/modindices.html}
#' @export
#' @examples
#' \dontrun{
#' model <- 'visual =~ x1 + x2 + x3
#'           textual =~ x4 + x5 + x6
#'           speed =~ x7 + x8 + x9'
#' fit <- lavaan::cfa(model, data = lavaan::HolzingerSwineford1939)
#' candidates <- cfa_htmt_candidates(fit)
#' attr(candidates, "baseline_pairs") # 기준 미만 쌍도 포함한 현재 HTMT
#' attr(candidates, "excluded_factors")
#' cfa_htmt_candidates(fit) # 기준 미만 쌍도 모든 문항 점검
#' cfa_htmt_candidates(fit, only_exceed = TRUE) # 기준 초과 쌍으로 제한
#' cfa_htmt_candidates(fit, latent = c("visual", "textual"), cutoff = 0.85)
#' cfa_htmt_candidates_plot(candidates)
#' # 후보 계산과 그래프 표시를 한 번에 실행
#' checks <- cfa_htmt_candidates(fit, only_exceed = FALSE, plot = TRUE)
#' checks # 일반 tibble 출력
#' print(checks, n = Inf, width = Inf) # 전체 행과 22개 열 확인
#' attr(checks, "plot") # 그래프 다시 표시
#' # 사용자 모형의 Value-Effort 쌍 점검
#' # checks <- cfa_htmt_candidates(fit_7f, latent = c("Value", "Effort"))
#' # checks[, c("latent", "Item", "htmt_current", "htmt_if_dropped",
#' #            "improvement", "mean_within", "mean_between", "cross_loading_mi")]
#' }
cfa_htmt_candidates <- function(fit, latent = NULL, cutoff = 0.9, htmt2 = FALSE,
                                 min_items = 2L, only_exceed = FALSE,
                                 missing = c("everything", "pairwise.complete.obs", "complete.obs"),
                                 print_result = TRUE, plot = FALSE) {
  if (!inherits(fit, "lavaan")) stop("fit must be a fitted lavaan object.")
  if (lavaan::lavInspect(fit, "ngroups") != 1L || lavaan::lavInspect(fit, "nlevels") != 1L) {
    stop("Only single-group, single-level models are supported; inspect groups separately.")
  }
  if (length(cutoff) != 1L || !is.numeric(cutoff) || !is.finite(cutoff) || cutoff <= 0 || cutoff > 1) {
    stop("cutoff must be a number in (0, 1].")
  }
  for (flag in list(htmt2 = htmt2, only_exceed = only_exceed, print_result = print_result, plot = plot)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) stop("Logical options must be TRUE or FALSE.")
  }
  if (!is.numeric(min_items) || length(min_items) != 1L || !is.finite(min_items) ||
      min_items < 2 || min_items != floor(min_items)) stop("min_items must be an integer >= 2.")
  missing <- match.arg(missing)
  data <- tryCatch(lavaan::lavInspect(fit, "data"), error = function(e) NULL)
  if (is.null(dim(data)) || nrow(data) < 2L) stop("Raw observations are required; covariance-only fits are not supported.")
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  mm <- pe[pe$op == "=~", c("lhs", "rhs", "std.all")]
  factors <- unique(mm$lhs)
  if (!length(factors)) stop("The model has no measurement factors.")
  blocks <- stats::setNames(lapply(factors, function(x) unique(mm$rhs[mm$lhs == x])), factors)
  higher <- vapply(blocks, function(items) !all(items %in% colnames(data)), logical(1))
  single <- lengths(blocks) < 2L
  excluded <- tibble::tibble(latent = factors[higher | single],
                             n_items = lengths(blocks)[higher | single],
                             reason = ifelse(higher[higher | single], "higher_order_or_unobserved", "single_item"))
  blocks <- blocks[!(higher | single)]
  if (!is.null(latent)) {
    if (!is.character(latent) || !length(latent) || anyNA(latent) || !all(latent %in% factors)) {
      stop("latent must contain names of measurement factors in the model.")
    }
    latent <- unique(latent)
    if (!any(latent %in% names(blocks))) stop("Selected factors have no eligible multi-item blocks.")
  }
  duplicated_items <- unique(unlist(blocks, use.names = FALSE)[duplicated(unlist(blocks, use.names = FALSE))])
  if (length(duplicated_items)) stop("HTMT requires disjoint item blocks. Cross-loaded items: ", paste(duplicated_items, collapse = ", "))
  items <- unique(unlist(blocks, use.names = FALSE))
  if (length(items)) {
    R <- suppressWarnings(stats::cor(data[, items, drop = FALSE], use = missing))
  } else R <- matrix(numeric(), 0, 0)
  notes <- character()
  if (length(blocks) < 2L) notes <- c(notes, "At least two eligible multi-item factors are required for HTMT.")
  if (any(!is.finite(R))) notes <- c(notes, "Non-finite correlations: inspect missing data, constant items, and the missing option.")
  mi <- tryCatch(lavaan::modindices(fit), error = function(e) {
    notes <<- c(notes, paste("Modification indices unavailable:", conditionMessage(e)))
    NULL
  })
  baseline <- .cfa_htmt_pairs(R, blocks, htmt2, cutoff)
  selected <- rep(TRUE, nrow(baseline))
  if (!is.null(latent)) selected <- if (length(latent) == 1L) {
    baseline$factor1 %in% latent | baseline$factor2 %in% latent
  } else baseline$factor1 %in% latent & baseline$factor2 %in% latent
  if (only_exceed) selected <- selected & is.finite(baseline$htmt) & baseline$htmt >= cutoff
  selected_pairs <- baseline[selected, , drop = FALSE]
  rows <- list()
  cache <- new.env(parent = emptyenv())
  mean_finite <- function(z) if (length(z) && all(is.finite(z))) mean(abs(z)) else NA_real_
  for (pair_row in seq_len(nrow(selected_pairs))) {
    a <- selected_pairs$factor1[pair_row]; b <- selected_pairs$factor2[pair_row]
    current <- selected_pairs$htmt[pair_row]
    for (owner in c(a, b)) for (item in blocks[[owner]]) {
      other <- if (owner == a) b else a
      remaining <- length(blocks[[owner]]) - 1L
      valid_removal <- remaining >= min_items
      if (!exists(item, cache, inherits = FALSE)) {
        reduced <- blocks
        reduced[[owner]] <- setdiff(reduced[[owner]], item)
        assign(item, if (valid_removal) .cfa_htmt_pairs(R, reduced, htmt2, cutoff) else NULL, cache)
      }
      after <- get(item, cache, inherits = FALSE)
      target <- if (is.null(after)) NA_real_ else after$htmt[after$factor1 == a & after$factor2 == b]
      if (!length(target)) target <- NA_real_
      status <- if (!valid_removal) "too_few_items" else if (!is.finite(current)) {
        "invalid_current_htmt"
      } else if (!is.finite(target)) "invalid_after_htmt" else "evaluated"
      cross_mi <- if (is.null(mi)) NULL else mi[mi$op == "=~" & mi$lhs == other & mi$rhs == item, , drop = FALSE]
      finite_after <- if (is.null(after)) numeric() else after$htmt[is.finite(after$htmt)]
      recommendation <- if (status != "evaluated") status else if (target < current && target < cutoff) {
        "Below cutoff; review content"
      } else if (target < current) "Reduced; review remaining overlap" else "No HTMT reduction"
      rows[[length(rows) + 1L]] <- tibble::tibble(
        factor1 = a, factor2 = b, pair = paste(a, b, sep = " / "), latent = owner, Item = item,
        lambda = mm$std.all[match(paste(owner, item), paste(mm$lhs, mm$rhs))],
        n_items = length(blocks[[owner]]), items_remaining = remaining,
        htmt_current = current, htmt_if_dropped = target, delta = target - current,
        improvement = current - target, below_cutoff = if (is.finite(target)) target < cutoff else NA,
        mean_within = mean_finite(R[item, setdiff(blocks[[owner]], item)]),
        mean_between = mean_finite(R[item, blocks[[other]]]),
        cross_loading_mi = if (!is.null(cross_mi) && nrow(cross_mi) > 0L) cross_mi$mi[1] else NA_real_,
        cross_loading_sepc = if (!is.null(cross_mi) && nrow(cross_mi) > 0L) cross_mi$sepc.all[1] else NA_real_,
        max_htmt_after = if (length(finite_after)) max(finite_after) else NA_real_,
        n_exceed_after = if (is.null(after)) NA_integer_ else sum(after$htmt >= cutoff, na.rm = TRUE),
        n_invalid_after = if (is.null(after)) NA_integer_ else sum(!is.finite(after$htmt)),
        status = status, recommendation = recommendation
      )
    }
  }
  result <- if (length(rows)) dplyr::bind_rows(rows) else .cfa_htmt_empty()
  if (nrow(result)) {
    result <- result[order(match(result$pair, unique(result$pair)), -result$improvement, na.last = TRUE), ]
  }
  attr(result, "baseline_pairs") <- baseline
  attr(result, "excluded_factors") <- excluded
  attr(result, "item_correlations") <- R
  attr(result, "settings") <- list(cutoff = cutoff, htmt2 = htmt2, min_items = min_items,
                                    missing = missing, only_exceed = only_exceed, correlation = "Pearson")
  attr(result, "diagnostic_notes") <- notes
  if (print_result) {
    message("HTMT >= ", cutoff, ": ", sum(baseline$htmt >= cutoff, na.rm = TRUE),
            " pair(s) overall; selected pairs: ", nrow(selected_pairs),
            "; ", nrow(result), " one-item exclusion checks.")
    if (nrow(excluded)) message("Excluded factors: ", paste(excluded$latent, collapse = ", "))
    if (length(notes)) message(paste(notes, collapse = "\n"))
    message("Negative delta / positive improvement means lower HTMT. Review content and refit before changing the scale.")
  }
  if (plot) {
    plotter <- get0("cfa_htmt_candidates_plot", mode = "function", inherits = TRUE)
    if (is.null(plotter)) stop("Load cfa_htmt_candidates_plot() before using plot = TRUE.")
    graph <- plotter(result)
    print(graph)
    attr(result, "plot") <- graph
    return(invisible(result))
  }
  result
}



.cfa_htmt_value <- function(R, items_a, items_b, geometric) {
  between <- abs(R[items_a, items_b, drop = FALSE])
  aa <- abs(R[items_a, items_a, drop = FALSE]); bb <- abs(R[items_b, items_b, drop = FALSE])
  within_a <- aa[upper.tri(aa)]; within_b <- bb[upper.tri(bb)]
  if (!length(between) || !length(within_a) || !length(within_b) ||
      any(!is.finite(c(between, within_a, within_b)))) return(NA_real_)
  average <- function(z) {
    if (!geometric) return(mean(z))
    z <- z[z != 0]
    if (!length(z)) return(NA_real_)
    exp(mean(log(z)))
  }
  denominator <- sqrt(average(within_a) * average(within_b))
  if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
  value <- average(between) / denominator
  if (is.finite(value)) value else NA_real_
}

.cfa_htmt_pairs <- function(R, blocks, geometric, cutoff) {
  if (length(blocks) < 2L) return(tibble::tibble(factor1 = character(), factor2 = character(),
                                                pair = character(), htmt = numeric(), exceeds = logical()))
  pairs <- utils::combn(names(blocks), 2L)
  values <- vapply(seq_len(ncol(pairs)), function(i) {
    .cfa_htmt_value(R, blocks[[pairs[1, i]]], blocks[[pairs[2, i]]], geometric)
  }, numeric(1))
  tibble::tibble(factor1 = pairs[1, ], factor2 = pairs[2, ],
                 pair = paste(pairs[1, ], pairs[2, ], sep = " / "), htmt = values,
                 exceeds = ifelse(is.finite(values), values >= cutoff, NA))
}

.cfa_htmt_empty <- function() {
  tibble::tibble(factor1 = character(), factor2 = character(), pair = character(),
    latent = character(), Item = character(), lambda = numeric(), n_items = integer(),
    items_remaining = integer(), htmt_current = numeric(), htmt_if_dropped = numeric(),
    delta = numeric(), improvement = numeric(), below_cutoff = logical(), mean_within = numeric(),
    mean_between = numeric(), cross_loading_mi = numeric(), cross_loading_sepc = numeric(),
    max_htmt_after = numeric(), n_exceed_after = integer(), n_invalid_after = integer(),
    status = character(), recommendation = character())
}
