#' CFA 문항 제외 전후 HTMT 후보 시각화
#'
#' @param df lavaan 적합 객체 또는 cfa_htmt_candidates() 결과.
#' @param latent 점검할 요인. 한 개면 해당 요인을 포함하는 쌍, 여러 개면 그들 사이의 쌍.
#' @param cutoff NULL이면 후보 결과의 settings에서 기준을 읽습니다. lavaan 입력은 기본 0.9.
#' @param top_n 요인쌍마다 표시할 최대 후보 수. 기본 10, Inf이면 전체.
#' @param range_focus TRUE이면 제외 전후 값과 기준선 부근으로 x축 확대.
#' @param font_size 값 표시 텍스트 크기.
#' @param padding x축 여백. 기본 0.04.
#' @param v_line_show TRUE이면 쌍별 현재 HTMT에 회색 수직선을 표시합니다.
#' @param ... df가 lavaan일 때 cfa_htmt_candidates()에 전달할 인수.
#'   기본적으로 기준 미만 쌍도 표시합니다. 예: only_exceed = TRUE로 초과 쌍만 보기,
#'   htmt2 = TRUE, min_items = 3.
#' @return ggplot2 객체. 빈 원은 현재값, 채운 원은 문항 제외 후 값이며
#'   붉은 점선은 cutoff입니다. HTMT가 1보다 커도 축에서 자르지 않습니다.
#' @details 모형을 재적합하지 않은 한 문항 제외 탐색 결과입니다.
#'   계산 불가 문항은 상태를 표시하고, 후보가 없으면 안내 그래프를 반환합니다.
#'   df가 이미 계산된 결과이면 cutoff는 그래프의 비교 기준만 바꿉니다.
#'   후보 쌍을 새 기준으로 다시 선택하려면 cfa_htmt_candidates()를 다시 실행하십시오.
#' @export
#' @examples
#' \dontrun{
#' model <- 'visual =~ x1 + x2 + x3
#'           textual =~ x4 + x5 + x6
#'           speed =~ x7 + x8 + x9'
#' fit <- lavaan::cfa(model, data = lavaan::HolzingerSwineford1939)
#' cfa_htmt_candidates_plot(fit, only_exceed = FALSE)
#' checks <- cfa_htmt_candidates(fit, only_exceed = FALSE, print_result = FALSE)
#' cfa_htmt_candidates_plot(checks, latent = c("visual", "textual"), top_n = Inf)
#' # cfa_htmt_candidates_plot(fit_7f, latent = c("Value", "Effort"))
#' }
cfa_htmt_candidates_plot <- function(df, latent = NULL, cutoff = NULL, top_n = 10,
                                      range_focus = TRUE, font_size = 3,
                                      padding = 0.04, v_line_show = TRUE, ...) {
  if (inherits(df, "lavaan")) {
    if (is.null(cutoff)) cutoff <- .9
    df <- cfa_htmt_candidates(df, latent = latent, cutoff = cutoff, print_result = FALSE, ...)
    latent <- NULL
  }
  required <- c("factor1", "factor2", "pair", "latent", "Item", "htmt_current", "htmt_if_dropped", "status")
  if (!is.data.frame(df) || !all(required %in% names(df))) {
    stop("df must be a lavaan fit or cfa_htmt_candidates() result.")
  }
  if (is.null(cutoff)) {
    settings <- attr(df, "settings")
    cutoff <- if (is.null(settings$cutoff)) .9 else settings$cutoff
  }
  if (!is.numeric(cutoff) || length(cutoff) != 1L || !is.finite(cutoff) || cutoff <= 0 || cutoff > 1) {
    stop("cutoff must be in (0, 1].")
  }
  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) || top_n < 1 ||
      (is.finite(top_n) && top_n != floor(top_n))) stop("top_n must be a positive integer or Inf.")
  if (!is.numeric(padding) || length(padding) != 1L || !is.finite(padding) || padding < 0) stop("padding must be >= 0.")
  if (!is.numeric(font_size) || length(font_size) != 1L || !is.finite(font_size) || font_size <= 0) stop("font_size must be positive.")
  for (flag in list(range_focus, v_line_show)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) stop("Plot flags must be TRUE or FALSE.")
  }
  if (!is.null(latent)) {
    if (!is.character(latent) || !length(latent) || anyNA(latent)) stop("latent must contain factor names.")
    latent <- unique(latent)
    keep <- if (length(latent) == 1L) df$factor1 %in% latent | df$factor2 %in% latent else
      df$factor1 %in% latent & df$factor2 %in% latent
    df <- df[keep, , drop = FALSE]
  }
  if (!nrow(df)) return(ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = "No matching HTMT candidates.\nUse only_exceed = FALSE to inspect all pairs.") +
    ggplot2::theme_void() + ggplot2::labs(title = "HTMT candidate diagnostics"))
  df <- as.data.frame(df)
  df$improvement <- df$htmt_current - df$htmt_if_dropped
  df <- df[order(match(df$pair, unique(df$pair)), -df$improvement, na.last = TRUE), , drop = FALSE]
  if (is.finite(top_n)) {
    index <- ave(seq_len(nrow(df)), df$pair, FUN = seq_along)
    df <- df[index <= top_n, , drop = FALSE]
  }
  good <- is.finite(df$htmt_current) & is.finite(df$htmt_if_dropped) & df$status == "evaluated"
  df$result <- ifelse(!good, "Not evaluable", ifelse(df$htmt_if_dropped < cutoff,
    "Below cutoff", ifelse(df$improvement > 0, "Reduced", "No reduction")))
  df$row_key <- factor(paste(df$pair, df$latent, df$Item, sep = " | "),
                        levels = rev(unique(paste(df$pair, df$latent, df$Item, sep = " | "))))
  labels <- stats::setNames(paste(df$latent, df$Item, sep = ": "), as.character(df$row_key))
  df$after_label <- ifelse(good, sprintf("%.3f (delta=%+.3f)", df$htmt_if_dropped,
                                         df$htmt_if_dropped - df$htmt_current), df$status)
  df$label_x <- ifelse(good, df$htmt_if_dropped, ifelse(is.finite(df$htmt_current), df$htmt_current, cutoff))
  values <- c(df$htmt_current, df$htmt_if_dropped, cutoff)
  values <- values[is.finite(values)]
  xlim <- if (range_focus) c(max(0, min(values) - padding), max(values) + padding) else
    c(0, max(1, max(values) + padding))
  if (diff(xlim) <= 0) xlim[2] <- xlim[1] + .1
  colors <- c("Below cutoff" = "#15803d", "Reduced" = "#2563eb",
              "No reduction" = "#c2410c", "Not evaluable" = "#64748b")
  p <- ggplot2::ggplot(df, ggplot2::aes(y = row_key)) +
    ggplot2::geom_segment(ggplot2::aes(x = htmt_current, xend = htmt_if_dropped,
                                      yend = row_key, color = result), linewidth = 1.2, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(x = htmt_current, shape = "Current"),
                        size = 2.5, color = "#475569", na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(x = htmt_if_dropped, color = result, shape = "If dropped"),
                        size = 2.5, na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(x = label_x, label = after_label),
                       vjust = 1.8, size = font_size, na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = cutoff, color = "#b91c1c", linetype = "dashed") +
    ggplot2::scale_color_manual(values = colors, drop = FALSE) +
    ggplot2::scale_shape_manual(values = c(Current = 1, `If dropped` = 16)) +
    ggplot2::scale_y_discrete(labels = labels, expand = ggplot2::expansion(add = c(.8, .8))) +
    ggplot2::facet_wrap(~pair, scales = "free_y") +
    ggplot2::coord_cartesian(xlim = xlim, clip = "off") +
    ggplot2::labs(title = "HTMT before and after excluding one item",
      subtitle = paste0("Cutoff = ", cutoff, "; fixed correlations, no CFA refit"),
      x = "HTMT", y = "Candidate item", color = "Result", shape = "Estimate") +
    ggplot2::theme_bw() + ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                                        plot.margin = ggplot2::margin(10, 28, 16, 10))
  if (v_line_show) {
    reference <- unique(df[is.finite(df$htmt_current), c("pair", "htmt_current")])
    p <- p + ggplot2::geom_vline(data = reference, ggplot2::aes(xintercept = htmt_current),
                                 color = "#94a3b8", linetype = "dotted")
  }
  p
}
