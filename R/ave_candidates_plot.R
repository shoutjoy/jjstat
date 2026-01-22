#' ave_candidates_plot: 문항 제거 전후 AVE 비교 시각화
#'
#' @param df lavaan 객체 또는 ave_raise_candidates() 결과
#' @param range_focus TRUE면 AVE 변화 중심으로 x축 범위 조정
#' @param latent 특정 잠재변수만 시각화 (선택)
#' @param font_size 텍스트 크기
#' @param padding 범위조절 값 (기본 0.04)
#' @param v_line_show 각 문항의 ave_current 위치에 수직선 추가 여부
#'
#' @return ggplot2 객체
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' library(ggplot2)
#' library(dplyr)
#'
#' # 예시 데이터: HolzingerSwineford1939
#' model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' fit <- cfa(model, data = lavaan::HolzingerSwineford1939)
#'
#' # 1. lavaan 객체 입력 시 자동 처리
#' ave_candidates_plot(fit)
#'
#' # 2. 특정 잠재변수만 보기
#' ave_candidates_plot(fit, latent = "visual")
#'
#' # 3. AVE 변화량이 두드러진 경우에만 확대해서 보기
#' ave_candidates_plot(fit, range_focus = TRUE)
#'
#' # 4. AVE 변화 전후 수직선 제거
#' ave_candidates_plot(fit, v_line_show = FALSE)
#'
#' # 5. font 크기 조절
#' ave_candidates_plot(fit, font_size = 3)
#'
#' # 6. ave_raise_candidates() 결과를 직접 전달
#' result <- ave_raise_candidates(fit, print_result = FALSE)
#' ave_candidates_plot(result)
#' }
ave_candidates_plot <- function(df,
                                latent = NULL,
                                range_focus = TRUE,
                                font_size = 4,
                                padding = 0.04,
                                v_line_show = TRUE) {

  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("magrittr", quietly = TRUE)) {
    stop("필요한 패키지(ggplot2, dplyr, magrittr)를 설치해주세요.")
  }

  if (inherits(df, "lavaan")) {
    message("ℹ️  lavaan 객체 감지: ave_raise_candidates()를 자동 실행합니다.")
    df <- ave_raise_candidates(df, print_result = FALSE)
  }

  if (!is.null(latent)) {
    df <- df %>% dplyr::filter(latent == !!latent)
  }

  df_plot <- df %>%
    dplyr::mutate(
      delta = ave_if_dropped - ave_current,
      direction = dplyr::if_else(delta >= 0, "Increase", "Decrease"),
      label_current = sprintf("%.3f", ave_current),
      label_if = sprintf("%.3f (Δ=%+0.3f)", ave_if_dropped, delta),
      Item_fct = factor(Item, levels = rev(unique(Item))),
      y_pos = as.numeric(factor(Item, levels = rev(unique(Item))))
    )

  if (range_focus) {
    all_vals <- c(df_plot$ave_current, df_plot$ave_if_dropped)
    ave_min <- min(all_vals, na.rm = TRUE)
    ave_max <- max(all_vals, na.rm = TRUE)
    x_min <- max(0, ave_min - padding)
    x_max <- min(1, ave_max + padding)
  } else {
    x_min <- 0
    x_max <- 1
  }

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(y = Item_fct)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = 1, y = y_pos, yend = y_pos),
      linetype = "dashed", color = "gray85", linewidth = 0.3
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = ave_current, xend = ave_if_dropped, y = y_pos, yend = y_pos, color = direction),
      size = 3
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = ave_current, y = y_pos),
      shape = 21, fill = "white", size = 2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = ave_if_dropped, y = y_pos),
      shape = 21, fill = "black", size = 2
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = ave_current, y = y_pos, label = label_current),
      vjust = -1, size = font_size
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = ave_if_dropped, y = y_pos, label = label_if),
      vjust = 1.5, size = font_size
    ) +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dotted", color = "gray50") +
    ggplot2::scale_color_manual(values = c(
      "Increase" = "steelblue",
      "Decrease" = "tomato"
    )) +
    ggplot2::facet_wrap(~ latent, scales = "free_y") +
    ggplot2::coord_cartesian(xlim = c(x_min, x_max)) +
    ggplot2::labs(
      x = "AVE",
      y = "Item",
      title = "문항 제거 전후 AVE 비교 (잠재변수별)",
      color = "Change"
    ) +
    ggplot2::theme_bw()

  if (v_line_show) {
    p <- p + ggplot2::geom_vline(
      data = df_plot,
      ggplot2::aes(xintercept = ave_current, group = Item),
      linetype = "dashed",
      color = "gray60",
      linewidth = 0.3,
      alpha = 0.6
    )
  }

  return(p)
}
