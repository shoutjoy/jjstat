
#' BIC Comparison Plot for Latent Profile Analysis
#'
#' Fits Gaussian mixture models and compares BIC values.
#' Supports conventional BIC and mclust BIC.
#'
#' Conventional BIC:
#' BIC = -2 * log(L) + k * log(n)
#' Smaller BIC indicates better model fit.
#'
#' mclust BIC:
#' BIC = 2 * log(L) - k * log(n)
#' Larger BIC indicates better model fit.
#'
#' The relationship is:
#' BIC_standard = -BIC_mclust
#'
#' @param data Numeric data frame, matrix, or mclustBIC object.
#' @param n_profiles_range Positive integer vector.
#' @param type Output type: all, res, data, wide_data,
#'   long_data, graph, gg, best_model, diagnostics.
#' @param legend_position Legend position.
#' @param covar_model Covariance model names.
#' @param set_n_profile Optional profile count for model selection.
#' @param xintercept Vertical reference line. NULL disables it.
#' @param bic_text Show BIC values at each plotted point.
#' @param size.text Label text size.
#' @param flip Legacy argument retained for compatibility.
#'   BIC direction is controlled by bic_rev.
#' @param bic_rev TRUE: conventional BIC, smaller is better.
#'   FALSE: mclust BIC, larger is better.
#' @param na2zero Legacy argument; zero substitution is disabled.
#' @param new.window Open a new graphics window.
#' @param basic TRUE selects EII, EEE, VII, EEI.
#'
#' @return A list containing BIC tables, graph,
#'   best_model, bic_object, and diagnostics.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Conventional BIC
#' a <- lpa_BIC_plot(
#'   iris[, 1:4],
#'   n_profiles_range = 1:4,
#'   bic_rev = TRUE,
#'   new.window = FALSE
#' )
#' print(a$graph)
#'
#' # Example 2: mclust BIC
#' b <- lpa_BIC_plot(
#'   a$bic_object,
#'   bic_rev = FALSE,
#'   new.window = FALSE
#' )
#'
#' # Example 3: Best model for 4 profiles
#' c <- lpa_BIC_plot(
#'   a$bic_object,
#'   xintercept = 4,
#'   set_n_profile = 4,
#'   bic_text = TRUE
#' )
#' c$best_model
#'
#' # Example 4: No BIC value labels
#' d <- lpa_BIC_plot(
#'   a$bic_object,
#'   bic_text = FALSE
#' )
#' }

lpa_BIC_plot <- function(
    data,
    n_profiles_range = 1:6,
    type = "all",
    legend_position = "top",
    covar_model = c(
      "EII", "EEE", "VII",
      "EEV", "EEI", "VVI", "VVV"
    ),
    set_n_profile = NULL,
    xintercept = 4,
    bic_text = TRUE,
    size.text = 4,
    flip = TRUE,
    bic_rev = TRUE,
    na2zero = FALSE,
    new.window = FALSE,
    basic = TRUE
) {

  # =================================================
  # 1. Required packages
  # =================================================

  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("mclust 패키지를 설치하세요.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 패키지를 설치하세요.")
  }

  # =================================================
  # 2. Validate arguments
  # =================================================

  if (!is.logical(bic_rev) ||
      length(bic_rev) != 1L ||
      is.na(bic_rev)) {
    stop("bic_rev는 TRUE 또는 FALSE여야 합니다.")
  }

  if (!is.logical(bic_text) ||
      length(bic_text) != 1L ||
      is.na(bic_text)) {
    stop("bic_text는 TRUE 또는 FALSE여야 합니다.")
  }

  if (!is.null(set_n_profile)) {
    if (!is.numeric(set_n_profile) ||
        length(set_n_profile) != 1L ||
        !is.finite(set_n_profile) ||
        set_n_profile < 1 ||
        set_n_profile != floor(set_n_profile)) {
      stop("set_n_profile은 양의 정수여야 합니다.")
    }
  }

  if (!is.null(xintercept)) {
    if (!is.numeric(xintercept) ||
        length(xintercept) != 1L ||
        !is.finite(xintercept)) {
      stop("xintercept는 숫자 하나 또는 NULL이어야 합니다.")
    }
  }

  if (!is.numeric(size.text) ||
      length(size.text) != 1L ||
      !is.finite(size.text) ||
      size.text <= 0) {
    stop("size.text는 양수여야 합니다.")
  }

  # =================================================
  # 3. Covariance model selection
  # =================================================

  all_models <- c(
    "EII", "VII", "EEI", "VEI",
    "EVI", "VVI", "EEE", "EVE",
    "VEE", "VVE", "EEV", "VEV",
    "EVV", "VVV"
  )

  if (basic) {
    selected_models <- c("EII", "EEE", "VII", "EEI")
  } else {
    selected_models <- intersect(
      covar_model,
      all_models
    )
  }

  if (length(selected_models) == 0L) {
    stop("선택한 공분산 모형이 유효하지 않습니다.")
  }

  # =================================================
  # 4. BIC estimation or reuse
  # =================================================

  if (inherits(data, "mclustBIC")) {

    # Previously fitted object: skip estimation
    bic <- data

    removed_n <- NA_integer_
    n_used <- NA_integer_
    variables <- NULL

    message("Using existing mclustBIC object.")

  } else {

    if (!is.data.frame(data) && !is.matrix(data)) {
      stop(
        "data는 data.frame, matrix 또는 ",
        "mclustBIC 객체여야 합니다."
      )
    }

    dat <- as.data.frame(data)

    # Select numeric indicators
    is_num <- vapply(
      dat,
      is.numeric,
      logical(1)
    )

    if (any(!is_num)) {
      warning(
        "비수치형 변수 제외: ",
        paste(
          names(dat)[!is_num],
          collapse = ", "
        )
      )
    }

    dat <- dat[
      ,
      is_num,
      drop = FALSE
    ]

    if (ncol(dat) == 0L) {
      stop("분석할 숫자형 변수가 없습니다.")
    }

    # Replace infinite values with NA
    for (j in seq_along(dat)) {
      dat[[j]][
        !is.finite(dat[[j]])
      ] <- NA_real_
    }

    # Remove incomplete observations
    n_before <- nrow(dat)

    dat <- stats::na.omit(dat)

    removed_n <- n_before - nrow(dat)

    if (removed_n > 0L) {
      warning(
        removed_n,
        "개 결측 사례를 제외했습니다."
      )
    }

    if (nrow(dat) < 3L) {
      stop("분석 가능한 사례가 부족합니다.")
    }

    # Remove zero-variance variables
    valid_var <- vapply(
      dat,
      function(z) {
        v <- stats::var(z)
        is.finite(v) && v > 0
      },
      logical(1)
    )

    if (any(!valid_var)) {
      warning(
        "분산이 0인 변수 제외: ",
        paste(
          names(dat)[!valid_var],
          collapse = ", "
        )
      )
    }

    dat <- dat[
      ,
      valid_var,
      drop = FALSE
    ]

    if (ncol(dat) == 0L) {
      stop("유효한 분석 변수가 없습니다.")
    }

    # Validate profile numbers
    profiles <- sort(
      unique(as.integer(n_profiles_range))
    )

    profiles <- profiles[
      !is.na(profiles) &
        profiles >= 1 &
        profiles <= nrow(dat)
    ]

    if (length(profiles) == 0L) {
      stop("유효한 프로파일 수가 없습니다.")
    }

    n_used <- nrow(dat)
    variables <- names(dat)

    # Fit Gaussian mixture models
    message("Fitting LPA models ...")

    bic <- mclust::mclustBIC(
      data = as.matrix(dat),
      G = profiles,
      modelNames = selected_models,
      verbose = FALSE
    )

    message("LPA fitting completed.")
  }

  # =================================================
  # 5. Convert BIC matrix to long format
  # =================================================

  bic_matrix <- as.matrix(bic)

  profile_ids <- suppressWarnings(
    as.integer(
      rownames(bic_matrix)
    )
  )

  model_names <- colnames(bic_matrix)

  if (is.null(model_names) ||
      anyNA(profile_ids) ||
      length(profile_ids) != nrow(bic_matrix)) {
    stop(
      "BIC 행렬의 프로파일 또는 ",
      "모형명이 올바르지 않습니다."
    )
  }

  # Automatically create covariance model column
  to_plot <- expand.grid(
    n_profiles = profile_ids,
    Covariance_matrix_structure = model_names,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  # Raw mclust BIC
  to_plot$BIC_mclust <- as.vector(bic_matrix)

  # Conventional BIC
  to_plot$BIC_standard <- -to_plot$BIC_mclust

  # Keep valid estimates
  to_plot <- to_plot[
    is.finite(to_plot$BIC_mclust),
    ,
    drop = FALSE
  ]

  if (nrow(to_plot) == 0L) {
    stop("유효한 BIC 추정 결과가 없습니다.")
  }

  # =================================================
  # 6. Set BIC orientation
  # =================================================

  if (bic_rev) {

    to_plot$val <- to_plot$BIC_standard

    y_title <- "BIC (smaller is better)"
    bic_method <- "Conventional BIC"

  } else {

    to_plot$val <- to_plot$BIC_mclust

    y_title <- "mclust BIC (larger is better)"
    bic_method <- "mclust BIC"
  }

  # =================================================
  # 7. Determine optimal model
  # =================================================

  selected_profile <- NULL

  if (!is.null(set_n_profile)) {

    selected_profile <- set_n_profile

  } else if (
    !is.null(xintercept) &&
    xintercept %in% to_plot$n_profiles
  ) {

    selected_profile <- xintercept
  }

  if (!is.null(selected_profile)) {

    candidates <- to_plot[
      to_plot$n_profiles == selected_profile,
      ,
      drop = FALSE
    ]

    if (nrow(candidates) == 0L) {
      stop(
        "선택한 프로파일 수에 ",
        "유효한 BIC 값이 없습니다."
      )
    }

  } else {

    candidates <- to_plot
  }

  # Conventional BIC: minimum
  # mclust BIC: maximum
  best_idx <- if (bic_rev) {
    which.min(candidates$val)
  } else {
    which.max(candidates$val)
  }

  best_model <- candidates[
    best_idx,
    ,
    drop = FALSE
  ]

  # =================================================
  # 8. Wide BIC table
  # =================================================

  to_plot_wide <- stats::reshape(
    to_plot[
      ,
      c(
        "n_profiles",
        "Covariance_matrix_structure",
        "val"
      )
    ],
    idvar = "n_profiles",
    timevar = "Covariance_matrix_structure",
    direction = "wide"
  )

  names(to_plot_wide) <- sub(
    "^val\\.",
    "",
    names(to_plot_wide)
  )

  to_plot_wide <- to_plot_wide[
    order(to_plot_wide$n_profiles),
    ,
    drop = FALSE
  ]

  rownames(to_plot_wide) <- NULL

  # =================================================
  # 9. Base ggplot
  # =================================================

  gg <- ggplot2::ggplot(
    to_plot,
    ggplot2::aes(
      x = n_profiles,
      y = val,
      color = Covariance_matrix_structure,
      group = Covariance_matrix_structure
    )
  ) +
    ggplot2::geom_line(
      linewidth = 1
    ) +
    ggplot2::geom_point(
      size = 2
    ) +
    ggplot2::labs(
      x = "Number of Profiles",
      y = y_title,
      color = "Covariance Model",
      subtitle = if (bic_rev) {
        "BIC = -2 log(L) + k log(n)"
      } else {
        "BIC = 2 log(L) - k log(n)"
      }
    ) +
    ggplot2::scale_x_continuous(
      breaks = sort(
        unique(to_plot$n_profiles)
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = legend_position,
      plot.subtitle = ggplot2::element_text(
        size = 10,
        color = "gray40"
      )
    )

  # =================================================
  # 10. Vertical reference line
  # =================================================

  if (!is.null(xintercept)) {

    gg <- gg +
      ggplot2::geom_vline(
        xintercept = xintercept,
        linetype = "dashed",
        color = "tomato"
      )
  }

  # =================================================
  # 11. BIC value labels
  # =================================================

  if (bic_text) {

    # Exclude the best point from ordinary labels.
    # The best point receives a separate red label.
    label_data <- to_plot[
      !(
        to_plot$n_profiles == best_model$n_profiles[1] &
          to_plot$Covariance_matrix_structure ==
          best_model$Covariance_matrix_structure[1]
      ),
      ,
      drop = FALSE
    ]

    if (nrow(label_data) > 0L) {

      if (requireNamespace("ggrepel", quietly = TRUE)) {

        gg <- gg +
          ggrepel::geom_text_repel(
            data = label_data,
            ggplot2::aes(
              label = round(val, 1)
            ),
            size = size.text,
            max.overlaps = Inf,
            show.legend = FALSE
          )

      } else {

        gg <- gg +
          ggplot2::geom_text(
            data = label_data,
            ggplot2::aes(
              label = round(val, 1)
            ),
            size = size.text,
            vjust = -0.8,
            check_overlap = TRUE,
            show.legend = FALSE
          )
      }
    }
  }

  # =================================================
  # 12. Highlight optimal BIC with red arrow
  # =================================================

  bx <- best_model$n_profiles[1]
  by <- best_model$val[1]

  bm <- best_model$Covariance_matrix_structure[1]

  x_range <- range(to_plot$n_profiles)
  y_range <- range(to_plot$val)

  x_span <- diff(x_range)
  y_span <- diff(y_range)

  if (x_span == 0) {
    x_span <- 1
  }

  if (y_span == 0) {
    y_span <- max(
      1,
      abs(by) * 0.05
    )
  }

  x_offset <- 0.35 * x_span /
    max(
      1,
      length(unique(to_plot$n_profiles)) - 1
    )

  y_offset <- 0.12 * y_span

  # Annotation position
  arrow_x <- if (bx >= mean(x_range)) {
    bx - x_offset
  } else {
    bx + x_offset
  }

  arrow_y <- if (bic_rev) {
    by + y_offset
  } else {
    by - y_offset
  }

  best_label <- paste0(
    "Best: ", bm,
    "\nBIC = ", round(by, 1)
  )

  # Highlight optimal point
  gg <- gg +
    ggplot2::geom_point(
      data = best_model,
      mapping = ggplot2::aes(
        x = n_profiles,
        y = val
      ),
      inherit.aes = FALSE,
      shape = 21,
      fill = "red",
      color = "darkred",
      size = 4,
      stroke = 1
    )

  # Red arrow
  gg <- gg +
    ggplot2::annotate(
      "segment",
      x = arrow_x,
      y = arrow_y,
      xend = bx,
      yend = by,
      color = "red",
      linewidth = 0.9,
      arrow = grid::arrow(
        length = grid::unit(0.25, "cm"),
        type = "closed"
      )
    )

  # =================================================
  # 13. Optimal model label
  #     ggplot2 4.x: linewidth
  # =================================================

  gg <- gg +
    ggplot2::annotate(
      "label",
      x = arrow_x,
      y = arrow_y,
      label = best_label,
      color = "red3",
      fill = "white",
      size = size.text,
      fontface = "bold",
      hjust = 0.5,
      vjust = if (bic_rev) 0 else 1,
      linewidth = 0.3
    )

  # =================================================
  # 14. Optimal model summary
  # =================================================

  best_summary <- data.frame(
    n_profiles = best_model$n_profiles,
    Covariance_matrix_structure =
      best_model$Covariance_matrix_structure,
    BIC = best_model$val,
    BIC_standard = best_model$BIC_standard,
    BIC_mclust = best_model$BIC_mclust,
    criterion = bic_method,
    stringsAsFactors = FALSE
  )

  # =================================================
  # 15. Diagnostics
  # =================================================

  diagnostics <- list(
    bic_method = bic_method,
    bic_rev = bic_rev,
    bic_text = bic_text,
    selected_profile = selected_profile,
    selected_models = model_names,
    available_profiles = sort(
      unique(to_plot$n_profiles)
    ),
    n = n_used,
    variables = variables,
    removed_missing = removed_n,
    best_model = best_summary
  )

  # =================================================
  # 16. Return results
  # =================================================

  res <- list(
    data = to_plot_wide,
    wide_data = to_plot_wide,
    to_plot = to_plot,
    long_data = to_plot,
    graph = gg,
    best_model = best_summary,
    bic_object = bic,
    diagnostics = diagnostics
  )

  # Open a new graphics device if requested
  if (new.window && interactive()) {
    grDevices::dev.new()
    print(gg)
  }

  # Select output component
  result <- switch(
    type,
    all = res,
    res = res,
    data = to_plot_wide,
    wide_data = to_plot_wide,
    long_data = to_plot,
    graph = gg,
    gg = gg,
    best_model = best_summary,
    diagnostics = diagnostics,
    stop(
      "type은 all, res, data, wide_data, ",
      "long_data, graph, gg, best_model, ",
      "diagnostics 중 하나여야 합니다."
    )
  )

  return(result)
}
