#' Automated Johnson-Neyman and Simple Slopes Analysis
#'
#' @param model An lm object with an interaction between a two-level factor or a numeric 0/1 predictor and a continuous moderator.
#' @param pred Name of the two-level factor or numeric 0/1 predictor, or its expanded coefficient name (e.g., 'GroupTreatment').
#' @param modx Name of the continuous moderator (character string).
#' @param alpha Significance level.
#' @param modx.values Moderator values at which to estimate conditional effects; defaults to mean and +/- 1 SD.
#' @param mod.range Plotting range for the moderator; defaults to the observed range.
#' @param digits Number of decimal places used in formatted output.
#' @param plot Whether to generate a plot.
#' @param n.grid Number of grid points used to draw the plot.
#' @param jn.label JN label placement: 'axis' (near the bottom of the plot) or 'repel' (using ggrepel).
#' @param grayscale Whether to use a grayscale-friendly plot.
#' @param legend.size Font size (pt) of legend labels and legend title.
#' @param x.title.size Font size (pt) of the x-axis title.
#' @param y.title.size Font size (pt) of the y-axis title.
#' @param x.text.size Font size (pt) of the x-axis tick labels.
#' @param y.text.size Font size (pt) of the y-axis tick labels.
#' @param caption.size Font size (pt) of the plot caption.
#' @param console.explain Print the Johnson-Neyman interpretation when printing the plot.
#' @param show.data Print the model-derived Y/G/X analysis data when printing results.
#' @param auto.print Automatically print the analysis tables, interpretation, and plot when the function is called.
#' @param original.modx Original (uncentered) moderator variable name or a numeric vector aligned to the model rows. NULL attempts to detect a variable by removing the '_c' suffix from modx.
#' @param original.mean Optional known original-scale mean. Used only when original.modx cannot be resolved; must represent the centering constant.
#' @param centering.tol Maximum permitted discrepancy when verifying that modx equals original.modx minus its mean.
#' @return A list containing model, model_summary, model_data (Y/G/X), interaction,
#'   slopes, bounds, regions, original_bounds, original_regions, curve, plot, and interpretation.
#' @export
jn_analysis <- function(model, pred = "G", modx = "X", alpha = 0.05,
                        modx.values = NULL, mod.range = NULL,
                        digits = 3, plot = TRUE, n.grid = 501,
                        jn.label = c("axis", "repel"), grayscale = TRUE,
                        legend.size = 10, x.title.size = 12, y.title.size = 12,
                        x.text.size = 10, y.text.size = 10, caption.size = 9,
                        console.explain = TRUE, show.data = TRUE, auto.print = TRUE,
                        original.modx = NULL, original.mean = NULL, centering.tol = 1e-6) {
  jn.label <- match.arg(jn.label)
  font_sizes <- list(legend.size = legend.size, x.title.size = x.title.size,
                     y.title.size = y.title.size, x.text.size = x.text.size,
                     y.text.size = y.text.size, caption.size = caption.size)
  if (any(!vapply(font_sizes, function(z) is.numeric(z) && length(z) == 1L &&
                  is.finite(z) && z > 0, logical(1))))
    stop("All font size arguments must be positive finite numbers.")
  if (!inherits(model, "lm")) stop("model must be an lm object.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("The ggplot2 package is required.")
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0 || alpha >= 1)
    stop("alpha must be strictly between 0 and 1.")
  mf <- stats::model.frame(model)
  if (!modx %in% names(mf))
    stop("modx must be a variable in model.frame(model).")
  cf <- stats::coef(model)
  V <- stats::vcov(model)
  if (anyNA(cf)) stop("Model coefficients contain NA values (singular fit).")
  terms_obj <- stats::terms(model)
  tl <- attr(terms_obj, "term.labels")
  interaction_terms <- tl[grepl(":", tl, fixed = TRUE)]
  # Accept either the original factor variable name or its expanded coefficient name.
  pred_var <- pred
  if (!pred_var %in% names(mf)) {
    candidates <- setdiff(names(mf), modx)
    candidates <- candidates[vapply(candidates, function(z) {
      is.factor(mf[[z]]) || is.character(mf[[z]]) || is.logical(mf[[z]]) ||
        (is.numeric(mf[[z]]) && length(unique(mf[[z]])) == 2L)
    }, logical(1))]
    candidates <- candidates[vapply(candidates, function(z) {
      any(vapply(strsplit(interaction_terms, ":", fixed = TRUE),
                 function(parts) length(parts) == 2L && setequal(parts, c(z, modx)),
                 logical(1))) && startsWith(pred, z)
    }, logical(1))]
    if (length(candidates) != 1L)
      stop("Cannot resolve pred to a binary variable. Supply the variable name or its expanded coefficient name.")
    pred_var <- candidates
  }
  g <- mf[[pred_var]]
  x <- mf[[modx]]
  group_raw <- g
  if (is.factor(g) || is.character(g) || is.logical(g)) {
    g <- factor(g)
    if (nlevels(g) != 2L) stop("pred must have exactly two levels.")
    group_labels <- levels(g)
  } else if (is.numeric(g) && setequal(unique(g), c(0, 1))) {
    group_labels <- c("0", "1")
  } else {
    stop("pred must be a two-level factor or a numeric 0/1 variable.")
  }
  # Model-derived analysis data, aligned to the exact rows used by lm().
  # Factor: reference level = 0; second level = 1.
  group_code <- if (is.numeric(group_raw)) as.integer(group_raw) else
    as.integer(factor(group_raw, levels = group_labels) == group_labels[2])
  model_data <- data.frame(Y = as.numeric(stats::model.response(mf)),
                           G = group_code, X = as.numeric(x))
  if (!is.numeric(x) || !is.finite(stats::sd(x)) || stats::sd(x) <= 0)
    stop("modx must be a numeric continuous variable with nonzero variance.")
  int_parts <- strsplit(interaction_terms, ":", fixed = TRUE)
  hit <- which(vapply(int_parts, function(parts)
    length(parts) == 2L && setequal(parts, c(pred_var, modx)), logical(1)))
  if (length(hit) != 1L)
    stop("The model must contain exactly one interaction between pred and modx.")
  # Match coefficients from the original model matrix, including factor expansion.
  mm <- stats::model.matrix(model)
  assign_id <- attr(mm, "assign")
  main_id <- match(pred_var, tl)
  int_id <- match(interaction_terms[hit], tl)
  main_cols <- colnames(mm)[assign_id == main_id]
  int_cols <- colnames(mm)[assign_id == int_id]
  if (length(main_cols) != 1L || length(int_cols) != 1L)
    stop("The binary predictor must contribute exactly one main-effect and one interaction coefficient.")
  a <- main_cols
  b <- int_cols
  if (!a %in% names(cf) || !b %in% names(cf))
    stop("Could not match model coefficients to the binary predictor and interaction.")
  if (pred != pred_var && pred != a)
    stop(paste0("The supplied coefficient name does not match the fitted model. Expected: ", a))
  df <- stats::df.residual(model)
  if (df <= 0) stop("Residual degrees of freedom must be positive.")
  crit <- stats::qt(1 - alpha / 2, df)
  B0 <- unname(cf[a]); B1 <- unname(cf[b])
  v00 <- V[a, a]; v01 <- V[a, b]; v11 <- V[b, b]
  calc <- function(z) {
    est <- B0 + B1 * z
    se <- sqrt(pmax(0, v00 + 2 * v01 * z + v11 * z^2))
    tval <- ifelse(se > 0, est / se, NA_real_)
    pval <- 2 * stats::pt(-abs(tval), df)
    data.frame(X = z, B = est, SE = se, CI.lower = est - crit * se,
               CI.upper = est + crit * se, t = tval, df = df, p = pval,
               significant = !is.na(pval) & pval < alpha)
  }
  m <- mean(x); s <- stats::sd(x)
  if (is.null(modx.values)) modx.values <- c(m - s, m, m + s)
  if (!is.numeric(modx.values) || any(!is.finite(modx.values)))
    stop("modx.values must contain finite numeric values.")
  slopes <- calc(modx.values)
  slopes$level <- if (length(modx.values) == 3L &&
                      isTRUE(all.equal(as.numeric(modx.values), c(m-s,m,m+s))))
    c("-1 SD", "Mean", "+1 SD") else paste0("X=", round(modx.values, digits))
  slopes$CI95 <- sprintf(paste0("[%." , digits, "f, %." , digits, "f]"),
                         slopes$CI.lower, slopes$CI.upper)
  slopes <- slopes[, c("level", "X", "B", "SE", "t", "df", "p", "CI95", "significant")]
  if (is.null(mod.range)) mod.range <- range(x)
  if (!is.numeric(mod.range) || length(mod.range) != 2L ||
      any(!is.finite(mod.range)) || mod.range[1] >= mod.range[2])
    stop("mod.range must contain two finite numbers in ascending order.")
  # Recover the original moderator scale only when centering can be verified.
  original_source <- NULL
  original_values <- NULL
  centering_constant <- NA_real_
  if (!is.numeric(centering.tol) || length(centering.tol) != 1L ||
      !is.finite(centering.tol) || centering.tol < 0)
    stop("centering.tol must be a finite non-negative number.")
  if (!is.null(original.modx)) {
    if (is.character(original.modx) && length(original.modx) == 1L) {
      if (original.modx %in% names(mf)) {
        original_values <- mf[[original.modx]]
      } else {
        original_values <- tryCatch(
          eval(as.name(original.modx), envir = environment(stats::formula(model))),
          error = function(e) NULL)
        if (is.null(original_values))
          stop("original.modx was not found in the model frame or formula environment.")
      }
      original_source <- original.modx
    } else if (is.numeric(original.modx)) {
      original_values <- original.modx
      original_source <- "supplied numeric vector"
    } else stop("original.modx must be a variable name or numeric vector.")
  } else if (grepl("_c$", modx)) {
    candidate <- sub("_c$", "", modx)
    if (candidate %in% names(mf)) {
      original_values <- mf[[candidate]]
      original_source <- candidate
    } else {
      original_values <- tryCatch(
        eval(as.name(candidate), envir = environment(stats::formula(model))),
        error = function(e) NULL)
      if (!is.null(original_values)) original_source <- candidate
    }
  }
  # Search the original data argument used by lm(), including data = dat.
  if (is.null(original_values) && is.null(original.modx) && grepl("_c$", modx)) {
    candidate <- sub("_c$", "", modx)
    data_source <- tryCatch(eval(model$call$data,
      envir = environment(stats::formula(model))), error = function(e) NULL)
    if (is.data.frame(data_source) && candidate %in% names(data_source)) {
      original_values <- data_source[[candidate]]
      original_source <- candidate
    }
  }
  if (!is.null(original_values)) {
    if (!is.numeric(original_values))
      stop("The original moderator must be numeric.")
    # lm() may drop rows due to missing values: align named full-length vectors
    # using model.frame row names when possible.
    if (length(original_values) != length(x)) {
      row_ids <- suppressWarnings(as.integer(rownames(mf)))
      if (length(row_ids) == length(x) && all(!is.na(row_ids)) &&
          length(original_values) >= max(row_ids)) {
        original_values <- original_values[row_ids]
      } else {
        stop("The original moderator cannot be aligned to the model rows. Supply an aligned numeric vector.")
      }
    }
    if (any(!is.finite(original_values)))
      stop("The original moderator contains non-finite values in model rows.")
    offsets <- original_values - as.numeric(x)
    if (max(abs(offsets - mean(offsets))) > centering.tol)
      stop("Original and centered moderator values do not differ by a constant; verify original.modx.")
    centering_constant <- mean(offsets)
    if (!is.null(original.mean) &&
        (!isTRUE(all.equal(as.numeric(original.mean), centering_constant,
                           tolerance = centering.tol))))
      stop("original.mean does not match the centering constant derived from original.modx.")
  } else if (!is.null(original.mean)) {
    if (!is.numeric(original.mean) || length(original.mean) != 1L ||
        !is.finite(original.mean)) stop("original.mean must be a finite number.")
    centering_constant <- original.mean
    original_source <- "original.mean argument"
  }
  # (B0+B1*x)^2 - tcrit^2*(v00+2*v01*x+v11*x^2)=0
  A <- B1^2 - crit^2 * v11
  B <- 2 * (B0 * B1 - crit^2 * v01)
  C <- B0^2 - crit^2 * v00
  tol <- 1e-10 * max(1, abs(A), abs(B), abs(C))
  if (abs(A) < tol) {
    roots <- if (abs(B) < tol) numeric() else -C / B
  } else {
    disc <- B^2 - 4 * A * C
    roots <- if (disc < -tol) numeric() else if (abs(disc) <= tol)
      -B / (2 * A) else sort(c((-B-sqrt(disc))/(2*A), (-B+sqrt(disc))/(2*A)))
  }
  roots <- sort(unique(roots[is.finite(roots)]))
  bounds <- data.frame(bound = roots, in_observed_range = roots >= min(x) & roots <= max(x),
                       in_plot_range = roots >= mod.range[1] & roots <= mod.range[2])
  original_bounds <- bounds
  original_bounds$original_score <- if (is.finite(centering_constant))
    bounds$bound + centering_constant else rep(NA_real_, nrow(bounds))
  cuts <- sort(unique(c(mod.range[1], roots[roots > mod.range[1] & roots < mod.range[2]], mod.range[2])))
  regions <- data.frame(from = head(cuts, -1), to = tail(cuts, -1))
  regions$significant <- calc((regions$from + regions$to) / 2)$significant
  regions$label <- ifelse(regions$significant, "Significant", "Not significant")
  original_regions <- regions
  original_regions$from_original <- if (is.finite(centering_constant))
    regions$from + centering_constant else rep(NA_real_, nrow(regions))
  original_regions$to_original <- if (is.finite(centering_constant))
    regions$to + centering_constant else rep(NA_real_, nrow(regions))
  original_regions$interpretation <- ifelse(
    regions$significant,
    "Group difference is statistically significant",
    "Group difference is not statistically significant")
  curve <- calc(seq(mod.range[1], mod.range[2], length.out = n.grid))
  p <- NULL
  if (isTRUE(plot)) {
    # Background: significant = light gray; not significant = white with diagonal hatching.
    # Hatching provides a distinction that remains visible in grayscale printing.
    fills <- if (isTRUE(grayscale)) c("Significant" = "grey88", "Not significant" = "white")
             else c("Significant" = "#D6E8F3", "Not significant" = "#FFF4E8")
    yr <- range(c(curve$CI.lower, curve$CI.upper, 0), finite = TRUE)
    pad <- max(diff(yr) * 0.08, 0.2)
    ymin <- yr[1] - pad
    ymax <- yr[2] + pad
    # Add regularly spaced diagonal lines in nonsignificant regions (no extra package required).
    nonsig <- regions[!regions$significant, , drop = FALSE]
    hatch <- data.frame(x = numeric(), xend = numeric(), y = numeric(), yend = numeric())
    if (nrow(nonsig)) {
      dx <- diff(mod.range) / 38
      dy <- diff(c(ymin, ymax)) / 13
      slope <- dy / dx
      for (i in seq_len(nrow(nonsig))) {
        lo <- nonsig$from[i]; hi <- nonsig$to[i]
        if (hi <= lo) next
        starts <- seq(lo - (ymax - ymin) / slope, hi, by = dx * 2.4)
        for (st in starts) {
          x0 <- max(lo, st); x1 <- min(hi, st + (ymax - ymin) / slope)
          if (x1 <= x0) next
          y0 <- ymin + (x0 - st) * slope
          y1 <- ymin + (x1 - st) * slope
          hatch <- rbind(hatch, data.frame(x = x0, xend = x1, y = y0, yend = y1))
        }
      }
    }
    # Draw diagonal hatching inside the legend key for non-significant regions.
    # The key is based on the mapped fill, so it matches the plot in grayscale.
    draw_jn_legend_key <- function(data, params, size) {
      key_fill <- data$fill[1]
      base <- grid::rectGrob(
        gp = grid::gpar(fill = key_fill, col = "grey60", lwd = 0.5)
      )
      if (!isTRUE(grayscale) || !identical(tolower(key_fill), "#ffffff") &&
          !identical(tolower(key_fill), "white")) return(base)
      # Short, parallel diagonal strokes remain within the legend rectangle.
      strokes <- grid::segmentsGrob(
        x0 = grid::unit(c(0.00, 0.00, 0.25, 0.55, 0.85), "npc"),
        y0 = grid::unit(c(0.30, 0.80, 0.00, 0.00, 0.00), "npc"),
        x1 = grid::unit(c(0.30, 0.80, 1.00, 1.00, 1.00), "npc"),
        y1 = grid::unit(c(1.00, 1.00, 1.00, 0.70, 0.20), "npc"),
        gp = grid::gpar(col = "grey35", lwd = 0.75)
      )
      grid::grobTree(base, strokes)
    }
    p <- ggplot2::ggplot(curve, ggplot2::aes(x = X, y = B)) +
      ggplot2::geom_rect(data = regions, ggplot2::aes(xmin = from, xmax = to,
          ymin = -Inf, ymax = Inf, fill = label), inherit.aes = FALSE,
          key_glyph = draw_jn_legend_key) +
      ggplot2::scale_fill_manual(values = fills, name = "Significance region", drop = FALSE) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = CI.lower, ymax = CI.upper),
                           fill = "grey55", alpha = 0.23) +
      ggplot2::geom_line(linewidth = 0.95, color = "black") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey35") +
      ggplot2::scale_y_continuous(limits = c(ymin, ymax), expand = ggplot2::expansion(mult = 0)) +
      ggplot2::labs(title = "Johnson-Neyman Regions of Significance",
                    subtitle = paste0("Conditional Group Difference and ", round((1-alpha)*100), "% Confidence Interval"),
                    x = modx, y = paste0("Conditional ", pred_var, " Effect (", group_labels[2], " - ", group_labels[1], ")")) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "bottom",
                     plot.caption = ggplot2::element_text(hjust = 0, size = caption.size),
                     legend.text = ggplot2::element_text(size = legend.size),
                     legend.title = ggplot2::element_text(size = legend.size),
                     axis.title.x = ggplot2::element_text(size = x.title.size),
                     axis.title.y = ggplot2::element_text(size = y.title.size),
                     axis.text.x = ggplot2::element_text(size = x.text.size),
                     axis.text.y = ggplot2::element_text(size = y.text.size),
                     panel.grid.minor = ggplot2::element_blank())
    if (nrow(hatch)) {
      p <- p + ggplot2::geom_segment(data = hatch,
        ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
        inherit.aes = FALSE, color = "grey65", linewidth = 0.23)
    }
    shown <- bounds$bound[bounds$in_plot_range]
    if (length(shown)) {
      ann <- calc(shown)
      # Add the original-scale moderator threshold when the centering constant is known.
      ann$label <- paste0(
        "JN = ", formatC(ann$X, format = "f", digits = digits),
        if (is.finite(centering_constant)) {
          paste0(" (raw = ",
                 formatC(ann$X + centering_constant, format = "f", digits = digits),
                 ")")
        } else {
          ""
        }
      )
      p <- p + ggplot2::geom_vline(xintercept = shown, linetype = "dotdash",
                                   color = "black", linewidth = 0.65)
      if (identical(jn.label, "repel") && requireNamespace("ggrepel", quietly = TRUE)) {
        p <- p + ggrepel::geom_label_repel(data = ann,
          ggplot2::aes(x = X, y = B, label = label), inherit.aes = FALSE,
          size = 3.4, fill = "white", color = "black", seed = 123,
          box.padding = 0.6, max.overlaps = Inf)
      } else {
        # Place labels near the bottom of the plot to avoid overlapping the effect line.
        ann$label_y <- ymin + 0.055 * (ymax - ymin)
        p <- p + ggplot2::geom_label(data = ann,
          ggplot2::aes(x = X, y = label_y, label = label),
          inherit.aes = FALSE, size = 3.3, fill = "white", color = "black",
          label.size = 0.3)
      }
    }
    p <- p + ggplot2::geom_vline(xintercept = m, linetype = "dotted", color = "grey40")
    if (nrow(bounds)) {
      outside <- bounds$bound[!bounds$in_plot_range]
      cap <- paste0("All JN bounds: ", paste(formatC(bounds$bound, digits = digits, format = "f"), collapse = ", "))
      if (length(outside)) cap <- paste0(cap, " | Outside plot range: ",
          paste(formatC(outside, digits = digits, format = "f"), collapse = ", "))
      p <- p + ggplot2::labs(caption = cap)
    } else {
      p <- p + ggplot2::labs(caption = "No real-valued JN bounds")
    }
  }
  interaction <- data.frame(term = b, B = B1, SE = sqrt(v11),
                            t = B1 / sqrt(v11), df = df,
                            p = 2 * stats::pt(-abs(B1 / sqrt(v11)), df))
  # Explanation distinguishes observed support from extrapolation.
  format_num <- function(z) formatC(z, format = "f", digits = digits)
  region_text <- if (nrow(regions)) {
    paste(vapply(seq_len(nrow(regions)), function(i) {
      paste0("X in [", format_num(regions$from[i]), ", ",
             format_num(regions$to[i]), "]: ", regions$label[i])
    }, character(1)), collapse = "\n")
  } else "No regions available."
  bound_text <- if (nrow(bounds)) {
    paste(vapply(seq_len(nrow(bounds)), function(i) {
      paste0("JN = ", format_num(bounds$bound[i]),
             if (bounds$in_observed_range[i]) " (inside observed X range)"
             else " (outside observed X range; extrapolation)")
    }, character(1)), collapse = "\n")
  } else "No real-valued JN thresholds were found."
  original_text <- if (is.finite(centering_constant)) {
    region_lines <- paste(vapply(seq_len(nrow(original_regions)), function(i) {
      paste0("Original score in [", format_num(original_regions$from_original[i]),
             ", ", format_num(original_regions$to_original[i]), "]: ",
             original_regions$interpretation[i])
    }, character(1)), collapse = "\n")
    paste0("\n[Original-Scale Moderator Interpretation]\n",
           "Original moderator: ", original_source, "\n",
           "Centering constant = ", format_num(centering_constant), "\n",
           "Original score = centered score + centering constant\n",
           "JN thresholds on original scale: ",
           if (nrow(original_bounds)) paste(format_num(original_bounds$original_score),
                                             collapse = ", ") else "none", "\n",
           "Significance within the observed moderator range:\n", region_lines, "\n",
           "A threshold marks statistical significance, not a deterministic educational cutoff.\n")
  } else {
    paste0("\n[Original-Scale Moderator Interpretation]\n",
           "Original scores unavailable: supply original.modx or original.mean.\n",
           "Do not interpret centered JN thresholds as raw scores.\n")
  }
  explanation <- paste0(
    "[Johnson-Neyman Interpretation]\n",
    "Predictor: ", pred_var, " (", group_labels[2], " = 1; ",
    group_labels[1], " = 0)\n",
    "Moderator: ", modx, "\n",
    "Moderator mean = ", format_num(m), "; SD = ", format_num(s),
    "; observed range = [", format_num(min(x)), ", ", format_num(max(x)), "]\n",
    "Alpha = ", format_num(alpha), "; confidence level = ",
    format_num(100 * (1 - alpha)), "%\n",
    "Conditional group difference = ", format_num(B0), " + (",
    format_num(B1), ") * X\n",
    "JN thresholds:\n", bound_text, "\n",
    "Significance within the plotted moderator range:\n", region_text, "\n",
    "The regions describe two-sided tests of the conditional group difference. ",
    "A non-significant result does not establish equivalence. ",
    "Thresholds outside the observed moderator range require extrapolation.\n",
    original_text
  )
  if (!is.null(p) && isTRUE(console.explain)) {
    # A dedicated S3 print method prints the interpretation whenever print(plot) is called.
    class(p) <- c("jn_plot", class(p))
    attr(p, "jn_interpretation") <- explanation
  }
  out <- list(model = model, model_summary = summary(model),
              model_data = model_data, data = model_data,
              interpretation = explanation,
              show.data = show.data,
              interaction = interaction, slopes = slopes,
              bounds = bounds, regions = regions, original_bounds = original_bounds,
              original_regions = original_regions, original_mean = centering_constant,
              original_source = original_source, curve = curve, plot = p,
              range = range(x), mean = m, sd = s, alpha = alpha,
              predictor = pred_var, group_levels = group_labels)
  class(out) <- c("jn_analysis", "list")
  if (isTRUE(auto.print)) {
    print(out)
    if (!is.null(out$plot)) {
      # Use the regular ggplot print method to avoid repeating the interpretation.
      plot_to_print <- out$plot
      class(plot_to_print) <- setdiff(class(plot_to_print), "jn_plot")
      attr(plot_to_print, "jn_interpretation") <- NULL
      print(plot_to_print)
    }
  }
  invisible(out)
}

#' @export
print.jn_analysis <- function(x, digits = 3, ...) {
  cat("\n[Model Summary]\n")
  print(x$model_summary)
  if (isTRUE(x$show.data)) {
    cat("\n[Model-Derived Analysis Data: Y / G / X]\n")
    print(x$model_data, row.names = FALSE)
  }
  cat("\n[Interaction Term Test]\n")
  print(x$interaction, row.names = FALSE)
  cat("\n[Simple Slopes / Conditional Group Difference]\n")
  tab <- x$slopes
  numeric_cols <- vapply(tab, is.numeric, logical(1))
  tab[numeric_cols] <- lapply(tab[numeric_cols], round, digits = digits)
  tab$p <- ifelse(x$slopes$p < .001, "< .001", sprintf("%.*f", digits, x$slopes$p))
  print(tab, row.names = FALSE)
  cat("\n[Johnson-Neyman Bounds]\n")
  if (nrow(x$bounds)) print(x$bounds, row.names = FALSE) else cat("No real-valued bounds\n")
  cat("\n[Regions of Significance Within Plot Range]\n")
  print(x$regions, row.names = FALSE)
  cat("\n[Original-Scale Johnson-Neyman Bounds]\n")
  print(x$original_bounds, row.names = FALSE)
  cat("\n[Original-Scale Regions of Significance]\n")
  print(x$original_regions, row.names = FALSE)
  cat("\n", x$interpretation, sep = "")
  invisible(x)
}

#' Print a Johnson-Neyman plot and its interpretation
#' @param x A jn_plot object.
#' @param ... Additional graphical printing arguments.
#' @export
print.jn_plot <- function(x, ...) {
  explanation <- attr(x, "jn_interpretation")
  if (!is.null(explanation)) cat("\n", explanation, sep = "")
  class(x) <- class(x)[class(x) != "jn_plot"]
  attr(x, "jn_interpretation") <- NULL
  print(x, ...)
  invisible(x)
}




#' Automated Johnson-Neyman and Simple Slopes Analysis
#'
#' @param model An lm object with an interaction between a two-level factor or a numeric 0/1 predictor and a continuous moderator.
#' @param pred Name of the two-level factor or numeric 0/1 predictor, or its expanded coefficient name (e.g., 'GroupTreatment').
#' @param modx Name of the continuous moderator (character string).
#' @param alpha Significance level.
#' @param modx.values Moderator values at which to estimate conditional effects; defaults to mean and +/- 1 SD.
#' @param mod.range Plotting range for the moderator; defaults to the observed range.
#' @param digits Number of decimal places used in formatted output.
#' @param plot Whether to generate a plot.
#' @param n.grid Number of grid points used to draw the plot.
#' @param jn.label JN label placement: 'axis' (near the bottom of the plot) or 'repel' (using ggrepel).
#' @param grayscale Whether to use a grayscale-friendly plot.
#' @param legend.size Font size (pt) of legend labels and legend title.
#' @param x.title.size Font size (pt) of the x-axis title.
#' @param y.title.size Font size (pt) of the y-axis title.
#' @param x.text.size Font size (pt) of the x-axis tick labels.
#' @param y.text.size Font size (pt) of the y-axis tick labels.
#' @param caption.size Font size (pt) of the plot caption.
#' @param console.explain Print the Johnson-Neyman interpretation when printing the plot.
#' @param show.data Print the model-derived Y/G/X analysis data when printing results.
#' @param auto.print Automatically print the analysis tables, interpretation, and plot when the function is called.
#' @param original.modx Original (uncentered) moderator variable name or a numeric vector aligned to the model rows. NULL attempts to detect a variable by removing the '_c' suffix from modx.
#' @param original.mean Optional known original-scale mean. Used only when original.modx cannot be resolved; must represent the centering constant.
#' @param centering.tol Maximum permitted discrepancy when verifying that modx equals original.modx minus its mean.
#' @return A list containing model, model_summary, model_data (Y/G/X), interaction,
#'   slopes, bounds, regions, original_bounds, original_regions, curve, plot, and interpretation.
#' @export
Johnson_Neyman <- function(model, pred = "G", modx = "X", alpha = 0.05,
                        modx.values = NULL, mod.range = NULL,
                        digits = 3, plot = TRUE, n.grid = 501,
                        jn.label = c("axis", "repel"), grayscale = TRUE,
                        legend.size = 10, x.title.size = 12, y.title.size = 12,
                        x.text.size = 10, y.text.size = 10, caption.size = 9,
                        console.explain = TRUE, show.data = TRUE, auto.print = TRUE,
                        original.modx = NULL, original.mean = NULL, centering.tol = 1e-6) {
  jn.label <- match.arg(jn.label)
  font_sizes <- list(legend.size = legend.size, x.title.size = x.title.size,
                     y.title.size = y.title.size, x.text.size = x.text.size,
                     y.text.size = y.text.size, caption.size = caption.size)
  if (any(!vapply(font_sizes, function(z) is.numeric(z) && length(z) == 1L &&
                  is.finite(z) && z > 0, logical(1))))
    stop("All font size arguments must be positive finite numbers.")
  if (!inherits(model, "lm")) stop("model must be an lm object.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("The ggplot2 package is required.")
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0 || alpha >= 1)
    stop("alpha must be strictly between 0 and 1.")
  mf <- stats::model.frame(model)
  if (!modx %in% names(mf))
    stop("modx must be a variable in model.frame(model).")
  cf <- stats::coef(model)
  V <- stats::vcov(model)
  if (anyNA(cf)) stop("Model coefficients contain NA values (singular fit).")
  terms_obj <- stats::terms(model)
  tl <- attr(terms_obj, "term.labels")
  interaction_terms <- tl[grepl(":", tl, fixed = TRUE)]
  # Accept either the original factor variable name or its expanded coefficient name.
  pred_var <- pred
  if (!pred_var %in% names(mf)) {
    candidates <- setdiff(names(mf), modx)
    candidates <- candidates[vapply(candidates, function(z) {
      is.factor(mf[[z]]) || is.character(mf[[z]]) || is.logical(mf[[z]]) ||
        (is.numeric(mf[[z]]) && length(unique(mf[[z]])) == 2L)
    }, logical(1))]
    candidates <- candidates[vapply(candidates, function(z) {
      any(vapply(strsplit(interaction_terms, ":", fixed = TRUE),
                 function(parts) length(parts) == 2L && setequal(parts, c(z, modx)),
                 logical(1))) && startsWith(pred, z)
    }, logical(1))]
    if (length(candidates) != 1L)
      stop("Cannot resolve pred to a binary variable. Supply the variable name or its expanded coefficient name.")
    pred_var <- candidates
  }
  g <- mf[[pred_var]]
  x <- mf[[modx]]
  group_raw <- g
  if (is.factor(g) || is.character(g) || is.logical(g)) {
    g <- factor(g)
    if (nlevels(g) != 2L) stop("pred must have exactly two levels.")
    group_labels <- levels(g)
  } else if (is.numeric(g) && setequal(unique(g), c(0, 1))) {
    group_labels <- c("0", "1")
  } else {
    stop("pred must be a two-level factor or a numeric 0/1 variable.")
  }
  # Model-derived analysis data, aligned to the exact rows used by lm().
  # Factor: reference level = 0; second level = 1.
  group_code <- if (is.numeric(group_raw)) as.integer(group_raw) else
    as.integer(factor(group_raw, levels = group_labels) == group_labels[2])
  model_data <- data.frame(Y = as.numeric(stats::model.response(mf)),
                           G = group_code, X = as.numeric(x))
  if (!is.numeric(x) || !is.finite(stats::sd(x)) || stats::sd(x) <= 0)
    stop("modx must be a numeric continuous variable with nonzero variance.")
  int_parts <- strsplit(interaction_terms, ":", fixed = TRUE)
  hit <- which(vapply(int_parts, function(parts)
    length(parts) == 2L && setequal(parts, c(pred_var, modx)), logical(1)))
  if (length(hit) != 1L)
    stop("The model must contain exactly one interaction between pred and modx.")
  # Match coefficients from the original model matrix, including factor expansion.
  mm <- stats::model.matrix(model)
  assign_id <- attr(mm, "assign")
  main_id <- match(pred_var, tl)
  int_id <- match(interaction_terms[hit], tl)
  main_cols <- colnames(mm)[assign_id == main_id]
  int_cols <- colnames(mm)[assign_id == int_id]
  if (length(main_cols) != 1L || length(int_cols) != 1L)
    stop("The binary predictor must contribute exactly one main-effect and one interaction coefficient.")
  a <- main_cols
  b <- int_cols
  if (!a %in% names(cf) || !b %in% names(cf))
    stop("Could not match model coefficients to the binary predictor and interaction.")
  if (pred != pred_var && pred != a)
    stop(paste0("The supplied coefficient name does not match the fitted model. Expected: ", a))
  df <- stats::df.residual(model)
  if (df <= 0) stop("Residual degrees of freedom must be positive.")
  crit <- stats::qt(1 - alpha / 2, df)
  B0 <- unname(cf[a]); B1 <- unname(cf[b])
  v00 <- V[a, a]; v01 <- V[a, b]; v11 <- V[b, b]
  calc <- function(z) {
    est <- B0 + B1 * z
    se <- sqrt(pmax(0, v00 + 2 * v01 * z + v11 * z^2))
    tval <- ifelse(se > 0, est / se, NA_real_)
    pval <- 2 * stats::pt(-abs(tval), df)
    data.frame(X = z, B = est, SE = se, CI.lower = est - crit * se,
               CI.upper = est + crit * se, t = tval, df = df, p = pval,
               significant = !is.na(pval) & pval < alpha)
  }
  m <- mean(x); s <- stats::sd(x)
  if (is.null(modx.values)) modx.values <- c(m - s, m, m + s)
  if (!is.numeric(modx.values) || any(!is.finite(modx.values)))
    stop("modx.values must contain finite numeric values.")
  slopes <- calc(modx.values)
  slopes$level <- if (length(modx.values) == 3L &&
                      isTRUE(all.equal(as.numeric(modx.values), c(m-s,m,m+s))))
    c("-1 SD", "Mean", "+1 SD") else paste0("X=", round(modx.values, digits))
  slopes$CI95 <- sprintf(paste0("[%." , digits, "f, %." , digits, "f]"),
                         slopes$CI.lower, slopes$CI.upper)
  slopes <- slopes[, c("level", "X", "B", "SE", "t", "df", "p", "CI95", "significant")]
  if (is.null(mod.range)) mod.range <- range(x)
  if (!is.numeric(mod.range) || length(mod.range) != 2L ||
      any(!is.finite(mod.range)) || mod.range[1] >= mod.range[2])
    stop("mod.range must contain two finite numbers in ascending order.")
  # Recover the original moderator scale only when centering can be verified.
  original_source <- NULL
  original_values <- NULL
  centering_constant <- NA_real_
  if (!is.numeric(centering.tol) || length(centering.tol) != 1L ||
      !is.finite(centering.tol) || centering.tol < 0)
    stop("centering.tol must be a finite non-negative number.")
  if (!is.null(original.modx)) {
    if (is.character(original.modx) && length(original.modx) == 1L) {
      if (original.modx %in% names(mf)) {
        original_values <- mf[[original.modx]]
      } else {
        original_values <- tryCatch(
          eval(as.name(original.modx), envir = environment(stats::formula(model))),
          error = function(e) NULL)
        if (is.null(original_values))
          stop("original.modx was not found in the model frame or formula environment.")
      }
      original_source <- original.modx
    } else if (is.numeric(original.modx)) {
      original_values <- original.modx
      original_source <- "supplied numeric vector"
    } else stop("original.modx must be a variable name or numeric vector.")
  } else if (grepl("_c$", modx)) {
    candidate <- sub("_c$", "", modx)
    if (candidate %in% names(mf)) {
      original_values <- mf[[candidate]]
      original_source <- candidate
    } else {
      original_values <- tryCatch(
        eval(as.name(candidate), envir = environment(stats::formula(model))),
        error = function(e) NULL)
      if (!is.null(original_values)) original_source <- candidate
    }
  }
  # Search the original data argument used by lm(), including data = dat.
  if (is.null(original_values) && is.null(original.modx) && grepl("_c$", modx)) {
    candidate <- sub("_c$", "", modx)
    data_source <- tryCatch(eval(model$call$data,
                                 envir = environment(stats::formula(model))), error = function(e) NULL)
    if (is.data.frame(data_source) && candidate %in% names(data_source)) {
      original_values <- data_source[[candidate]]
      original_source <- candidate
    }
  }
  if (!is.null(original_values)) {
    if (!is.numeric(original_values))
      stop("The original moderator must be numeric.")
    # lm() may drop rows due to missing values: align named full-length vectors
    # using model.frame row names when possible.
    if (length(original_values) != length(x)) {
      row_ids <- suppressWarnings(as.integer(rownames(mf)))
      if (length(row_ids) == length(x) && all(!is.na(row_ids)) &&
          length(original_values) >= max(row_ids)) {
        original_values <- original_values[row_ids]
      } else {
        stop("The original moderator cannot be aligned to the model rows. Supply an aligned numeric vector.")
      }
    }
    if (any(!is.finite(original_values)))
      stop("The original moderator contains non-finite values in model rows.")
    offsets <- original_values - as.numeric(x)
    if (max(abs(offsets - mean(offsets))) > centering.tol)
      stop("Original and centered moderator values do not differ by a constant; verify original.modx.")
    centering_constant <- mean(offsets)
    if (!is.null(original.mean) &&
        (!isTRUE(all.equal(as.numeric(original.mean), centering_constant,
                           tolerance = centering.tol))))
      stop("original.mean does not match the centering constant derived from original.modx.")
  } else if (!is.null(original.mean)) {
    if (!is.numeric(original.mean) || length(original.mean) != 1L ||
        !is.finite(original.mean)) stop("original.mean must be a finite number.")
    centering_constant <- original.mean
    original_source <- "original.mean argument"
  }
  # (B0+B1*x)^2 - tcrit^2*(v00+2*v01*x+v11*x^2)=0
  A <- B1^2 - crit^2 * v11
  B <- 2 * (B0 * B1 - crit^2 * v01)
  C <- B0^2 - crit^2 * v00
  tol <- 1e-10 * max(1, abs(A), abs(B), abs(C))
  if (abs(A) < tol) {
    roots <- if (abs(B) < tol) numeric() else -C / B
  } else {
    disc <- B^2 - 4 * A * C
    roots <- if (disc < -tol) numeric() else if (abs(disc) <= tol)
      -B / (2 * A) else sort(c((-B-sqrt(disc))/(2*A), (-B+sqrt(disc))/(2*A)))
  }
  roots <- sort(unique(roots[is.finite(roots)]))
  bounds <- data.frame(bound = roots, in_observed_range = roots >= min(x) & roots <= max(x),
                       in_plot_range = roots >= mod.range[1] & roots <= mod.range[2])
  original_bounds <- bounds
  original_bounds$original_score <- if (is.finite(centering_constant))
    bounds$bound + centering_constant else rep(NA_real_, nrow(bounds))
  cuts <- sort(unique(c(mod.range[1], roots[roots > mod.range[1] & roots < mod.range[2]], mod.range[2])))
  regions <- data.frame(from = head(cuts, -1), to = tail(cuts, -1))
  regions$significant <- calc((regions$from + regions$to) / 2)$significant
  regions$label <- ifelse(regions$significant, "Significant", "Not significant")
  original_regions <- regions
  original_regions$from_original <- if (is.finite(centering_constant))
    regions$from + centering_constant else rep(NA_real_, nrow(regions))
  original_regions$to_original <- if (is.finite(centering_constant))
    regions$to + centering_constant else rep(NA_real_, nrow(regions))
  original_regions$interpretation <- ifelse(
    regions$significant,
    "Group difference is statistically significant",
    "Group difference is not statistically significant")
  curve <- calc(seq(mod.range[1], mod.range[2], length.out = n.grid))
  p <- NULL
  if (isTRUE(plot)) {
    # Background: significant = light gray; not significant = white with diagonal hatching.
    # Hatching provides a distinction that remains visible in grayscale printing.
    fills <- if (isTRUE(grayscale)) c("Significant" = "grey88", "Not significant" = "white")
    else c("Significant" = "#D6E8F3", "Not significant" = "#FFF4E8")
    yr <- range(c(curve$CI.lower, curve$CI.upper, 0), finite = TRUE)
    pad <- max(diff(yr) * 0.08, 0.2)
    ymin <- yr[1] - pad
    ymax <- yr[2] + pad
    # Add regularly spaced diagonal lines in nonsignificant regions (no extra package required).
    nonsig <- regions[!regions$significant, , drop = FALSE]
    hatch <- data.frame(x = numeric(), xend = numeric(), y = numeric(), yend = numeric())
    if (nrow(nonsig)) {
      dx <- diff(mod.range) / 38
      dy <- diff(c(ymin, ymax)) / 13
      slope <- dy / dx
      for (i in seq_len(nrow(nonsig))) {
        lo <- nonsig$from[i]; hi <- nonsig$to[i]
        if (hi <= lo) next
        starts <- seq(lo - (ymax - ymin) / slope, hi, by = dx * 2.4)
        for (st in starts) {
          x0 <- max(lo, st); x1 <- min(hi, st + (ymax - ymin) / slope)
          if (x1 <= x0) next
          y0 <- ymin + (x0 - st) * slope
          y1 <- ymin + (x1 - st) * slope
          hatch <- rbind(hatch, data.frame(x = x0, xend = x1, y = y0, yend = y1))
        }
      }
    }
    # Draw diagonal hatching inside the legend key for non-significant regions.
    # The key is based on the mapped fill, so it matches the plot in grayscale.
    draw_jn_legend_key <- function(data, params, size) {
      key_fill <- data$fill[1]
      base <- grid::rectGrob(
        gp = grid::gpar(fill = key_fill, col = "grey60", lwd = 0.5)
      )
      if (!isTRUE(grayscale) || !identical(tolower(key_fill), "#ffffff") &&
          !identical(tolower(key_fill), "white")) return(base)
      # Short, parallel diagonal strokes remain within the legend rectangle.
      strokes <- grid::segmentsGrob(
        x0 = grid::unit(c(0.00, 0.00, 0.25, 0.55, 0.85), "npc"),
        y0 = grid::unit(c(0.30, 0.80, 0.00, 0.00, 0.00), "npc"),
        x1 = grid::unit(c(0.30, 0.80, 1.00, 1.00, 1.00), "npc"),
        y1 = grid::unit(c(1.00, 1.00, 1.00, 0.70, 0.20), "npc"),
        gp = grid::gpar(col = "grey35", lwd = 0.75)
      )
      grid::grobTree(base, strokes)
    }
    p <- ggplot2::ggplot(curve, ggplot2::aes(x = X, y = B)) +
      ggplot2::geom_rect(data = regions, ggplot2::aes(xmin = from, xmax = to,
                                                      ymin = -Inf, ymax = Inf, fill = label), inherit.aes = FALSE,
                         key_glyph = draw_jn_legend_key) +
      ggplot2::scale_fill_manual(values = fills, name = "Significance region", drop = FALSE) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = CI.lower, ymax = CI.upper),
                           fill = "grey55", alpha = 0.23) +
      ggplot2::geom_line(linewidth = 0.95, color = "black") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey35") +
      ggplot2::scale_y_continuous(limits = c(ymin, ymax), expand = ggplot2::expansion(mult = 0)) +
      ggplot2::labs(title = "Johnson-Neyman Regions of Significance",
                    subtitle = paste0("Conditional Group Difference and ", round((1-alpha)*100), "% Confidence Interval"),
                    x = modx, y = paste0("Conditional ", pred_var, " Effect (", group_labels[2], " - ", group_labels[1], ")")) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "bottom",
                     plot.caption = ggplot2::element_text(hjust = 0, size = caption.size),
                     legend.text = ggplot2::element_text(size = legend.size),
                     legend.title = ggplot2::element_text(size = legend.size),
                     axis.title.x = ggplot2::element_text(size = x.title.size),
                     axis.title.y = ggplot2::element_text(size = y.title.size),
                     axis.text.x = ggplot2::element_text(size = x.text.size),
                     axis.text.y = ggplot2::element_text(size = y.text.size),
                     panel.grid.minor = ggplot2::element_blank())
    if (nrow(hatch)) {
      p <- p + ggplot2::geom_segment(data = hatch,
                                     ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                                     inherit.aes = FALSE, color = "grey65", linewidth = 0.23)
    }
    shown <- bounds$bound[bounds$in_plot_range]
    if (length(shown)) {
      ann <- calc(shown)
      # Add the original-scale moderator threshold when the centering constant is known.
      ann$label <- paste0(
        "JN = ", formatC(ann$X, format = "f", digits = digits),
        if (is.finite(centering_constant)) {
          paste0(" (raw = ",
                 formatC(ann$X + centering_constant, format = "f", digits = digits),
                 ")")
        } else {
          ""
        }
      )
      p <- p + ggplot2::geom_vline(xintercept = shown, linetype = "dotdash",
                                   color = "black", linewidth = 0.65)
      if (identical(jn.label, "repel") && requireNamespace("ggrepel", quietly = TRUE)) {
        p <- p + ggrepel::geom_label_repel(data = ann,
                                           ggplot2::aes(x = X, y = B, label = label), inherit.aes = FALSE,
                                           size = 3.4, fill = "white", color = "black", seed = 123,
                                           box.padding = 0.6, max.overlaps = Inf)
      } else {
        # Place labels near the bottom of the plot to avoid overlapping the effect line.
        ann$label_y <- ymin + 0.055 * (ymax - ymin)
        p <- p + ggplot2::geom_label(data = ann,
                                     ggplot2::aes(x = X, y = label_y, label = label),
                                     inherit.aes = FALSE, size = 3.3, fill = "white", color = "black",
                                     label.size = 0.3)
      }
    }
    p <- p + ggplot2::geom_vline(xintercept = m, linetype = "dotted", color = "grey40")
    if (nrow(bounds)) {
      outside <- bounds$bound[!bounds$in_plot_range]
      cap <- paste0("All JN bounds: ", paste(formatC(bounds$bound, digits = digits, format = "f"), collapse = ", "))
      if (length(outside)) cap <- paste0(cap, " | Outside plot range: ",
                                         paste(formatC(outside, digits = digits, format = "f"), collapse = ", "))
      p <- p + ggplot2::labs(caption = cap)
    } else {
      p <- p + ggplot2::labs(caption = "No real-valued JN bounds")
    }
  }
  interaction <- data.frame(term = b, B = B1, SE = sqrt(v11),
                            t = B1 / sqrt(v11), df = df,
                            p = 2 * stats::pt(-abs(B1 / sqrt(v11)), df))
  # Explanation distinguishes observed support from extrapolation.
  format_num <- function(z) formatC(z, format = "f", digits = digits)
  region_text <- if (nrow(regions)) {
    paste(vapply(seq_len(nrow(regions)), function(i) {
      paste0("X in [", format_num(regions$from[i]), ", ",
             format_num(regions$to[i]), "]: ", regions$label[i])
    }, character(1)), collapse = "\n")
  } else "No regions available."
  bound_text <- if (nrow(bounds)) {
    paste(vapply(seq_len(nrow(bounds)), function(i) {
      paste0("JN = ", format_num(bounds$bound[i]),
             if (bounds$in_observed_range[i]) " (inside observed X range)"
             else " (outside observed X range; extrapolation)")
    }, character(1)), collapse = "\n")
  } else "No real-valued JN thresholds were found."
  original_text <- if (is.finite(centering_constant)) {
    region_lines <- paste(vapply(seq_len(nrow(original_regions)), function(i) {
      paste0("Original score in [", format_num(original_regions$from_original[i]),
             ", ", format_num(original_regions$to_original[i]), "]: ",
             original_regions$interpretation[i])
    }, character(1)), collapse = "\n")
    paste0("\n[Original-Scale Moderator Interpretation]\n",
           "Original moderator: ", original_source, "\n",
           "Centering constant = ", format_num(centering_constant), "\n",
           "Original score = centered score + centering constant\n",
           "JN thresholds on original scale: ",
           if (nrow(original_bounds)) paste(format_num(original_bounds$original_score),
                                            collapse = ", ") else "none", "\n",
           "Significance within the observed moderator range:\n", region_lines, "\n",
           "A threshold marks statistical significance, not a deterministic educational cutoff.\n")
  } else {
    paste0("\n[Original-Scale Moderator Interpretation]\n",
           "Original scores unavailable: supply original.modx or original.mean.\n",
           "Do not interpret centered JN thresholds as raw scores.\n")
  }
  explanation <- paste0(
    "[Johnson-Neyman Interpretation]\n",
    "Predictor: ", pred_var, " (", group_labels[2], " = 1; ",
    group_labels[1], " = 0)\n",
    "Moderator: ", modx, "\n",
    "Moderator mean = ", format_num(m), "; SD = ", format_num(s),
    "; observed range = [", format_num(min(x)), ", ", format_num(max(x)), "]\n",
    "Alpha = ", format_num(alpha), "; confidence level = ",
    format_num(100 * (1 - alpha)), "%\n",
    "Conditional group difference = ", format_num(B0), " + (",
    format_num(B1), ") * X\n",
    "JN thresholds:\n", bound_text, "\n",
    "Significance within the plotted moderator range:\n", region_text, "\n",
    "The regions describe two-sided tests of the conditional group difference. ",
    "A non-significant result does not establish equivalence. ",
    "Thresholds outside the observed moderator range require extrapolation.\n",
    original_text
  )
  if (!is.null(p) && isTRUE(console.explain)) {
    # A dedicated S3 print method prints the interpretation whenever print(plot) is called.
    class(p) <- c("jn_plot", class(p))
    attr(p, "jn_interpretation") <- explanation
  }
  out <- list(model = model, model_summary = summary(model),
              model_data = model_data, data = model_data,
              interpretation = explanation,
              show.data = show.data,
              interaction = interaction, slopes = slopes,
              bounds = bounds, regions = regions, original_bounds = original_bounds,
              original_regions = original_regions, original_mean = centering_constant,
              original_source = original_source, curve = curve, plot = p,
              range = range(x), mean = m, sd = s, alpha = alpha,
              predictor = pred_var, group_levels = group_labels)
  class(out) <- c("jn_analysis", "list")
  if (isTRUE(auto.print)) {
    print(out)
    if (!is.null(out$plot)) {
      # Use the regular ggplot print method to avoid repeating the interpretation.
      plot_to_print <- out$plot
      class(plot_to_print) <- setdiff(class(plot_to_print), "jn_plot")
      attr(plot_to_print, "jn_interpretation") <- NULL
      print(plot_to_print)
    }
  }
  invisible(out)
}

#' @export
print.jn_analysis <- function(x, digits = 3, ...) {
  cat("\n[Model Summary]\n")
  print(x$model_summary)
  if (isTRUE(x$show.data)) {
    cat("\n[Model-Derived Analysis Data: Y / G / X]\n")
    print(x$model_data, row.names = FALSE)
  }
  cat("\n[Interaction Term Test]\n")
  print(x$interaction, row.names = FALSE)
  cat("\n[Simple Slopes / Conditional Group Difference]\n")
  tab <- x$slopes
  numeric_cols <- vapply(tab, is.numeric, logical(1))
  tab[numeric_cols] <- lapply(tab[numeric_cols], round, digits = digits)
  tab$p <- ifelse(x$slopes$p < .001, "< .001", sprintf("%.*f", digits, x$slopes$p))
  print(tab, row.names = FALSE)
  cat("\n[Johnson-Neyman Bounds]\n")
  if (nrow(x$bounds)) print(x$bounds, row.names = FALSE) else cat("No real-valued bounds\n")
  cat("\n[Regions of Significance Within Plot Range]\n")
  print(x$regions, row.names = FALSE)
  cat("\n[Original-Scale Johnson-Neyman Bounds]\n")
  print(x$original_bounds, row.names = FALSE)
  cat("\n[Original-Scale Regions of Significance]\n")
  print(x$original_regions, row.names = FALSE)
  cat("\n", x$interpretation, sep = "")
  invisible(x)
}

#' Print a Johnson-Neyman plot and its interpretation
#' @param x A jn_plot object.
#' @param ... Additional graphical printing arguments.
#' @export
print.jn_plot <- function(x, ...) {
  explanation <- attr(x, "jn_interpretation")
  if (!is.null(explanation)) cat("\n", explanation, sep = "")
  class(x) <- class(x)[class(x) != "jn_plot"]
  attr(x, "jn_interpretation") <- NULL
  print(x, ...)
  invisible(x)
}

