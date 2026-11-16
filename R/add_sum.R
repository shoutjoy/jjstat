#' Add Margins and Percentages to a Contingency Table
#'
#' Creates a contingency table from raw data or an existing frequency table.
#' Continuous numeric variables can be automatically converted into categorical
#' variables before constructing the contingency table.
#'
#' @param dataset A data frame when `raw = TRUE`, or a contingency table,
#'   matrix, or numeric data frame when `raw = FALSE`.
#' @param v1 Character scalar specifying the first variable name.
#' @param v2 Character scalar specifying the second variable name.
#' @param raw Logical. If `TRUE`, `dataset` is treated as raw case-level data.
#' @param add_ratio Logical. If `TRUE`, percentages are added using `ratio_fun`.
#' @param ratio_fun Function used to add percentages. The default is
#'   `add_ratio_df`. This function is used only when `add_ratio = TRUE`.
#' @param auto_bin Logical. If `TRUE`, continuous numeric variables are
#'   automatically converted into categorical variables.
#' @param bin_method Character. Method used to categorize continuous variables.
#'   One of `"quantile"`, `"equal_width"`, `"sd"`, or `"pretty"`.
#' @param bins Integer. Desired number of categories for `"quantile"`,
#'   `"equal_width"`, and `"pretty"` methods.
#' @param max_categories Integer. Numeric variables having more unique values
#'   than this threshold are regarded as continuous and automatically binned.
#' @param include_na Logical. If `TRUE`, missing values are included as an
#'   explicit `"Missing"` category.
#' @param labels Optional character vector of category labels. When `NULL`,
#'   interval labels are generated automatically.
#' @param plot Logical. If `TRUE`, a mosaic plot is drawn.
#' @param color Logical or color specification passed to
#'   [graphics::mosaicplot()].
#' @param ylab Character scalar specifying the y-axis label.
#' @param xlab Character scalar specifying the x-axis label.
#' @param cex Numeric text scaling factor for axis labels.
#' @param title Optional title for the mosaic plot.
#' @param type Character. Output type. `"df"` and `"res"` return the processed
#'   table. `"all"` and `"a11"` return all components.
#' @param pattern Logical. If `TRUE`, `patternGraph()` is called when available.
#'
#' @return
#' Depending on `type`, either a contingency table or a named list containing:
#'
#' \itemize{
#'   \item `table`: Table with margins or percentages.
#'   \item `frequency`: Original contingency-frequency table.
#'   \item `categorized_data`: Data frame containing the categorized variables.
#'   \item `bin_info`: Information about the categorization method and cut points.
#'   \item `mosaic`: Result of the mosaic plot call.
#'   \item `pattern_graph`: Result returned by `patternGraph()`.
#' }
#'
#' @details
#' Numeric variables are automatically categorized only when the number of
#' unique non-missing values exceeds `max_categories`.
#'
#' The available categorization methods are:
#'
#' \itemize{
#'   \item `"quantile"`: Uses sample quantiles and attempts to create groups
#'   with similar numbers of observations.
#'   \item `"equal_width"`: Divides the observed range into equal-width intervals.
#'   \item `"sd"`: Categorizes values using mean and standard deviation:
#'   below -1 SD, -1 SD to mean, mean to +1 SD, and above +1 SD.
#'   \item `"pretty"`: Uses [base::pretty()] to generate readable interval
#'   boundaries.
#' }
#'
#' @export
#'
#' @examples
#' # Example 1: Variables with few unique values are used directly
#' add_sum(
#'   mtcars,
#'   v1 = "am",
#'   v2 = "cyl",
#'   raw = TRUE,
#'   add_ratio = FALSE
#' )
#'
#' # Example 2: Continuous variables are automatically categorized
#' add_sum(
#'   mtcars,
#'   v1 = "mpg",
#'   v2 = "hp",
#'   raw = TRUE,
#'   add_ratio = FALSE
#' )
#'
#' # Example 3: Three quantile categories
#' add_sum(
#'   mtcars,
#'   v1 = "mpg",
#'   v2 = "hp",
#'   raw = TRUE,
#'   bins = 3,
#'   bin_method = "quantile",
#'   add_ratio = FALSE
#' )
#'
#' # Example 4: Equal-width categorization
#' add_sum(
#'   mtcars,
#'   v1 = "mpg",
#'   v2 = "hp",
#'   raw = TRUE,
#'   bins = 4,
#'   bin_method = "equal_width",
#'   add_ratio = FALSE
#' )
#'
#' # Example 5: Mean and standard-deviation categorization
#' add_sum(
#'   mtcars,
#'   v1 = "mpg",
#'   v2 = "hp",
#'   raw = TRUE,
#'   bin_method = "sd",
#'   add_ratio = FALSE
#' )
#'
#' # Example 6: Return all intermediate results
#' result <- add_sum(
#'   mtcars,
#'   v1 = "mpg",
#'   v2 = "hp",
#'   raw = TRUE,
#'   add_ratio = FALSE,
#'   type = "all"
#' )
#'
#' result$table
#' result$categorized_data
#' result$bin_info
#'
#' # Example 7: Disable automatic categorization
#' add_sum(
#'   mtcars,
#'   v1 = "mpg",
#'   v2 = "hp",
#'   raw = TRUE,
#'   auto_bin = FALSE,
#'   add_ratio = FALSE
#' )
#'
#' \dontrun{
#' # Example 8: Add percentages using add_ratio_df()
#' add_sum(
#'   mtcars,
#'   v1 = "mpg",
#'   v2 = "hp",
#'   raw = TRUE,
#'   add_ratio = TRUE
#' )
#' }
add_sum <- function(dataset,
                    v1 = "a1",
                    v2 = "성조",
                    raw = FALSE,
                    add_ratio = TRUE,
                    ratio_fun = add_ratio_df,
                    auto_bin = TRUE,
                    bin_method = c(
                      "quantile",
                      "equal_width",
                      "sd",
                      "pretty"
                    ),
                    bins = 4L,
                    max_categories = 10L,
                    include_na = FALSE,
                    labels = NULL,
                    plot = FALSE,
                    color = TRUE,
                    ylab = "onset",
                    xlab = "speaker",
                    cex = 1.2,
                    title = NULL,
                    type = c("df", "res", "all", "a11"),
                    pattern = TRUE) {
  
  # 인수값을 검증하고 선택지를 확정한다.
  type <- match.arg(type)
  bin_method <- match.arg(bin_method)
  
  if (!is.logical(raw) || length(raw) != 1L || is.na(raw)) {
    stop("`raw` must be a single TRUE or FALSE value.", call. = FALSE)
  }
  
  if (!is.logical(add_ratio) ||
      length(add_ratio) != 1L ||
      is.na(add_ratio)) {
    stop(
      "`add_ratio` must be a single TRUE or FALSE value.",
      call. = FALSE
    )
  }
  
  if (!is.logical(auto_bin) ||
      length(auto_bin) != 1L ||
      is.na(auto_bin)) {
    stop(
      "`auto_bin` must be a single TRUE or FALSE value.",
      call. = FALSE
    )
  }
  
  if (!is.numeric(bins) ||
      length(bins) != 1L ||
      is.na(bins) ||
      bins < 2) {
    stop("`bins` must be an integer greater than or equal to 2.", call. = FALSE)
  }
  
  bins <- as.integer(bins)
  
  if (!is.numeric(max_categories) ||
      length(max_categories) != 1L ||
      is.na(max_categories) ||
      max_categories < 2) {
    stop(
      "`max_categories` must be an integer greater than or equal to 2.",
      call. = FALSE
    )
  }
  
  max_categories <- as.integer(max_categories)
  
  # 내부 함수: 하나의 숫자형 변수를 범주화한다.
  categorize_numeric <- function(x,
                                 variable_name,
                                 method,
                                 n_bins,
                                 user_labels = NULL) {
    
    x_non_missing <- x[!is.na(x)]
    unique_values <- sort(unique(x_non_missing))
    
    # 값이 없거나 하나뿐이면 범주화하지 않는다.
    if (length(unique_values) <= 1L) {
      return(list(
        variable = x,
        categorized = FALSE,
        variable_name = variable_name,
        method = "none",
        breaks = NULL,
        labels = NULL,
        reason = "The variable has one or fewer unique non-missing values."
      ))
    }
    
    # 범주 수보다 고유값 수가 적으면 가능한 범주 수를 줄인다.
    effective_bins <- min(n_bins, length(unique_values))
    
    if (method == "quantile") {
      
      # 분위수 기반 절단점을 계산한다.
      probs <- seq(0, 1, length.out = effective_bins + 1L)
      
      cut_points <- as.numeric(
        stats::quantile(
          x_non_missing,
          probs = probs,
          na.rm = TRUE,
          names = FALSE,
          type = 7
        )
      )
      
      # 동점으로 인한 중복 절단점을 제거한다.
      cut_points <- unique(cut_points)
      
      # 절단점이 충분하지 않으면 pretty 방식으로 대체한다.
      if (length(cut_points) < 3L) {
        cut_points <- unique(
          pretty(
            range(x_non_missing, na.rm = TRUE),
            n = effective_bins
          )
        )
      }
      
    } else if (method == "equal_width") {
      
      x_range <- range(x_non_missing, na.rm = TRUE)
      
      cut_points <- seq(
        from = x_range[1L],
        to = x_range[2L],
        length.out = effective_bins + 1L
      )
      
    } else if (method == "sd") {
      
      x_mean <- mean(x_non_missing, na.rm = TRUE)
      x_sd <- stats::sd(x_non_missing, na.rm = TRUE)
      
      if (is.na(x_sd) || x_sd == 0) {
        cut_points <- unique(
          pretty(
            range(x_non_missing, na.rm = TRUE),
            n = effective_bins
          )
        )
      } else {
        cut_points <- c(
          -Inf,
          x_mean - x_sd,
          x_mean,
          x_mean + x_sd,
          Inf
        )
      }
      
    } else {
      
      # 사람이 읽기 쉬운 절단점을 생성한다.
      cut_points <- unique(
        pretty(
          range(x_non_missing, na.rm = TRUE),
          n = effective_bins
        )
      )
    }
    
    # cut()이 최소 2개 구간을 만들 수 있는지 검사한다.
    if (length(unique(cut_points)) < 3L) {
      return(list(
        variable = factor(x),
        categorized = FALSE,
        variable_name = variable_name,
        method = "factor",
        breaks = cut_points,
        labels = NULL,
        reason = "Insufficient distinct cut points were generated."
      ))
    }
    
    cut_points <- sort(unique(cut_points))
    
    # 최솟값과 최댓값이 반드시 포함되도록 경계를 확장한다.
    if (is.finite(cut_points[1L])) {
      cut_points[1L] <- -Inf
    }
    
    if (is.finite(cut_points[length(cut_points)])) {
      cut_points[length(cut_points)] <- Inf
    }
    
    number_of_intervals <- length(cut_points) - 1L
    
    # 사용자 레이블의 길이를 검사한다.
    if (!is.null(user_labels)) {
      if (length(user_labels) != number_of_intervals) {
        warning(
          "`labels` length does not match the number of generated intervals. ",
          "Automatic interval labels will be used.",
          call. = FALSE
        )
        
        user_labels <- NULL
      }
    }
    
    # 표준편차 방식의 경우 해석 가능한 레이블을 기본으로 제공한다.
    if (method == "sd" &&
        is.null(user_labels) &&
        number_of_intervals == 4L) {
      user_labels <- c(
        "Low: below -1 SD",
        "Lower-middle: -1 SD to mean",
        "Upper-middle: mean to +1 SD",
        "High: above +1 SD"
      )
    }
    
    categorized_x <- cut(
      x,
      breaks = cut_points,
      labels = user_labels,
      include.lowest = TRUE,
      right = TRUE,
      ordered_result = TRUE
    )
    
    list(
      variable = categorized_x,
      categorized = TRUE,
      variable_name = variable_name,
      method = method,
      breaks = cut_points,
      labels = levels(categorized_x),
      reason = paste0(
        "Numeric variable with ",
        length(unique_values),
        " unique values was categorized."
      )
    )
  }
  
  categorized_data <- NULL
  bin_info <- list()
  
  if (raw) {
    
    if (!is.data.frame(dataset)) {
      stop(
        "When `raw = TRUE`, `dataset` must be a data frame.",
        call. = FALSE
      )
    }
    
    if (!is.character(v1) || length(v1) != 1L ||
        !is.character(v2) || length(v2) != 1L) {
      stop(
        "`v1` and `v2` must each be a single variable name.",
        call. = FALSE
      )
    }
    
    missing_variables <- setdiff(c(v1, v2), names(dataset))
    
    if (length(missing_variables) > 0L) {
      stop(
        "Variables not found in `dataset`: ",
        paste(missing_variables, collapse = ", "),
        call. = FALSE
      )
    }
    
    var1 <- dataset[[v1]]
    var2 <- dataset[[v2]]
    
    # 첫 번째 변수가 연속형인지 판정하여 자동 범주화한다.
    if (auto_bin &&
        is.numeric(var1) &&
        length(unique(var1[!is.na(var1)])) > max_categories) {
      
      bin_v1 <- categorize_numeric(
        x = var1,
        variable_name = v1,
        method = bin_method,
        n_bins = bins,
        user_labels = labels
      )
      
      var1 <- bin_v1$variable
      bin_info[[v1]] <- bin_v1
      
    } else {
      
      bin_info[[v1]] <- list(
        variable_name = v1,
        categorized = FALSE,
        method = "none",
        breaks = NULL,
        labels = if (is.factor(var1)) levels(var1) else NULL,
        reason = "The variable was retained without automatic categorization."
      )
    }
    
    # 두 번째 변수가 연속형인지 판정하여 자동 범주화한다.
    if (auto_bin &&
        is.numeric(var2) &&
        length(unique(var2[!is.na(var2)])) > max_categories) {
      
      bin_v2 <- categorize_numeric(
        x = var2,
        variable_name = v2,
        method = bin_method,
        n_bins = bins,
        user_labels = labels
      )
      
      var2 <- bin_v2$variable
      bin_info[[v2]] <- bin_v2
      
    } else {
      
      bin_info[[v2]] <- list(
        variable_name = v2,
        categorized = FALSE,
        method = "none",
        breaks = NULL,
        labels = if (is.factor(var2)) levels(var2) else NULL,
        reason = "The variable was retained without automatic categorization."
      )
    }
    
    # 결측값을 별도 범주로 포함한다.
    if (include_na) {
      var1 <- addNA(var1, ifany = TRUE)
      var2 <- addNA(var2, ifany = TRUE)
      
      levels(var1)[is.na(levels(var1))] <- "Missing"
      levels(var2)[is.na(levels(var2))] <- "Missing"
    }
    
    categorized_data <- data.frame(
      var1 = var1,
      var2 = var2,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    names(categorized_data) <- c(v1, v2)
    
    # 범주화된 변수를 이용하여 교차빈도표를 생성한다.
    data_table <- table(
      categorized_data[[v1]],
      categorized_data[[v2]],
      useNA = if (include_na) "ifany" else "no"
    )
    
    names(dimnames(data_table)) <- c(v1, v2)
    
  } else {
    
    if (!(is.table(dataset) ||
          is.matrix(dataset) ||
          is.data.frame(dataset))) {
      stop(
        "When `raw = FALSE`, `dataset` must be a table, matrix, ",
        "or numeric data frame.",
        call. = FALSE
      )
    }
    
    data_table <- as.matrix(dataset)
    
    if (!is.numeric(data_table)) {
      stop(
        "The contingency table must contain numeric frequencies.",
        call. = FALSE
      )
    }
    
    if (any(data_table < 0, na.rm = TRUE)) {
      stop(
        "Contingency-table frequencies cannot be negative.",
        call. = FALSE
      )
    }
  }
  
  if (anyNA(data_table)) {
    stop(
      "The contingency table contains missing frequency values.",
      call. = FALSE
    )
  }
  
  # 비율표 또는 합계표를 생성한다.
  if (add_ratio) {
    
    if (!is.function(ratio_fun)) {
      stop(
        "`ratio_fun` must be a function when `add_ratio = TRUE`.",
        call. = FALSE
      )
    }
    
    data_margin <- ratio_fun(data_table)
    
  } else {
    
    # addmargins()는 stats 패키지 함수이므로 네임스페이스를 명시한다.
    data_margin <- stats::addmargins(
      data_table,
      FUN = sum,
      quiet = TRUE
    )
  }
  
  mosaic_result <- NULL
  pattern_result <- NULL
  
  if (plot) {
    
    plot_title <- if (is.null(title)) {
      paste0("Contingency Table: ", v1, " × ", v2)
    } else {
      title
    }
    
    mosaic_result <- graphics::mosaicplot(
      t(data_table),
      color = color,
      ylab = ylab,
      xlab = xlab,
      cex.axis = cex,
      main = plot_title
    )
    
    if (pattern &&
        exists("patternGraph", mode = "function", inherits = TRUE)) {
      pattern_result <- patternGraph(
        data_table,
        type = "g",
        tolong = TRUE
      )
    }
  }
  
  result_all <- list(
    table = data_margin,
    frequency = data_table,
    categorized_data = categorized_data,
    bin_info = bin_info,
    mosaic = mosaic_result,
    pattern_graph = pattern_result,
    call = match.call()
  )
  
  switch(
    type,
    df = data_margin,
    res = data_margin,
    all = result_all,
    a11 = result_all
  )
}