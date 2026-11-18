#' 범주형 독립변수별 다중 t-test 및 일원분산분석
#'
#' @description
#' 여러 연속형 종속변수와 범주형 독립변수의 조합을 순회한다.
#' 유효 수준이 2개이면 독립표본 t-test를, 3개 이상이면 일원분산분석을 수행한다.
#' 자동 사후검정 모드에서는 중앙값 중심 Levene 검정 결과에 따라
#' 일반 ANOVA + Tukey 또는 Welch ANOVA + Games-Howell을 선택한다.
#' 기존 jjstat의 ttest_df() 및 aov_df() 결과도 별도로 보관한다.
#'
#' @details
#' 모든 비교는 각 종속변수와 독립변수의 완전사례를 기준으로 독립적으로 수행한다.
#' 숫자형으로 코딩된 집단변수는 명시적으로 factor로 변환해야 한다.
#' iv_vars = NULL이면 factor 열을 자동 검색한다. 문자형 집단변수를 포함하려면
#' include_character = TRUE로 지정한다. 종속변수는 독립변수 후보에서 제외한다.
#'
#' `posthoc = "auto"`에서 Levene p < levene_alpha이면 Welch ANOVA와
#' Games-Howell 사후검정을, 그 외의 유효한 경우 일반 ANOVA와 Tukey를 적용한다.
#' Levene 계산이 실패하면 보수적으로 Welch ANOVA를 사용한다.
#' 명시적으로 `posthoc = "games-howell"`을 설정하면 Welch ANOVA를 사용한다.
#' 나머지 사후검정 선택은 일반 ANOVA를 사용하므로 등분산 가정을 확인해야 한다.
#'
#' Scheffe 및 Duncan의 사후검정은 jjstat::aov_df()에 위임하며,
#' 표준 posthoc 테이블에는 base R의 Tukey 또는 pairwise t-test 결과와
#' Games-Howell 결과를 제공한다. Scheffe/Duncan의 상세 결과는
#' anova_raw에서 확인한다. LSD는 보정되지 않은 쌍별 pooled t-test이며
#' `posthoc = "lsd"`와 `p.adj = "holm"`을 조합하면 Holm 보정을 적용한다.
#' `posthoc = "bonferroni"` 또는 `"holm"`은 같은 pooled t-test에
#' 각 보정법을 적용한다. 이들은 Tukey HSD와 동일한 검정이 아니다.
#'
#' @param data 분석할 data.frame.
#' @param dv_vars 한 개 이상의 numeric 종속변수명을 담은 문자형 벡터.
#' @param digits 출력 시 사용되는 소수점 자리수. 원래 수치형 결과는 반올림하지 않음.
#' @param posthoc `"auto"`, `"tukey"`, `"scheffe"`, `"duncan"`,
#'   `"lsd"`, `"bonferroni"`, `"holm"`, `"games-howell"` 중 하나.
#' @param p.adj LSD 사후비교 p값 보정 방법. stats::p.adjust.methods 중 하나.
#' @param var.equal t-test에서 공통분산을 가정할지 여부. FALSE면 Welch t-test.
#' @param levene_alpha 자동 분산동질성 판단 기준. 0과 1 사이의 유의수준.
#' @param iv_vars 사용할 독립변수 이름의 문자형 벡터. NULL이면 자동 검색.
#' @param include_character TRUE이면 문자형 열도 범주형 독립변수 후보로 포함.
#'
#' @return 다음 요소를 가지는 list:
#' \describe{
#'   \item{factor_info}{독립변수별 관측 수준 수 및 최초 분류.}
#'   \item{t_vars}{전체 데이터에서 2수준인 독립변수명.}
#'   \item{aov_vars}{전체 데이터에서 3수준 이상인 독립변수명.}
#'   \item{ttest}{종속변수별 jjstat::ttest_df() 원래 결과 목록.}
#'   \item{anova}{종속변수별 표준화 ANOVA 결과표 목록.}
#'   \item{diagnostics}{분석 조합별 표본 수, Levene, 분석 상태.}
#'   \item{games_howell}{Games-Howell 사후검정 결과.}
#'   \item{ttest_summary}{t-test 검정통계량 및 p값.}
#'   \item{posthoc}{기계적으로 결합 가능한 사후검정 결과.}
#'   \item{ttest_raw}{각 조합의 jjstat::ttest_df() 원래 반환 객체.}
#'   \item{anova_raw}{각 조합의 jjstat::aov_df() 원래 반환 객체.}
#' }
#' @seealso \code{\link{ttest_df}}, \code{\link{aov_df}},
#'   \code{\link[stats]{oneway.test}}, \code{\link[stats]{TukeyHSD}}
#' @references
#' Welch, B. L. (1951). On the comparison of several mean values:
#' An alternative approach. Biometrika, 38(3/4), 330-336.
#'
#' Games, P. A., & Howell, J. F. (1976). Pairwise multiple comparison
#' procedures with unequal n's and/or variances: A Monte Carlo study.
#' Journal of Educational Statistics, 1(2), 113-125.
#'
#' @export
#' @examples
#' # Example 1: mtcars - 숫자코드 집단변수 factor화
#' d <- mtcars
#' d$am <- factor(d$am)
#' d$vs <- factor(d$vs)
#' d$cyl <- factor(d$cyl)
#' d$gear <- factor(d$gear)
#' x <- jj_group_compare(d, c("mpg", "hp"))
#' x$factor_info
#' x$ttest_summary
#' x$diagnostics
#'
#' # Example 2: 특정 독립변수만 비교
#' x2 <- jj_group_compare(d, "mpg", iv_vars = c("am", "cyl"))
#' x2$anova$mpg
#'
#' # Example 3: Tukey 사후검정
#' x3 <- jj_group_compare(d, "mpg", posthoc = "tukey")
#' x3$posthoc
#'
#' # Example 4: Holm 보정 LSD
#' x4 <- jj_group_compare(d, "mpg", posthoc = "lsd", p.adj = "holm")
#' x4$posthoc
#'
#' # Example 5: Bonferroni 보정 비교
#' x5 <- jj_group_compare(d, "mpg", posthoc = "bonferroni")
#'
#' # Example 6: Welch ANOVA 및 Games-Howell
#' x6 <- jj_group_compare(d, "mpg", posthoc = "games-howell")
#' x6$games_howell
#'
#' # Example 7: iris, 세 종별 다중 종속변수
#' x7 <- jj_group_compare(iris,
#'   c("Sepal.Length", "Petal.Length", "Petal.Width"))
#' x7$anova$Sepal.Length
#'
#' # Example 8: 문자형 범주변수 포함
#' d8 <- transform(iris, Species = as.character(Species))
#' x8 <- jj_group_compare(d8, "Sepal.Width", include_character = TRUE)
#'
#' # Example 9: 결측치 포함
#' d9 <- d
#' d9$mpg[1:4] <- NA_real_
#' x9 <- jj_group_compare(d9, "mpg")
#' x9$diagnostics
#'
#' # Example 10: Scheffe - 상세 결과는 anova_raw에 보존
#' \dontrun{
#' x10 <- jj_group_compare(d, "mpg", posthoc = "scheffe")
#' x10$anova_raw
#' }
#'
#' # Example 11: Duncan - 상세 결과는 anova_raw에 보존
#' \dontrun{
#' x11 <- jj_group_compare(d, "mpg", posthoc = "duncan")
#' x11$anova_raw
#' }
#'
#' # Example 12: t-test만 실행
#' x12 <- jj_group_compare(d, "mpg", iv_vars = c("am", "vs"))
#' x12$ttest_summary
#'
#' # Example 13: 분산 동질성 기준 변경
#' x13 <- jj_group_compare(d, "mpg", levene_alpha = 0.01)
#'
#' # Example 14: 원래 jjstat 스타일의 결과 확인
#' x14 <- jj_group_compare(d, "mpg", iv_vars = "cyl")
#' x14$anova_raw
jj_group_compare <- function(
    data,
    dv_vars,
    digits = 3,
    posthoc = "auto",
    p.adj = "holm",
    var.equal = FALSE,
    levene_alpha = 0.05,
    iv_vars = NULL,
    include_character = FALSE
) {
  # 1. 입력 형식과 패키지 의존성 검사
  if (!is.data.frame(data)) stop("data는 data.frame이어야 합니다.")
  if (!is.character(dv_vars) || !length(dv_vars) ||
      anyNA(dv_vars) || anyDuplicated(dv_vars)) {
    stop("dv_vars에는 중복·결측이 없는 종속변수명을 입력하세요.")
  }
  if (!all(dv_vars %in% names(data))) stop("존재하지 않는 종속변수가 있습니다.")
  if (!all(vapply(data[dv_vars], is.numeric, logical(1)))) {
    stop("종속변수는 모두 numeric이어야 합니다.")
  }
  if (length(digits) != 1L || !is.numeric(digits) || is.na(digits) ||
      digits < 0 || digits != as.integer(digits)) stop("digits는 0 이상의 정수여야 합니다.")
  if (length(levene_alpha) != 1L || !is.numeric(levene_alpha) ||
      is.na(levene_alpha) || levene_alpha <= 0 || levene_alpha >= 1) {
    stop("levene_alpha는 0과 1 사이여야 합니다.")
  }
  if (!is.logical(var.equal) || length(var.equal) != 1L || is.na(var.equal)) {
    stop("var.equal은 TRUE 또는 FALSE여야 합니다.")
  }
  if (!is.logical(include_character) || length(include_character) != 1L ||
      is.na(include_character)) stop("include_character는 TRUE 또는 FALSE여야 합니다.")
  choices <- c("auto", "tukey", "scheffe", "duncan", "lsd",
               "bonferroni", "holm", "games-howell")
  posthoc <- match.arg(posthoc, choices)
  p.adj <- match.arg(p.adj, stats::p.adjust.methods)
  for (pkg in c("car", "rstatix", "tibble")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("필요한 패키지를 설치하세요: ", pkg)
    }
  }

  # 2. 독립변수 후보 식별 및 factor 변환
  if (is.null(iv_vars)) {
    is_group <- vapply(data, function(x) {
      is.factor(x) || (include_character && is.character(x))
    }, logical(1))
    iv_vars <- setdiff(names(data)[is_group], dv_vars)
  } else {
    if (!is.character(iv_vars) || anyNA(iv_vars) || anyDuplicated(iv_vars)) {
      stop("iv_vars는 중복이나 결측 없는 문자형 변수명이어야 합니다.")
    }
    if (!all(iv_vars %in% names(data))) stop("존재하지 않는 독립변수가 있습니다.")
    if (any(iv_vars %in% dv_vars)) stop("종속변수는 독립변수로 지정할 수 없습니다.")
    if (!all(vapply(data[iv_vars], function(x) is.factor(x) || is.character(x), logical(1)))) {
      stop("독립변수는 factor/character여야 합니다. 숫자형 코드는 factor로 변환하세요.")
    }
  }
  if (!length(iv_vars)) stop("사용 가능한 범주형 독립변수가 없습니다.")
  group_data <- data
  for (iv in iv_vars) group_data[[iv]] <- factor(as.character(data[[iv]]))
  nlevels_all <- vapply(group_data[iv_vars], nlevels, integer(1))
  factor_info <- tibble::tibble(
    variable = iv_vars,
    n_levels = unname(nlevels_all),
    method = ifelse(nlevels_all == 2L, "t-test",
                    ifelse(nlevels_all >= 3L, "ANOVA", "Exclude"))
  )
  t_vars <- factor_info$variable[factor_info$method == "t-test"]
  aov_vars <- factor_info$variable[factor_info$method == "ANOVA"]

  # 3. 빈 출력 기본 형식 정의: 성공/실패 결과를 일관되게 결합
  t_rows <- list()
  a_rows <- list()
  diag_rows <- list()
  ph_rows <- list()
  raw_t <- list()
  raw_a <- list()
  call_jj <- function(fun, args) {
    tryCatch(do.call(fun, args), error = function(e) {
      structure(list(message = conditionMessage(e)), class = "jj_group_error")
    })
  }

  # 4. 각 종속변수 × 독립변수별 완전사례를 만들고 재분류
  for (dv in dv_vars) {
    for (iv in iv_vars) {
      df <- data.frame(y = data[[dv]], g = group_data[[iv]])
      df <- df[stats::complete.cases(df) & is.finite(df$y), , drop = FALSE]
      df$g <- droplevels(df$g)
      nn <- as.integer(table(df$g))
      k <- length(nn)
      n <- nrow(df)
      key <- paste(dv, iv, sep = "::")
      lev_p <- NA_real_
      method <- NA_character_
      ph_method <- NA_character_
      note <- NA_character_

      # 빈 집단/작은 표본을 별도 기록하고 다음 비교로 진행
      if (k < 2L || any(nn < 2L)) {
        note <- "분석에 필요한 집단 수 또는 집단별 관측치(n >= 2)가 부족함"
      } else if (k == 2L) {
        method <- if (var.equal) "Student t-test" else "Welch t-test"
        fit <- tryCatch(stats::t.test(y ~ g, data = df,
                                     var.equal = var.equal),
                        error = function(e) e)
        if (inherits(fit, "error")) {
          note <- paste("t-test 실패:", conditionMessage(fit))
        } else {
          t_rows[[length(t_rows) + 1L]] <- tibble::tibble(
            dv = dv, iv = iv, method = method, n = n,
            group1 = levels(df$g)[1], group2 = levels(df$g)[2],
            t_value = unname(fit$statistic), df = unname(fit$parameter),
            p_value = fit$p.value,
            mean_diff = unname(fit$estimate[1] - fit$estimate[2]),
            ci_low = fit$conf.int[1], ci_high = fit$conf.int[2]
          )
        }
        # 원래 jjstat 결과는 조합별로 호출하여 오류를 격리
        raw_t[[key]] <- call_jj(ttest_df, list(
          data = df, dv_var = "y", iv_vars = "g", var.equal = var.equal,
          digits = digits, sig = TRUE
        ))
        if (inherits(raw_t[[key]], "jj_group_error")) {
          note <- paste(c(note, paste("ttest_df:", raw_t[[key]]$message)), collapse = "; ")
        }
      } else {
        # 5. 3개 이상 집단: Levene 진단
        lv <- tryCatch(car::leveneTest(y ~ g, data = df, center = stats::median),
                       error = function(e) NULL)
        if (!is.null(lv)) lev_p <- as.numeric(lv[["Pr(>F)"]][1])
        use_welch <- if (posthoc == "auto") {
          !is.finite(lev_p) || lev_p < levene_alpha
        } else {
          identical(posthoc, "games-howell")
        }
        method <- if (use_welch) "Welch ANOVA" else "Classical ANOVA"
        selected <- if (use_welch) "games-howell" else
          if (posthoc == "auto") "tukey" else posthoc
        ph_method <- selected
        if (!is.finite(lev_p)) note <- "Levene 계산 실패: Welch 적용 또는 가정 재확인"
        if (!use_welch && is.finite(lev_p) && lev_p < levene_alpha) {
          note <- "등분산성 위반 가능성: 일반 ANOVA 사후검정 강제 지정됨"
        }
        if (use_welch) {
          fit <- tryCatch(stats::oneway.test(y ~ g, data = df,
                                             var.equal = FALSE),
                          error = function(e) e)
          if (inherits(fit, "error")) {
            note <- paste("Welch ANOVA 실패:", conditionMessage(fit))
          } else {
            a_rows[[length(a_rows) + 1L]] <- tibble::tibble(
              dv = dv, iv = iv, n = n, method = method,
              F_value = unname(fit$statistic),
              df1 = unname(fit$parameter[1]),
              df2 = unname(fit$parameter[2]), p_value = fit$p.value
            )
            gh <- tryCatch(rstatix::games_howell_test(df, y ~ g),
                           error = function(e) e)
            if (inherits(gh, "error")) {
              note <- paste("Games-Howell 실패:", conditionMessage(gh))
            } else {
              gh <- as.data.frame(gh)
              gh$dv <- dv
              gh$iv <- iv
              gh$method <- "Games-Howell"
              ph_rows[[length(ph_rows) + 1L]] <- gh
            }
          }
        } else {
          fit <- tryCatch(stats::aov(y ~ g, data = df),
                          error = function(e) e)
          if (inherits(fit, "error")) {
            note <- paste("ANOVA 실패:", conditionMessage(fit))
          } else {
            sm <- summary(fit)[[1]]
            a_rows[[length(a_rows) + 1L]] <- tibble::tibble(
              dv = dv, iv = iv, n = n, method = method,
              F_value = unname(sm[["F value"]][1]),
              df1 = unname(sm[["Df"]][1]),
              df2 = unname(sm[["Df"]][2]),
              p_value = unname(sm[["Pr(>F)"]][1])
            )
            # 표준화 사후검정: Tukey 또는 pooled t-test
            if (selected == "tukey") {
              tk <- tryCatch(stats::TukeyHSD(fit, "g")$g,
                             error = function(e) e)
              if (!inherits(tk, "error")) {
                ph_rows[[length(ph_rows) + 1L]] <- data.frame(
                  dv = dv, iv = iv, method = "Tukey",
                  comparison = rownames(tk), estimate = unname(tk[, "diff"]),
                  conf.low = unname(tk[, "lwr"]),
                  conf.high = unname(tk[, "upr"]),
                  p.adj = unname(tk[, "p adj"]), row.names = NULL
                )
              } else note <- paste("Tukey 실패:", conditionMessage(tk))
            } else if (selected %in% c("lsd", "bonferroni", "holm")) {
              adj <- if (selected == "lsd") p.adj else selected
              pw <- tryCatch(stats::pairwise.t.test(df$y, df$g,
                             p.adjust.method = adj, pool.sd = TRUE),
                             error = function(e) e)
              if (!inherits(pw, "error")) {
                ij <- which(!is.na(pw$p.value), arr.ind = TRUE)
                if (nrow(ij)) {
                  ph_rows[[length(ph_rows) + 1L]] <- data.frame(
                    dv = dv, iv = iv,
                    method = paste0("Pooled pairwise t (", adj, ")"),
                    comparison = paste(rownames(pw$p.value)[ij[, 1]],
                                       colnames(pw$p.value)[ij[, 2]], sep = " - "),
                    p.adj = unname(pw$p.value[ij]), row.names = NULL
                  )
                }
              } else note <- paste("쌍별 비교 실패:", conditionMessage(pw))
            }
            # Scheffe/Duncan 및 기존 jjstat 결과는 원형 그대로 보관
            jj_selected <- if (selected %in% c("bonferroni", "holm")) "lsd" else selected
            jj_adj <- if (selected %in% c("bonferroni", "holm")) selected else p.adj
            raw_a[[key]] <- call_jj(aov_df, list(
              data = df, dv_var = "y", iv_vars = "g",
              posthoc = jj_selected, p.adj = jj_adj,
              digits = digits, sig = TRUE
            ))
            if (inherits(raw_a[[key]], "jj_group_error")) {
              note <- paste(c(note, paste("aov_df:", raw_a[[key]]$message)), collapse = "; ")
            }
          }
        }
      }
      diag_rows[[length(diag_rows) + 1L]] <- tibble::tibble(
        dv = dv, iv = iv, n = n, groups = k,
        min_n = if (k) min(nn) else NA_integer_,
        max_n = if (k) max(nn) else NA_integer_,
        levene_p = lev_p, method = method, posthoc = ph_method,
        note = note
      )
    }
  }

  # 6. 반환 구조를 안정적으로 정리 (서로 다른 형태의 원래 객체는 결합하지 않음)
  ttest_summary <- if (length(t_rows)) {
    do.call(rbind, t_rows)
  } else tibble::tibble(dv = character(), iv = character(),
                       method = character(), n = integer(),
                       group1 = character(), group2 = character(),
                       t_value = numeric(), df = numeric(),
                       p_value = numeric(), mean_diff = numeric(),
                       ci_low = numeric(), ci_high = numeric())
  anova_summary <- if (length(a_rows)) do.call(rbind, a_rows) else
    tibble::tibble(dv = character(), iv = character(), n = integer(),
                   method = character(), F_value = numeric(),
                   df1 = numeric(), df2 = numeric(), p_value = numeric())
  diagnostics <- do.call(rbind, diag_rows)
  posthoc_result <- if (length(ph_rows)) {
    # 서로 다른 사후검정 열을 보존하면서 결합
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("posthoc 결과 결합에는 dplyr 패키지가 필요합니다.")
    }
    dplyr::bind_rows(ph_rows)
  } else tibble::tibble()
  gh <- if (nrow(posthoc_result) && "method" %in% names(posthoc_result)) {
    posthoc_result[posthoc_result$method == "Games-Howell", , drop = FALSE]
  } else tibble::tibble()
  # 기존 호환성을 위해 dv별 명명 목록 유지
  old_t <- stats::setNames(lapply(dv_vars, function(dv) {
    raw_t[startsWith(names(raw_t), paste0(dv, "::"))]
  }), dv_vars)
  old_a <- stats::setNames(lapply(dv_vars, function(dv) {
    anova_summary[anova_summary$dv == dv, , drop = FALSE]
  }), dv_vars)
  list(factor_info = factor_info, t_vars = t_vars, aov_vars = aov_vars,
       ttest = old_t, anova = old_a, diagnostics = diagnostics,
       games_howell = gh, ttest_summary = ttest_summary,
       posthoc = posthoc_result, ttest_raw = raw_t, anova_raw = raw_a)
}




#' 범주형 독립변수별 다중 t-test 및 일원분산분석
#'
#' @description
#' 여러 연속형 종속변수와 범주형 독립변수의 조합을 순회한다.
#' 유효 수준이 2개이면 독립표본 t-test를, 3개 이상이면 일원분산분석을 수행한다.
#' 자동 사후검정 모드에서는 중앙값 중심 Levene 검정 결과에 따라
#' 일반 ANOVA + Tukey 또는 Welch ANOVA + Games-Howell을 선택한다.
#' 기존 jjstat의 ttest_df() 및 aov_df() 결과도 별도로 보관한다.
#'
#' @details
#' 모든 비교는 각 종속변수와 독립변수의 완전사례를 기준으로 독립적으로 수행한다.
#' 숫자형으로 코딩된 집단변수는 명시적으로 factor로 변환해야 한다.
#' iv_vars = NULL이면 factor 열을 자동 검색한다. 문자형 집단변수를 포함하려면
#' include_character = TRUE로 지정한다. 종속변수는 독립변수 후보에서 제외한다.
#'
#' `posthoc = "auto"`에서 Levene p < levene_alpha이면 Welch ANOVA와
#' Games-Howell 사후검정을, 그 외의 유효한 경우 일반 ANOVA와 Tukey를 적용한다.
#' Levene 계산이 실패하면 보수적으로 Welch ANOVA를 사용한다.
#' 명시적으로 `posthoc = "games-howell"`을 설정하면 Welch ANOVA를 사용한다.
#' 나머지 사후검정 선택은 일반 ANOVA를 사용하므로 등분산 가정을 확인해야 한다.
#'
#' Scheffe 및 Duncan의 사후검정은 jjstat::aov_df()에 위임하며,
#' 표준 posthoc 테이블에는 base R의 Tukey 또는 pairwise t-test 결과와
#' Games-Howell 결과를 제공한다. Scheffe/Duncan의 상세 결과는
#' anova_raw에서 확인한다. LSD는 보정되지 않은 쌍별 pooled t-test이며
#' `posthoc = "lsd"`와 `p.adj = "holm"`을 조합하면 Holm 보정을 적용한다.
#' `posthoc = "bonferroni"` 또는 `"holm"`은 같은 pooled t-test에
#' 각 보정법을 적용한다. 이들은 Tukey HSD와 동일한 검정이 아니다.
#'
#' @param data 분석할 data.frame.
#' @param dv_vars 한 개 이상의 numeric 종속변수명을 담은 문자형 벡터.
#' @param digits 출력 시 사용되는 소수점 자리수. 원래 수치형 결과는 반올림하지 않음.
#' @param posthoc `"auto"`, `"tukey"`, `"scheffe"`, `"duncan"`,
#'   `"lsd"`, `"bonferroni"`, `"holm"`, `"games-howell"` 중 하나.
#' @param p.adj LSD 사후비교 p값 보정 방법. stats::p.adjust.methods 중 하나.
#' @param var.equal t-test에서 공통분산을 가정할지 여부. FALSE면 Welch t-test.
#' @param levene_alpha 자동 분산동질성 판단 기준. 0과 1 사이의 유의수준.
#' @param iv_vars 사용할 독립변수 이름의 문자형 벡터. NULL이면 자동 검색.
#' @param include_character TRUE이면 문자형 열도 범주형 독립변수 후보로 포함.
#'
#' @return 다음 요소를 가지는 list:
#' \describe{
#'   \item{factor_info}{독립변수별 관측 수준 수 및 최초 분류.}
#'   \item{t_vars}{전체 데이터에서 2수준인 독립변수명.}
#'   \item{aov_vars}{전체 데이터에서 3수준 이상인 독립변수명.}
#'   \item{ttest}{종속변수별 jjstat::ttest_df() 원래 결과 목록.}
#'   \item{anova}{종속변수별 표준화 ANOVA 결과표 목록.}
#'   \item{diagnostics}{분석 조합별 표본 수, Levene, 분석 상태.}
#'   \item{games_howell}{Games-Howell 사후검정 결과.}
#'   \item{ttest_summary}{t-test 검정통계량 및 p값.}
#'   \item{posthoc}{기계적으로 결합 가능한 사후검정 결과.}
#'   \item{ttest_raw}{각 조합의 jjstat::ttest_df() 원래 반환 객체.}
#'   \item{anova_raw}{각 조합의 jjstat::aov_df() 원래 반환 객체.}
#' }
#' @seealso \code{\link{ttest_df}}, \code{\link{aov_df}},
#'   \code{\link[stats]{oneway.test}}, \code{\link[stats]{TukeyHSD}}
#' @references
#' Welch, B. L. (1951). On the comparison of several mean values:
#' An alternative approach. Biometrika, 38(3/4), 330-336.
#'
#' Games, P. A., & Howell, J. F. (1976). Pairwise multiple comparison
#' procedures with unequal n's and/or variances: A Monte Carlo study.
#' Journal of Educational Statistics, 1(2), 113-125.
#'
#' @export
#' @examples
#' # Example 1: mtcars - 숫자코드 집단변수 factor화
#' d <- mtcars
#' d$am <- factor(d$am)
#' d$vs <- factor(d$vs)
#' d$cyl <- factor(d$cyl)
#' d$gear <- factor(d$gear)
#' x <- jj_group_compare(d, c("mpg", "hp"))
#' x$factor_info
#' x$ttest_summary
#' x$diagnostics
#'
#' # Example 2: 특정 독립변수만 비교
#' x2 <- jj_group_compare(d, "mpg", iv_vars = c("am", "cyl"))
#' x2$anova$mpg
#'
#' # Example 3: Tukey 사후검정
#' x3 <- jj_group_compare(d, "mpg", posthoc = "tukey")
#' x3$posthoc
#'
#' # Example 4: Holm 보정 LSD
#' x4 <- jj_group_compare(d, "mpg", posthoc = "lsd", p.adj = "holm")
#' x4$posthoc
#'
#' # Example 5: Bonferroni 보정 비교
#' x5 <- jj_group_compare(d, "mpg", posthoc = "bonferroni")
#'
#' # Example 6: Welch ANOVA 및 Games-Howell
#' x6 <- jj_group_compare(d, "mpg", posthoc = "games-howell")
#' x6$games_howell
#'
#' # Example 7: iris, 세 종별 다중 종속변수
#' x7 <- jj_group_compare(iris,
#'   c("Sepal.Length", "Petal.Length", "Petal.Width"))
#' x7$anova$Sepal.Length
#'
#' # Example 8: 문자형 범주변수 포함
#' d8 <- transform(iris, Species = as.character(Species))
#' x8 <- jj_group_compare(d8, "Sepal.Width", include_character = TRUE)
#'
#' # Example 9: 결측치 포함
#' d9 <- d
#' d9$mpg[1:4] <- NA_real_
#' x9 <- jj_group_compare(d9, "mpg")
#' x9$diagnostics
#'
#' # Example 10: Scheffe - 상세 결과는 anova_raw에 보존
#' \dontrun{
#' x10 <- jj_group_compare(d, "mpg", posthoc = "scheffe")
#' x10$anova_raw
#' }
#'
#' # Example 11: Duncan - 상세 결과는 anova_raw에 보존
#' \dontrun{
#' x11 <- jj_group_compare(d, "mpg", posthoc = "duncan")
#' x11$anova_raw
#' }
#'
#' # Example 12: t-test만 실행
#' x12 <- jj_group_compare(d, "mpg", iv_vars = c("am", "vs"))
#' x12$ttest_summary
#'
#' # Example 13: 분산 동질성 기준 변경
#' x13 <- jj_group_compare(d, "mpg", levene_alpha = 0.01)
#'
#' # Example 14: 원래 jjstat 스타일의 결과 확인
#' x14 <- jj_group_compare(d, "mpg", iv_vars = "cyl")
#' x14$anova_raw
aov_group_compare <- function(
    data,
    dv_vars,
    digits = 3,
    posthoc = "auto",
    p.adj = "holm",
    var.equal = FALSE,
    levene_alpha = 0.05,
    iv_vars = NULL,
    include_character = FALSE
) {
  # 1. 입력 형식과 패키지 의존성 검사
  if (!is.data.frame(data)) stop("data는 data.frame이어야 합니다.")
  if (!is.character(dv_vars) || !length(dv_vars) ||
      anyNA(dv_vars) || anyDuplicated(dv_vars)) {
    stop("dv_vars에는 중복·결측이 없는 종속변수명을 입력하세요.")
  }
  if (!all(dv_vars %in% names(data))) stop("존재하지 않는 종속변수가 있습니다.")
  if (!all(vapply(data[dv_vars], is.numeric, logical(1)))) {
    stop("종속변수는 모두 numeric이어야 합니다.")
  }
  if (length(digits) != 1L || !is.numeric(digits) || is.na(digits) ||
      digits < 0 || digits != as.integer(digits)) stop("digits는 0 이상의 정수여야 합니다.")
  if (length(levene_alpha) != 1L || !is.numeric(levene_alpha) ||
      is.na(levene_alpha) || levene_alpha <= 0 || levene_alpha >= 1) {
    stop("levene_alpha는 0과 1 사이여야 합니다.")
  }
  if (!is.logical(var.equal) || length(var.equal) != 1L || is.na(var.equal)) {
    stop("var.equal은 TRUE 또는 FALSE여야 합니다.")
  }
  if (!is.logical(include_character) || length(include_character) != 1L ||
      is.na(include_character)) stop("include_character는 TRUE 또는 FALSE여야 합니다.")
  choices <- c("auto", "tukey", "scheffe", "duncan", "lsd",
               "bonferroni", "holm", "games-howell")
  posthoc <- match.arg(posthoc, choices)
  p.adj <- match.arg(p.adj, stats::p.adjust.methods)
  for (pkg in c("car", "rstatix", "tibble")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("필요한 패키지를 설치하세요: ", pkg)
    }
  }

  # 2. 독립변수 후보 식별 및 factor 변환
  if (is.null(iv_vars)) {
    is_group <- vapply(data, function(x) {
      is.factor(x) || (include_character && is.character(x))
    }, logical(1))
    iv_vars <- setdiff(names(data)[is_group], dv_vars)
  } else {
    if (!is.character(iv_vars) || anyNA(iv_vars) || anyDuplicated(iv_vars)) {
      stop("iv_vars는 중복이나 결측 없는 문자형 변수명이어야 합니다.")
    }
    if (!all(iv_vars %in% names(data))) stop("존재하지 않는 독립변수가 있습니다.")
    if (any(iv_vars %in% dv_vars)) stop("종속변수는 독립변수로 지정할 수 없습니다.")
    if (!all(vapply(data[iv_vars], function(x) is.factor(x) || is.character(x), logical(1)))) {
      stop("독립변수는 factor/character여야 합니다. 숫자형 코드는 factor로 변환하세요.")
    }
  }
  if (!length(iv_vars)) stop("사용 가능한 범주형 독립변수가 없습니다.")
  group_data <- data
  for (iv in iv_vars) group_data[[iv]] <- factor(as.character(data[[iv]]))
  nlevels_all <- vapply(group_data[iv_vars], nlevels, integer(1))
  factor_info <- tibble::tibble(
    variable = iv_vars,
    n_levels = unname(nlevels_all),
    method = ifelse(nlevels_all == 2L, "t-test",
                    ifelse(nlevels_all >= 3L, "ANOVA", "Exclude"))
  )
  t_vars <- factor_info$variable[factor_info$method == "t-test"]
  aov_vars <- factor_info$variable[factor_info$method == "ANOVA"]

  # 3. 빈 출력 기본 형식 정의: 성공/실패 결과를 일관되게 결합
  t_rows <- list()
  a_rows <- list()
  diag_rows <- list()
  ph_rows <- list()
  raw_t <- list()
  raw_a <- list()
  call_jj <- function(fun, args) {
    tryCatch(do.call(fun, args), error = function(e) {
      structure(list(message = conditionMessage(e)), class = "jj_group_error")
    })
  }

  # 4. 각 종속변수 × 독립변수별 완전사례를 만들고 재분류
  for (dv in dv_vars) {
    for (iv in iv_vars) {
      df <- data.frame(y = data[[dv]], g = group_data[[iv]])
      df <- df[stats::complete.cases(df) & is.finite(df$y), , drop = FALSE]
      df$g <- droplevels(df$g)
      nn <- as.integer(table(df$g))
      k <- length(nn)
      n <- nrow(df)
      key <- paste(dv, iv, sep = "::")
      lev_p <- NA_real_
      method <- NA_character_
      ph_method <- NA_character_
      note <- NA_character_

      # 빈 집단/작은 표본을 별도 기록하고 다음 비교로 진행
      if (k < 2L || any(nn < 2L)) {
        note <- "분석에 필요한 집단 수 또는 집단별 관측치(n >= 2)가 부족함"
      } else if (k == 2L) {
        method <- if (var.equal) "Student t-test" else "Welch t-test"
        fit <- tryCatch(stats::t.test(y ~ g, data = df,
                                      var.equal = var.equal),
                        error = function(e) e)
        if (inherits(fit, "error")) {
          note <- paste("t-test 실패:", conditionMessage(fit))
        } else {
          t_rows[[length(t_rows) + 1L]] <- tibble::tibble(
            dv = dv, iv = iv, method = method, n = n,
            group1 = levels(df$g)[1], group2 = levels(df$g)[2],
            t_value = unname(fit$statistic), df = unname(fit$parameter),
            p_value = fit$p.value,
            mean_diff = unname(fit$estimate[1] - fit$estimate[2]),
            ci_low = fit$conf.int[1], ci_high = fit$conf.int[2]
          )
        }
        # 원래 jjstat 결과는 조합별로 호출하여 오류를 격리
        raw_t[[key]] <- call_jj(ttest_df, list(
          data = df, dv_var = "y", iv_vars = "g", var.equal = var.equal,
          digits = digits, sig = TRUE
        ))
        if (inherits(raw_t[[key]], "jj_group_error")) {
          note <- paste(c(note, paste("ttest_df:", raw_t[[key]]$message)), collapse = "; ")
        }
      } else {
        # 5. 3개 이상 집단: Levene 진단
        lv <- tryCatch(car::leveneTest(y ~ g, data = df, center = stats::median),
                       error = function(e) NULL)
        if (!is.null(lv)) lev_p <- as.numeric(lv[["Pr(>F)"]][1])
        use_welch <- if (posthoc == "auto") {
          !is.finite(lev_p) || lev_p < levene_alpha
        } else {
          identical(posthoc, "games-howell")
        }
        method <- if (use_welch) "Welch ANOVA" else "Classical ANOVA"
        selected <- if (use_welch) "games-howell" else
          if (posthoc == "auto") "tukey" else posthoc
        ph_method <- selected
        if (!is.finite(lev_p)) note <- "Levene 계산 실패: Welch 적용 또는 가정 재확인"
        if (!use_welch && is.finite(lev_p) && lev_p < levene_alpha) {
          note <- "등분산성 위반 가능성: 일반 ANOVA 사후검정 강제 지정됨"
        }
        if (use_welch) {
          fit <- tryCatch(stats::oneway.test(y ~ g, data = df,
                                             var.equal = FALSE),
                          error = function(e) e)
          if (inherits(fit, "error")) {
            note <- paste("Welch ANOVA 실패:", conditionMessage(fit))
          } else {
            a_rows[[length(a_rows) + 1L]] <- tibble::tibble(
              dv = dv, iv = iv, n = n, method = method,
              F_value = unname(fit$statistic),
              df1 = unname(fit$parameter[1]),
              df2 = unname(fit$parameter[2]), p_value = fit$p.value
            )
            gh <- tryCatch(rstatix::games_howell_test(df, y ~ g),
                           error = function(e) e)
            if (inherits(gh, "error")) {
              note <- paste("Games-Howell 실패:", conditionMessage(gh))
            } else {
              gh <- as.data.frame(gh)
              gh$dv <- dv
              gh$iv <- iv
              gh$method <- "Games-Howell"
              ph_rows[[length(ph_rows) + 1L]] <- gh
            }
          }
        } else {
          fit <- tryCatch(stats::aov(y ~ g, data = df),
                          error = function(e) e)
          if (inherits(fit, "error")) {
            note <- paste("ANOVA 실패:", conditionMessage(fit))
          } else {
            sm <- summary(fit)[[1]]
            a_rows[[length(a_rows) + 1L]] <- tibble::tibble(
              dv = dv, iv = iv, n = n, method = method,
              F_value = unname(sm[["F value"]][1]),
              df1 = unname(sm[["Df"]][1]),
              df2 = unname(sm[["Df"]][2]),
              p_value = unname(sm[["Pr(>F)"]][1])
            )
            # 표준화 사후검정: Tukey 또는 pooled t-test
            if (selected == "tukey") {
              tk <- tryCatch(stats::TukeyHSD(fit, "g")$g,
                             error = function(e) e)
              if (!inherits(tk, "error")) {
                ph_rows[[length(ph_rows) + 1L]] <- data.frame(
                  dv = dv, iv = iv, method = "Tukey",
                  comparison = rownames(tk), estimate = unname(tk[, "diff"]),
                  conf.low = unname(tk[, "lwr"]),
                  conf.high = unname(tk[, "upr"]),
                  p.adj = unname(tk[, "p adj"]), row.names = NULL
                )
              } else note <- paste("Tukey 실패:", conditionMessage(tk))
            } else if (selected %in% c("lsd", "bonferroni", "holm")) {
              adj <- if (selected == "lsd") p.adj else selected
              pw <- tryCatch(stats::pairwise.t.test(df$y, df$g,
                                                    p.adjust.method = adj, pool.sd = TRUE),
                             error = function(e) e)
              if (!inherits(pw, "error")) {
                ij <- which(!is.na(pw$p.value), arr.ind = TRUE)
                if (nrow(ij)) {
                  ph_rows[[length(ph_rows) + 1L]] <- data.frame(
                    dv = dv, iv = iv,
                    method = paste0("Pooled pairwise t (", adj, ")"),
                    comparison = paste(rownames(pw$p.value)[ij[, 1]],
                                       colnames(pw$p.value)[ij[, 2]], sep = " - "),
                    p.adj = unname(pw$p.value[ij]), row.names = NULL
                  )
                }
              } else note <- paste("쌍별 비교 실패:", conditionMessage(pw))
            }
            # Scheffe/Duncan 및 기존 jjstat 결과는 원형 그대로 보관
            jj_selected <- if (selected %in% c("bonferroni", "holm")) "lsd" else selected
            jj_adj <- if (selected %in% c("bonferroni", "holm")) selected else p.adj
            raw_a[[key]] <- call_jj(aov_df, list(
              data = df, dv_var = "y", iv_vars = "g",
              posthoc = jj_selected, p.adj = jj_adj,
              digits = digits, sig = TRUE
            ))
            if (inherits(raw_a[[key]], "jj_group_error")) {
              note <- paste(c(note, paste("aov_df:", raw_a[[key]]$message)), collapse = "; ")
            }
          }
        }
      }
      diag_rows[[length(diag_rows) + 1L]] <- tibble::tibble(
        dv = dv, iv = iv, n = n, groups = k,
        min_n = if (k) min(nn) else NA_integer_,
        max_n = if (k) max(nn) else NA_integer_,
        levene_p = lev_p, method = method, posthoc = ph_method,
        note = note
      )
    }
  }

  # 6. 반환 구조를 안정적으로 정리 (서로 다른 형태의 원래 객체는 결합하지 않음)
  ttest_summary <- if (length(t_rows)) {
    do.call(rbind, t_rows)
  } else tibble::tibble(dv = character(), iv = character(),
                        method = character(), n = integer(),
                        group1 = character(), group2 = character(),
                        t_value = numeric(), df = numeric(),
                        p_value = numeric(), mean_diff = numeric(),
                        ci_low = numeric(), ci_high = numeric())
  anova_summary <- if (length(a_rows)) do.call(rbind, a_rows) else
    tibble::tibble(dv = character(), iv = character(), n = integer(),
                   method = character(), F_value = numeric(),
                   df1 = numeric(), df2 = numeric(), p_value = numeric())
  diagnostics <- do.call(rbind, diag_rows)
  posthoc_result <- if (length(ph_rows)) {
    # 서로 다른 사후검정 열을 보존하면서 결합
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("posthoc 결과 결합에는 dplyr 패키지가 필요합니다.")
    }
    dplyr::bind_rows(ph_rows)
  } else tibble::tibble()
  gh <- if (nrow(posthoc_result) && "method" %in% names(posthoc_result)) {
    posthoc_result[posthoc_result$method == "Games-Howell", , drop = FALSE]
  } else tibble::tibble()
  # 기존 호환성을 위해 dv별 명명 목록 유지
  old_t <- stats::setNames(lapply(dv_vars, function(dv) {
    raw_t[startsWith(names(raw_t), paste0(dv, "::"))]
  }), dv_vars)
  old_a <- stats::setNames(lapply(dv_vars, function(dv) {
    anova_summary[anova_summary$dv == dv, , drop = FALSE]
  }), dv_vars)
  list(factor_info = factor_info, t_vars = t_vars, aov_vars = aov_vars,
       ttest = old_t, anova = old_a, diagnostics = diagnostics,
       games_howell = gh, ttest_summary = ttest_summary,
       posthoc = posthoc_result, ttest_raw = raw_t, anova_raw = raw_a)
}
