#' Create CFA result summary by lavaan sem estimated result By Park Joonghee Ph.D
#' @param x lavaan result
#' @param format knitr markdown decision
#' @param cut loading criteran
#' @param angle tesxt angle
#' @param cex text size
#' @param hjust on text value gap
#' @param val.size text size
#' @param dis.sort Logical. For loading bars, \code{FALSE} (default) preserves the
#'   measurement order returned in \code{variable_order}; \code{TRUE}
#'   sorts the original item names lexicographically (e.g., Q1, Q10, Q2).
#'   With \code{FALSE}, legend entries follow the first appearance of each
#'   latent factor in the model; with \code{TRUE}, they are sorted by name.
#'   Sorting does not require renaming and is independent of display labels.
#' @param rename Logical. If \code{TRUE}, use \code{var_name} as display names
#'   in the loading table and bar plot. This does not control item order.
#' @param var_name Display names in the original measurement order, with one
#'   non-missing name per loading row. Ignored when \code{rename = FALSE}.
#' @param digits value rounding
#' @param type Character string selecting the result (case-sensitive):
#'   \describe{
#'     \item{\code{all}}{Default: a list of model syntax, fit tables, loadings,
#'       reliability, validity, latent paths, loading bars, and variable order.}
#'     \item{\code{raw}, \code{data.frame}}{A list of unformatted fit, loading,
#'       convergent/discriminant validity, HTMT and latent-path data, plus the plot.}
#'     \item{\code{model}}{Model syntax extracted from the fitted model.}
#'     \item{\code{modelfit}}{Fit indices with criteria, as a data frame.}
#'     \item{\code{modelfit_paper}}{Publication-oriented fit summary in raw format.}
#'     \item{\code{modelfit2}}{Formatted model-fit table.}
#'     \item{\code{loadings}, \code{item}, \code{indicator}}{Loading data with
#'       estimates, standard errors, standardized loadings, tests and acceptance labels.}
#'     \item{\code{loadings_bar}}{A ggplot of standardized loadings. Only this
#'       output skips fit, reliability and validity calculations when \code{web = FALSE}.}
#'     \item{\code{alpha}}{Cronbach alpha, composite reliability and their checks.}
#'     \item{\code{CR_AVE}, \code{Convergent}}{Cronbach alpha, composite
#'       reliability (omega3) and average variance extracted (AVE). If omega3
#'       exceeds 1, CR is recalculated from standardized loadings as
#'       \eqn{(\sum \lambda)^2 / ((\sum \lambda)^2 + \sum(1 - \lambda^2))}.
#'       Conversion details appear below affected web tables and in the
#'       \code{CR_adjustments} attribute of the full/raw result and convergent data.
#'       Invalid standardized loadings or denominators yield NA instead.}
#'     \item{\code{item_CR_AVE}}{Combined loading, CR and AVE table.}
#'     \item{\code{item_CR_AVE_web}}{Combined loading, CR and AVE table via web().}
#'     \item{\code{fl_criteria}, \code{Discriminant}}{Fornell-Larcker
#'       discriminant-validity data (latent correlations and square root of AVE).}
#'     \item{\code{htmt}, \code{HTMT}}{HTMT matrix after excluding single-item
#'       factors before calculation. At least two multi-item factors are required;
#'       otherwise returns NULL with an explanation in the full result.}
#'     \item{\code{str_cor}}{Regression/covariance estimates and significance data.}
#'   }
#' @param web Logical, default \code{TRUE}. If \code{TRUE}, always render
#'   the complete \code{type = "all"} result in a local web page, regardless
#'   of \code{type}. The page provides HTML/Markdown downloads and a copy
#'   button on each table (shown on hover or keyboard focus), code copying, and
#'   PNG downloads for the loading plot and two \code{semPaths2()} model diagrams
#'   at the bottom: unstandardized (\code{whatLabels = "est"}) and fully
#'   standardized (\code{whatLabels = "std"}) coefficients.
#'   Use \code{web = FALSE} to return a selected R result without opening a page.
#'   No server is needed.
#'   The full result is returned invisibly with its path in \code{attr(result, "web_file")}.
#' @param path_args Named list of overrides passed to \code{semPaths2()} for
#'   the web diagram. Defaults include \code{sizeMan = 5}, \code{sizeMan2 = 2},
#'   \code{sizeLat = 6}, \code{sizeLat2 = 3}, and \code{curve = 3}.
#'   Image dimensions adapt to the number of observed and latent variables.
#'   Both diagrams share these settings; \code{whatLabels} is set separately
#'   to \code{"est"} and \code{"std"} for the two coefficient versions.
#' @param htmt_cut htmt_cut is default =1.
#' if you want roburst option htmt_cut =0.9
#' @param htmt2 TRUE using htmt2
#' @importFrom magrittr %>%

##' @examples
#' \dontrun{
#' library(lavaan)
#' data(PoliticalDemocracy)
#'
#' model1 <- "
#'   ind60 =~ x1 + x2 + x3
#'   dem60 =~ y1 + y2 + y3 + y4
#'   dem65 =~ y5 + y6 + y7 + y8
#'   dem60 ~ ind60
#'   dem65 ~ ind60 + dem60
#' "
#'
#' fit1 <- sem(model1, data = PoliticalDemocracy)
#'
#' # 1. 기본 사용법: 전체 결과를 웹페이지로 출력
#' # 기본값: web = TRUE, dis.sort = FALSE
#' # 막대와 범례는 분석 결과의 변수/요인 순서를 유지합니다.
#' cfa2(fit1)
#'
#' # 2. 확인적요인분석 절차: 특정 결과를 R 객체로 받기
#' # res가 아닌 type을 사용하고, web = FALSE를 지정합니다.
#' cfa2(fit1, type = "modelfit",     web = FALSE) # 모델 적합도
#' cfa2(fit1, type = "loadings",     web = FALSE) # 지표타당도: 요인적재량
#' cfa2(fit1, type = "alpha",        web = FALSE) # 신뢰도: Cronbach alpha, CR
#' cfa2(fit1, type = "CR_AVE",       web = FALSE) # 수렴타당도: CR, AVE
#' cfa2(fit1, type = "Discriminant", web = FALSE) # Fornell-Larcker 판별타당도
#' cfa2(fit1, type = "htmt",         web = FALSE) # HTMT 판별타당도 (Henseler et al., 2015)
#' cfa2(fit1, type = "str_cor",      web = FALSE) # 잠재변수 간 경로/공분산 유의성
#' # HTMT는 단일 문항 요인을 계산 전에 제외합니다.
#' # dataset/model 인수를 추가할 필요 없이 fit1에서 데이터를 추출합니다.
#' cfa2(fit1, type = "htmt", htmt2 = TRUE, htmt_cut = 0.85, web = FALSE)
#'
#' # 3. 웹 보고서 기능
#' result <- cfa2(fit1, web = TRUE)
#' # 상단: 전체 보고서를 .html 또는 .md로 다운로드
#' # 각 표: 마우스를 올리거나 키보드로 이동하면 표 복사 버튼 표시
#' # 모델 구문 및 도형 R 코드: 코드 복사 버튼 제공
#' # 적재량 막대그래프: PNG 다운로드
#' # 보고서 하단: 비표준화/표준화 모델 도형과 각각의 PNG 다운로드
#' # 웹 출력에서도 전체 결과는 result에 저장됩니다.
#' result$variable_order
#' attr(result, "web_file") # 생성된 HTML 파일 경로
#'
#' # 도형 크기와 곡률 조절: 두 계수 도형에 공통 적용
#' cfa2(fit1, web = TRUE, path_args = list(
#'   sizeMan = 5, sizeMan2 = 2, sizeLat = 6, sizeLat2 = 3, curve = 3
#' ))
#' # web = TRUE이면 type 지정과 관계없이 전체 결과를 웹페이지로 출력합니다.
#'
#' # 4. 막대그래프만 R에서 받기 (기본: 분석 순서, 범례도 요인 순서)
#' cfa2(fit1, type = "loadings_bar", web = FALSE)
#' # 원래 변수명과 범례를 이름순으로 정렬하려는 경우
#' cfa2(fit1, type = "loadings_bar", dis.sort = TRUE, web = FALSE)
#' # 색상 변경
#' cfa2(fit1, type = "loadings_bar", web = FALSE) + ggplot2::scale_fill_grey()
#'
#' # 5. 표시 이름을 바꿀 때만 rename = TRUE 사용
#' # 이름은 result$variable_order의 순서에 맞춰 지정합니다.
#' new_names <- paste0("Indicator", seq_along(result$variable_order))
#' cfa2(fit1, rename = TRUE, var_name = new_names)
#' # rename = FALSE이면 var_name은 무시되며 정렬에는 영향을 주지 않습니다.
#'
#' # 6. 전체 R 결과 및 원본 데이터 받기
#' all_results <- cfa2(fit1, web = FALSE)
#' raw_results <- cfa2(fit1, type = "raw", web = FALSE)
#' # 특정 표를 HTML로 변환하기
#' knitr::kable(cfa2(fit1, type = "loadings", web = FALSE), format = "html")
#' }


#' @export
#'

cfa2 <- function(x,
                 format="markdown",
                 htmt_cut = 0.9,
                 htmt2=FALSE,
                 cut=0.7,
                 angle=90,
                 cex=11, hjust=0.9,
                 val.size=4,
                 dis.sort=FALSE,
                 rename=FALSE,
                 var_name=NA,
                 digits=3,
                 type = "all",
                 web = TRUE,
                 path_args = list()){

  if (!is.logical(web) || length(web) != 1L || is.na(web)) {
    stop("web must be TRUE or FALSE.")
  }
  if (!is.list(path_args) ||
      (length(path_args) && (is.null(names(path_args)) ||
                            anyNA(names(path_args)) || any(names(path_args) == "") ||
                            anyDuplicated(names(path_args))))) {
    stop("path_args must be a named list with unique argument names.")
  }
  if (isTRUE(web)) type <- "all"

  library(dplyr); library(lavaan); library(ggplot2)

  # A loading plot only needs parameter estimates. In particular, do not run
  # ordinal reliability integration or fit measures for this output.
  dataplot0 <- lavaan::parameterEstimates(x, standardized=TRUE) %>%
    dplyr::filter(op=="=~") %>%
    mutate(stars = ifelse(pvalue < .001,"***",
                          ifelse(pvalue < .01,"**",
                                 ifelse(pvalue < .05,"*","")))) %>%
    mutate(label = ifelse(std.all>0.7,"Yes(Good)",
                          ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
    dplyr::select(Latent=lhs, Item=rhs, Est=est, S.E.=se, cr=z, Sig.=stars, p=pvalue, std=std.all, beta_Accept=label)
  dataplot <- dataplot0 %>% dplyr::select(Item, Latent, std)
  varnames_check <- dataplot0[,"Item"]
  if (!is.logical(dis.sort) || length(dis.sort) != 1L || is.na(dis.sort)) {
    stop("dis.sort must be TRUE or FALSE.")
  }
  # Decide order from original item identities, before applying display names.
  item_order <- unique(varnames_check)
  if (isTRUE(dis.sort)) item_order <- sort(item_order)
  if (isTRUE(rename)) {
    if (length(var_name) != nrow(dataplot) || anyNA(var_name)) {
      stop("var_name must contain one non-missing name per loading row.")
    }
    dataplot$Item <- as.character(var_name)
  }
  display_order <- unique(dataplot$Item[match(item_order, varnames_check)])

  legend_order <- if (isTRUE(dis.sort)) sort(unique(dataplot$Latent)) else unique(dataplot$Latent)
  gg <- ggplot(dataplot, aes(x=Item, y=std, fill=Latent)) +
    scale_x_discrete(limits = display_order) +
    scale_fill_discrete(breaks = legend_order) +
    geom_bar(stat="identity", position='dodge') +
    geom_hline(yintercept = cut , color= "darkred", linetype = 2) +
    geom_hline(yintercept = cut-0.2, color= "gray40") +
    ggtitle("Factor loadings") +
    theme_bw() +
    geom_text(aes(label=round(std,2)), vjust=-.3, size=val.size) +
    ylim(0, 1.1) +
    theme(axis.text.x = element_text(angle=angle, size = cex, hjust = hjust, face="bold"))

  if (identical(type, "loadings_bar")) return(gg)

  library(knitr); library(tibble); library(semPlot)
  library(kableExtra); library(tidyverse)
  options(scipen = 100, knitr.kable.NA="")

  # [NEW] ---------- helpers ----------
  .meas_map <- function(x){
    lavaan::parameterEstimates(x, standardized = TRUE) |>
      dplyr::filter(op == "=~") |>
      dplyr::transmute(Latent = lhs, Item = rhs, lambda = std.all)
  }
  .lv_counts <- function(mm) mm |>
    dplyr::count(Latent, name = "k")
  .singles <- function(mm) .lv_counts(mm) |>
    dplyr::filter(k == 1) |>
    dplyr::pull(Latent) |>
    unique()

  # Cronbach alpha from model-implied item correlation
  .alpha_from_cor <- function(C){
    k <- ncol(C)
    if (is.null(k) || k < 2) return(NA_real_)
    s_total <- sum(C)                 # sum of all entries
    s_diag  <- sum(diag(C))           # sum of variances (=k in correlation)
    alpha <- (k/(k-1)) * (1 - (s_diag / s_total))
    as.numeric(alpha)
  }

  # CR & AVE from standardized loadings
  .cr_ave_from_lambda <- function(lambda_vec){
    k <- length(lambda_vec)
    if (k < 2) return(c(CR = NA_real_, AVE = NA_real_))
    sum_l <- sum(lambda_vec, na.rm=TRUE)
    sum_l2 <- sum(lambda_vec^2, na.rm=TRUE)
    theta_sum <- sum(1 - lambda_vec^2, na.rm=TRUE)
    CR  <- (sum_l^2) / ((sum_l^2) + theta_sum)
    AVE <- (sum_l2) / k
    c(CR = as.numeric(CR), AVE = as.numeric(AVE))
  }

  # safe drop latents (rows/cols) by names
  .drop_latents_square <- function(M, drop_names){
    if (is.null(M) || length(drop_names)==0) return(M)
    rn <- rownames(M); cn <- colnames(M)
    if (is.null(rn) || is.null(cn)) return(M)
    keep_r <- setdiff(rn, drop_names)
    keep_c <- setdiff(cn, drop_names)
    M[keep_r, keep_c, drop=FALSE]
  }
  # -----------------------------------

  # 01. Fit measures (동일)
  fit.indices <- c("chisq","pvalue","df","rmsea","gfi","agfi","srmr","cfi","tli","nfi","aic","bic")
  fitMeasures_vec <- round(lavaan::fitMeasures(x, fit.indices), 3)
  fitMeasures <- as.data.frame(fitMeasures_vec)
  fitMeasures$critera <- c("","*p.value >= 0.05","_chisq/df <= 3(<5(ok)","*RMSEA< 0.05(or 0.08)",
                           "*GFI >= 0.95","_AGFI>= 0.90","*SRMR < 0.08","*CFI >= 0.95",
                           "_TLI >= 0.90","_NFI >= 0.90","_lower","_lower")
  fitMeasures$Ref <- c("-","-","Wheaton et al.(1977)","Browne & Cudek(1993)",
                       "Jöreskog–Sörbom(1970)","Tanaka & Huba(1985)","Hu & Bentler(1999)",
                       "Kline(2016)","Bentler & Bonett(1980)","Bollen(1989)","Akaike(1973)","-")
  fitMeasures$chiq_df <- c("","", round(fitMeasures[1,1]/fitMeasures[3,1],2),
                           rep("", 9))
  fit <- fitMeasures %>%
    knitr::kable(digits=3, format=format,
                 caption="FitMeasure and criterian (*) satisfy (Kline, 2016)")

  # robust/general block 그대로 유지
  if (TRUE) { # standard fit indices; robust indices not auto-selected
    fitdata_00 <- lavaan::fitMeasures(x, c("chisq","df","pvalue",
                                   "rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue",
                                   "srmr","gfi","cfi","tli","aic","bic"))
    criteria_data_00 <- c("Chisq","df","p > .05","RMSEA < .05","90%CI.L","90%CI.U","p <= .05",
                          "SRMR < .08","GFI > .90","CFI > .90","TLI > .90","AIC lower","BIC lower")
    modelfitdata <- cbind.data.frame(criterian = criteria_data_00,
                                     Value = round(fitdata_00,3))
  } else {
    fitdata <- lavaan::fitMeasures(x, c("chisq","df","pvalue","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue",
                                "srmr","gfi","cfi","tli","aic","bic",
                                "chisq.scaled","df.scaled","pvalue.scaled","chisq.scaling.factor",
                                "cfi.robust","tli.robust","rmsea.robust","rmsea.ci.lower.robust",
                                "rmsea.ci.upper.robust","rmsea.pvalue.robust","srmr_bentler","srmr_mplus"))
    criteria_data <- c("Chisq","df","p > .05","RMSEA < .05","90%CI.L","90%CI.U","p <= .05",
                       "SRMR < .08","GFI > .90","CFI > .90","TLI > .90","AIC lower","BIC lower",
                       "chisq.robust","df.robust","p.robust","Satorra-Bentler correction",
                       "CFI.robust","TLI.robust","RMSEA.robust","RMSEA.ci.lower.robust",
                       "RMSEA.ci.upper.robust","RMSEA.p.robust(blank=NA)","SRMR_bentler","SRMR_Mplus")
    modelfitdata <- cbind(criterian = criteria_data, Value = round(fitdata,3))
  }

  fitMeasures_s1 <- modelfitdata %>%
    row2col("index") %>%
    unite_rows(5,6) %>% move_row(6,5) %>%
    replace_df_rep("rmsea.ci.lower","90%CI.L", "rmsea.ci.upper","90%CI.U") %>%
    knitr::kable(format=format, caption = "01 Model fit information")

  # 02. Factor loadings (표/그림은 단일지표도 표시)
  factorloading_0 <- lavaan::parameterEstimates(x, standardized=TRUE) %>%
    dplyr::filter(op=="=~") %>%
    mutate(stars = ifelse(pvalue < .001,"***",
                          ifelse(pvalue < .01,"**",
                                 ifelse(pvalue < .05,"*","")))) %>%
    mutate(label = ifelse(std.all > 0.7, "Good",
                          ifelse(std.all > 0.5, "fair", "No")),
           est = round(est,digits),
           z = round(z,digits),
           se = round(se,digits),
           std.all = round(std.all,digits),
           pvalue = ifelse(pvalue < .001, "< .001", pvalue)) %>%
    all_na_zero(imp="") %>%
    dplyr::select(Latent=lhs, Item=rhs, Est=est, SE=se, std=std.all, Z=z, `Sig.`=stars, p=pvalue, Accept=label) %>%
    Unite(6,7)

  if (isTRUE(rename)) {
    factorloading <- factorloading_0 %>%
      mutate(Indicator = var_name) %>%
      dplyr::select(Latent, Item, Indicator, Est, SE, std, Z, p, Accept) %>%
      knitr::kable(digits=3, format=format,
                   caption="02 Indicator Validity(1): Factor Loadings (Bagozzi & Yi, 1988)")
  } else {
    factorloading <- factorloading_0 %>%
      knitr::kable(digits=3, format=format,
                   caption="02 Indicator Validity(1): Factor Loadings (Bagozzi & Yi, 1988)")
  }

  # [NEW] ---------- build measurement map & singles ----------
  meas <- .meas_map(x)
  singles <- .singles(meas)   # character vector of latent names with k == 1

  # 03. Reliability / Convergent validity
  # Use semTools::reliability() to match raw Cronbach alpha,
  # omega3 (legacy CR definition), and avevar.
  lv_stats <- .lv_counts(meas) %>%
    dplyr::filter(k >= 2) %>%
    dplyr::pull(Latent)

  if (length(lv_stats) == 0L) {
    alpha_AVE_CR_0 <- data.frame(
      Cronbach = numeric(0), CR = numeric(0), AVE = numeric(0)
    )
  } else {
    rel <- suppressWarnings(semTools::reliability(x, return.total = FALSE))
    required_rows <- c("alpha", "omega3", "avevar")
    if (!is.matrix(rel) || !all(required_rows %in% rownames(rel))) {
      stop("Unexpected semTools::reliability() result: alpha/omega3/avevar missing")
    }
    missing_lv <- setdiff(lv_stats, colnames(rel))
    if (length(missing_lv)) {
      stop("Reliability output missing latent factors: ",
           paste(missing_lv, collapse = ", "))
    }
    alpha_AVE_CR_0 <- data.frame(
      Cronbach = as.numeric(rel["alpha", lv_stats]),
      CR = as.numeric(rel["omega3", lv_stats]),
      AVE = as.numeric(rel["avevar", lv_stats]),
      row.names = lv_stats
    )
  }

  cr_result <- .cfa2_cr_fallback(alpha_AVE_CR_0, meas)
  alpha_AVE_CR_0 <- cr_result$values
  cr_adjustments <- cr_result$adjustments
  cr_notes <- character()
  if (nrow(cr_adjustments)) {
    attr(alpha_AVE_CR_0, "CR_adjustments") <- cr_adjustments
    number <- function(value) formatC(value, format = "f", digits = digits)
    cr_notes <- vapply(seq_len(nrow(cr_adjustments)), function(i) {
      change <- cr_adjustments[i, ]
      if (is.finite(change$adjusted_CR)) {
        paste0("CR 계산 전환: ", change$Latent, " — 기존 omega3 = ",
          number(change$original_CR), " > 1이므로 단순 표준화 CR 공식 ",
          "[(Σλ)² / {(Σλ)² + Σ(1−λ²)}]으로 재계산했습니다. 적용 CR = ",
          number(change$adjusted_CR), ". 반올림 전 표준화 적재량을 사용하며 오차공분산은 반영하지 않습니다.")
      } else {
        paste0("CR 계산 전환 시도: ", change$Latent, " — 기존 omega3 = ",
          number(change$original_CR), " > 1. ", change$status, " CR은 NA로 표시합니다.")
      }
    }, character(1))
  }

  FL.1 <- alpha_AVE_CR_0 %>%
    mutate(alpha_Check = dplyr::case_when(
      is.na(Cronbach) ~ "NA",
      Cronbach > 0.7  ~ "(>0.7) *",
      Cronbach > 0.6  ~ "(poor) *",
      TRUE            ~ "Reject"
    ),
    CR_Check = dplyr::case_when(
      is.na(CR)       ~ "NA",
      !is.finite(CR) | CR < 0 | CR > 1 ~ "Invalid",
      CR > 0.7        ~ "(>0.7) *",
      TRUE            ~ "Reject"
    )) %>%
    dplyr::select(Cronbach, alpha_Check, CR, CR_Check) %>% as.data.frame()

  FL <- FL.1 %>% knitr::kable(digits=3, format=format,
                              caption="03-1. Internal consistency / Composite Reliability")

  alpha_AVE_CR <- alpha_AVE_CR_0 %>%
    knitr::kable(digits=3, format=format,
                 caption="03 Convergent validity (Fornell & Larcker, 1981): AVE(>0.5), CR(>0.7)")

  # 04. Fornell–Larcker (단일지표 제외)
  AVE_df <- alpha_AVE_CR_0 %>% dplyr::select(AVE)
  sqrt.AVE <- sqrt(AVE_df); colnames(sqrt.AVE) <- "sqrt.AVE"

  lv.cor <- lavaan::lavInspect(x, what="cor.lv")
  if (is.list(lv.cor)) lv.cor <- lv.cor[[1]]
  # drop singles
  lv.cor <- .drop_latents_square(lv.cor, singles)
  sqrt.AVE <- .drop_latents_square(as.matrix(sqrt.AVE), singles)

  # Fornell-Larcker: compare sqrt(AVE) against the maximum
  # absolute correlation with any OTHER latent variable.
  if (length(lv_stats) > 0L) {
    lv.cor <- lv.cor[lv_stats, lv_stats, drop = FALSE]
  }
  lv.cor1 <- lv.cor
  diag(lv.cor1) <- 0
  rho1 <- as.data.frame(lv.cor1)
  rho1$max <- if (nrow(lv.cor1) > 1L) {
    apply(abs(lv.cor1), 1, max)
  } else {
    rep(NA_real_, nrow(lv.cor1))
  }
  rho1$lv <- rownames(rho1)
  sqrt.AVE <- data.frame(
    lv = lv_stats,
    sqrt.AVE = sqrt(alpha_AVE_CR_0[lv_stats, "AVE"]),
    row.names = NULL
  )
  diff <- merge(rho1, sqrt.AVE, by = "lv", all.x = TRUE, sort = FALSE)
  diff$delta <- diff$sqrt.AVE - diff$max
  diff$sig <- ifelse(is.na(diff$delta), "NA",
                     ifelse(diff$delta > 0, "*", "ns"))
  FornellNacker <- diff[, setdiff(names(diff), "delta"), drop = FALSE]

  validity <- FornellNacker %>% cut_print() %>%
    knitr::kable(digits=3, format=format,
                 caption="04 Discriminant Validity: rho < sqrt(AVE) (Fornell & Larcker, 1981)")

  # Exclude single-item factors BEFORE lav_htmt computes monotrait correlations.
  htmt_blocks <- lapply(unique(meas$Latent), function(lv) unique(meas$Item[meas$Latent == lv]))
  names(htmt_blocks) <- unique(meas$Latent)
  htmt_blocks <- htmt_blocks[lengths(htmt_blocks) >= 2L]
  htmt_note <- if (length(singles)) {
    paste0("Single-item factors excluded: ", paste(singles, collapse = ", "), ".")
  } else ""
  htmt_error <- NULL
  htmt2_mat <- if (length(htmt_blocks) >= 2L) {
    tryCatch({
      lav_htmt(data = lav_return_data(x), blocks = htmt_blocks,
               cut = htmt_cut, htmt2 = htmt2, digits = digits)
    }, error = function(e) {
      htmt_error <<- conditionMessage(e)
      NULL
    })
  } else NULL

  if (!is.null(htmt2_mat)) {
    htmt <- knitr::kable(htmt2_mat, format = format, digits = digits,
      caption = paste("HTMT (Henseler, Ringle & Sarstedt, 2015) — threshold < 0.90 (robust: 0.85)", htmt_note))
  } else if (length(htmt_blocks) < 2L) {
    htmt <- paste("HTMT requires at least two multi-item factors.", htmt_note)
  } else {
    htmt <- paste("HTMT not computed:", htmt_error, htmt_note)
  }

  # 06. latent 경로/공분산 유의성
  lv.cor.sig0 <- lavaan::parameterEstimates(x, standardized = TRUE) %>%
    dplyr::filter(op=="~" | (op=="~~" & lhs != rhs)) %>%
    dplyr::select(lhs, op, rhs, std.lv, pvalue) %>%
    mutate(sig = ifelse(pvalue < .001,"***",
                        ifelse(pvalue < .01,"**",
                               ifelse(pvalue < .05,"*","Not Sig"))),
           op = ifelse(op=="~","<--", ifelse(op=="~~","cor",""))) %>%
    dplyr::select(lhs, op, rhs, std.lv, pvalue, sig)
  lv.cor.sig <- lv.cor.sig0 %>%
    knitr::kable(digits=3, format=format, caption="05 latent correlation / regression significance")

  # 07. item × (CR, AVE) 합표 (단일지표는 이미 제외됨)
  add_table <- full_join(
    factorloading_0 %>% dplyr::select(-Accept, -p),
    alpha_AVE_CR_0 %>% row2col("Latent") %>% dplyr::select(-Cronbach) %>% Round(3),
    by = "Latent"
  ) %>% nice_table()

  model <- lav_return_model(x)

  all.reuslt <- list(
    model = model,
    fit_criterian = fit,
    model_fit = fitMeasures_s1,
    model_fit_paper = modelfit_paper(
      modelfitdata,
      format = format
    ), # cfa2_modelfit_paperfunction generation
    factorloadings = factorloading,
    Internal_Consistency = FL,
    Convergent = alpha_AVE_CR,
    Discriminant = validity,
    Discriminant_HTMT = htmt,
    betaMat_sig = lv.cor.sig,
    loadings_Bar = gg,
    item_CR_AVE = add_table %>% kable(format="markdown", digits=3),
    variable_order = varnames_check
  )

  raw <- list(
    fit = fitMeasures,
    model_fit = modelfitdata,
    # model_fit_paper = modelfit_paper(fitMeasures_s1),
    factorloading = factorloading_0,
    convergent = alpha_AVE_CR_0,
    discriminant = FornellNacker,
    htmt = htmt2_mat,
    bar = gg,
    lv.cor.sig = lv.cor.sig0
  )

  if (nrow(cr_adjustments)) {
    attr(all.reuslt, "CR_adjustments") <- cr_adjustments
    attr(raw, "CR_adjustments") <- cr_adjustments
  }

  if (isTRUE(web)) {
    web_loadings <- factorloading_0
    if (isTRUE(rename)) {
      web_loadings$Indicator <- var_name
      web_loadings <- web_loadings[, c("Latent", "Item", "Indicator", "Est",
                                      "SE", "std", "Z", "p", "Accept")]
    }
    report_tables <- list(
      "01 Fit criteria" = fitMeasures,
      "02 Model fit" = modelfitdata,
      "03 Model fit for paper" = modelfit_paper(modelfitdata, format = "raw"),
      "04 Factor loadings" = web_loadings,
      "05 Internal consistency" = FL.1,
      "06 Convergent validity (CR / AVE)" = alpha_AVE_CR_0,
      "07 Discriminant validity (Fornell-Larcker)" = FornellNacker,
      "08 Discriminant validity (HTMT)" = htmt2_mat,
      "09 Latent regression / covariance significance" = lv.cor.sig0,
      "10 Item loadings / CR / AVE" = add_table
    )
    if (nzchar(htmt_note)) names(report_tables)[8] <- paste(names(report_tables)[8], "—", htmt_note)
    fit_label <- paste(deparse(substitute(x)), collapse = " ")
    diagram <- lapply(c("est", "std"), function(label_type) {
      .cfa2_path_image(
        x, path_args = utils::modifyList(path_args, list(whatLabels = label_type)),
        fit_label = fit_label,
        item_labels = if (isTRUE(rename)) stats::setNames(var_name, varnames_check) else NULL
      )
    })
    names(diagram) <- c("unstandardized", "standardized")
    table_notes <- stats::setNames(rep(list(cr_notes), 3L), names(report_tables)[c(5, 6, 10)])
    report_file <- .cfa2_web_report(report_tables, model, gg,
                                    varnames_check, digits = digits, diagram = diagram,
                                    table_notes = table_notes)
    attr(all.reuslt, "web_file") <- report_file
    utils::browseURL(report_file)
    return(invisible(all.reuslt))
  }

  switch(type,
         all = all.reuslt,
         raw = raw,
         data.frame = raw,
         model = model,
         modelfit = modelfitdata,
         modelfit_paper = modelfit_paper(modelfitdata,
           format = "raw"),
         modelfit2 = fitMeasures_s1,
         loadings = factorloading_0,
         item = factorloading_0,
         indicator = factorloading_0,
         loadings_bar = gg,
         alpha = FL.1,
         CR_AVE = alpha_AVE_CR_0,
         item_CR_AVE = add_table,
         item_CR_AVE_web = add_table %>% web(),
         Convergent = alpha_AVE_CR_0,
         fl_criteria = FornellNacker,
         Discriminant = FornellNacker,
         htmt = htmt2_mat,
         HTMT = htmt2_mat,
         str_cor = lv.cor.sig0
  )
}



# Replace only CR estimates above one, using full-precision standardized loadings.
.cfa2_cr_fallback <- function(values, measurement) {
  changes <- data.frame(Latent = character(), original_CR = numeric(),
                        adjusted_CR = numeric(), method = character(), status = character())
  rows <- which(!is.na(values$CR) & values$CR > 1)
  for (row in rows) {
    latent <- rownames(values)[row]
    lambda <- measurement$lambda[measurement$Latent == latent]
    valid <- length(lambda) >= 2L && all(is.finite(lambda)) && all(abs(lambda) <= 1)
    denominator <- if (valid) sum(lambda)^2 + sum(1 - lambda^2) else NA_real_
    replacement <- if (is.finite(denominator) && denominator > 0) {
      sum(lambda)^2 / denominator
    } else NA_real_
    if (!is.finite(replacement) || replacement < 0 || replacement > 1) replacement <- NA_real_
    changes <- rbind(changes, data.frame(
      Latent = latent, original_CR = values$CR[row], adjusted_CR = replacement,
      method = "standardized_loadings_CR",
      status = if (is.finite(replacement)) "converted" else
        "표준화 적재량의 결측·1 초과 또는 분모 문제로 유효한 대체값을 계산할 수 없습니다."
    ))
    values$CR[row] <- replacement
  }
  list(values = values, adjustments = changes)
}

# Render semPaths2 on a dedicated device, preserving the caller's graphics device.
.cfa2_path_image <- function(x, path_args = list(), fit_label = "fit", item_labels = NULL) {
  sem_paths <- get0("semPaths2", mode = "function", inherits = TRUE)
  if (is.null(sem_paths) && requireNamespace("jjstat", quietly = TRUE)) {
    sem_paths <- getExportedValue("jjstat", "semPaths2")
  }
  if (is.null(sem_paths)) stop("semPaths2() is required for the model diagram.")
  allowed <- setdiff(names(formals(sem_paths)), "lav_obj")
  unknown <- setdiff(names(path_args), allowed)
  if (length(unknown)) stop("Unknown semPaths2 argument(s): ", paste(unknown, collapse = ", "))
  n_items <- length(lavaan::lavNames(x, "ov"))
  n_latent <- length(lavaan::lavNames(x, "lv"))
  width <- min(18, max(11, 10 + n_latent * .35))
  height <- min(26, max(7, n_items * .38 + n_latent * .35))
  args <- list(sizeMan = 5, sizeMan2 = 2, sizeLat = 6, sizeLat2 = 3,
               curve = 3, rotation = 2, layout = "tree",
               mar = c(3, 12, 3, 6), edge.label.cex = .85,
               edge.label.position = .6, shapeLat = "ellipse")
  # Use full names instead of semPaths2's default ten-character abbreviations.
  labels <- semPlot::semPlotModel(x)@Vars$name
  if (!is.null(item_labels)) {
    index <- match(labels, names(item_labels))
    labels[!is.na(index)] <- as.character(item_labels[index[!is.na(index)]])
  }
  args$nodeLabels <- labels
  args <- utils::modifyList(args, path_args, keep.null = TRUE)
  code_args <- vapply(names(args), function(name) {
    paste0("  ", name, " = ", paste(deparse(args[[name]], width.cutoff = 80), collapse = "\n"))
  }, character(1))
  code <- paste0("semPaths2(\n  ", fit_label, ",\n", paste(code_args, collapse = ",\n"), "\n)")
  png_file <- tempfile("cfa2-model-", fileext = ".png")
  on.exit(unlink(png_file), add = TRUE)
  render <- function() {
    grDevices::png(png_file, width = width, height = height, units = "in", res = 150, bg = "white")
    device <- grDevices::dev.cur()
    on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE)
    do.call(sem_paths, c(list(lav_obj = x), args))
    grDevices::dev.off(device)
    knitr::image_uri(png_file)
  }
  # Keep the remaining report available when a particular model cannot be drawn.
  result <- tryCatch(list(uri = render(), error = NULL),
                     error = function(e) list(uri = NULL, error = conditionMessage(e)))
  c(result, list(code = code, width = width, height = height, args = args))
}

# Internal standalone report renderer; kept here so source("R/cfa2.R") is enough.
.cfa2_web_report <- function(tables, model, plot, variable_order, digits = 3,
                              file = tempfile("cfa2-report-", fileext = ".html"),
                              diagram = NULL, table_notes = list()) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Install htmltools to use cfa2(web = TRUE).")
  }
  esc <- function(x) as.character(htmltools::htmlEscape(enc2utf8(as.character(x))))
  code_block <- function(x) paste0("<pre><code>", esc(x), "</code></pre>")
  # A fenced block longer than any backtick sequence in user-supplied syntax.
  md_code <- function(x) {
    runs <- regmatches(x, gregexpr("`+", x))[[1]]
    fence <- paste(rep("`", max(c(2L, nchar(runs))) + 1L), collapse = "")
    paste(fence, x, fence, sep = "\n")
  }
  model_text <- paste(as.character(model), collapse = "\n")
  code_card <- function(title, text, id) paste0(
    '<section class="code-card"><div class="table-heading"><h2>', esc(title), '</h2>',
    '<div class="table-actions"><button type="button" class="copy-code" data-copy-target="',
    id, '">코드 복사</button><span class="copy-status" role="status" aria-live="polite"></span>',
    '</div></div><pre><code id="', id, '">', esc(text), '</code></pre></section>')
  image_card <- function(title, uri, filename, alt) paste0(
    '<section class="image-card"><div class="table-heading"><h2>', esc(title), '</h2>',
    '<a class="download-image" href="', uri, '" download="', filename, '">PNG 다운로드</a>',
    '</div><img class="report-plot" alt="', esc(alt), '" src="', uri, '"></section>')
  sections <- code_card("Model", model_text, "model-code")
  markdown <- c("# CFA / SEM Results", "", "## Model", "", md_code(model_text))
  for (i in seq_along(tables)) {
    title <- names(tables)[i]
    data <- tables[[i]]
    if (is.null(data)) {
      sections <- c(sections, paste0("<section><h2>", esc(title),
                    "</h2><p>Not computed / unavailable.</p></section>"))
      markdown <- c(markdown, "", paste0("## ", title), "", "Not computed / unavailable.")
      next
    }
    # knitr escapes cell HTML. Markdown needs explicit escaping of pipes and newlines.
    md_data <- as.data.frame(data, stringsAsFactors = FALSE)
    md_escape <- function(x) {
      x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
      x <- gsub("<", "&lt;", x, fixed = TRUE)
      x <- gsub(">", "&gt;", x, fixed = TRUE)
      x <- gsub("|", "&#124;", x, fixed = TRUE)
      gsub("[\r\n]+", "<br>", x)
    }
    for (col in names(md_data)) {
      if (!is.numeric(md_data[[col]])) md_data[[col]] <- md_escape(md_data[[col]])
    }
    names(md_data) <- md_escape(names(md_data))
    rownames(md_data) <- md_escape(rownames(md_data))
    html_table <- paste(knitr::kable(data, format = "html", digits = digits,
                                     escape = TRUE), collapse = "\n")
    md_table <- paste(knitr::kable(md_data, format = "pipe", digits = digits), collapse = "\n")
    notes <- table_notes[[title]]
    note_html <- if (length(notes)) paste0('<div class="table-note" role="note">',
                         paste(esc(notes), collapse = "<br>"), '</div>') else ""
    sections <- c(sections, paste0(
      '<section class="table-card" aria-labelledby="table-title-', i, '">',
      '<div class="table-heading"><h2 id="table-title-', i, '">', esc(title), '</h2>',
      '<div class="table-actions"><button type="button" class="copy-table" aria-label="',
      esc(paste(title, "표 복사")), '">표 복사</button>',
      '<span class="copy-status" role="status" aria-live="polite"></span></div></div>',
      '<div class="table-scroll" tabindex="0">', html_table, '</div>', note_html, '</section>'))
    markdown <- c(markdown, "", paste0("## ", title), "", md_table)
    if (length(notes)) markdown <- c(markdown, "", paste0("> ", notes))
  }
  png_file <- tempfile("cfa2-loadings-", fileext = ".png")
  on.exit(unlink(png_file), add = TRUE)
  ggplot2::ggsave(png_file, plot = plot, width = max(9, length(unique(variable_order)) * .55),
                 height = 5.5, dpi = 150, bg = "white", limitsize = FALSE)
  image_uri <- knitr::image_uri(png_file)
  sections <- c(sections, image_card("Factor loadings", image_uri,
                                      "cfa2-factor-loadings.png", "Standardized factor loadings"),
                  paste0('<section><h2>Variable order</h2>',
                         code_block(paste(variable_order, collapse = ", ")), '</section>'))
  markdown <- c(markdown, "", "## Factor loadings", "",
                paste0("![Standardized factor loadings](", image_uri, ")"),
                "", "## Variable order", "", md_code(paste(variable_order, collapse = ", ")))
  # Keep both model diagrams at the end of HTML and Markdown reports.
  if (!is.null(diagram)) {
    if (!is.null(diagram$code)) diagram <- list(unstandardized = diagram)
    for (mode in names(diagram)) {
      current <- diagram[[mode]]
      standardized <- identical(mode, "standardized")
      title <- if (standardized) "Model diagram — Standardized (표준화 계수)" else
                                "Model diagram — Unstandardized (비표준화 계수)"
      alt <- if (standardized) "SEM path diagram with standardized coefficients" else
                              "SEM path diagram with unstandardized coefficients"
      filename <- paste0("cfa2-model-", mode, ".png")
      if (!is.null(current$uri)) {
        sections <- c(sections, image_card(title, current$uri, filename, alt))
        markdown <- c(markdown, "", paste0("## ", title), "",
                      paste0("![", alt, "](", current$uri, ")"))
      } else {
        sections <- c(sections, paste0('<section><h2>', esc(title), '</h2>',
                       '<p>모델 도형을 그릴 수 없습니다: ', esc(current$error), '</p></section>'))
        markdown <- c(markdown, "", paste0("## ", title), "", current$error)
      }
      sections <- c(sections, paste0('<details class="diagram-code"><summary>',
                      esc(paste(title, "R 코드")), '</summary>',
                      code_card("semPaths2 R code", current$code, paste0("diagram-code-", mode)), '</details>'))
      markdown <- c(markdown, "", "### semPaths2 R code", "", md_code(current$code))
    }
  }
  css <- r"---(
:root{font-family:system-ui,-apple-system,'Segoe UI',sans-serif;color:#1e293b;background:#f1f5f9}
*{box-sizing:border-box}body{margin:0}header{position:sticky;top:0;z-index:5;background:#fff;border-bottom:1px solid #dbe2ec;padding:18px max(24px,calc((100% - 1200px)/2));display:flex;gap:16px;align-items:center;justify-content:space-between;flex-wrap:wrap}
h1{font-size:23px;margin:0 0 5px}header p{margin:0;color:#64748b;font-size:14px}main{max-width:1248px;margin:auto;padding:24px}section{background:#fff;border:1px solid #dbe2ec;border-radius:12px;padding:22px;margin-bottom:20px;min-width:0}
h2{font-size:18px;margin:0 0 16px}.downloads,.table-actions{display:flex;align-items:center;gap:10px;flex-wrap:wrap}button,.download-image{font:inherit;text-decoration:none;display:inline-block;cursor:pointer;border:1px solid #cbd5e1;border-radius:7px;padding:9px 14px;background:#fff;color:#1e293b}button:hover,.download-image:hover{background:#eff6ff;border-color:#2563eb}button:focus-visible,.download-image:focus-visible,[tabindex]:focus-visible{outline:3px solid #93c5fd;outline-offset:3px}#download-html{background:#1d4ed8;border-color:#1d4ed8;color:#fff}
.table-heading{display:flex;justify-content:space-between;gap:16px;align-items:baseline;flex-wrap:wrap}.table-actions{min-height:40px}.copy-table{opacity:0;transition:opacity .15s}.table-card:hover .copy-table,.table-card:focus-within .copy-table{opacity:1}.copy-status{font-size:13px;color:#1d4ed8}.table-note{margin-top:14px;padding:12px 14px;border-left:4px solid #f59e0b;background:#fffbeb;font-size:13px;line-height:1.7;overflow-wrap:anywhere}.table-scroll{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:14px}th,td{padding:10px 12px;border-bottom:1px solid #e2e8f0;white-space:nowrap}thead{background:#f8fafc}th{font-weight:600}tbody tr:hover{background:#f8fafc}pre{padding:16px;background:#f8fafc;white-space:pre-wrap;overflow-wrap:anywhere;border-radius:7px;font-size:14px;line-height:1.6}.report-plot{display:block;width:100%;height:auto}.image-card .table-heading{margin-bottom:16px}.code-card pre{max-height:360px;overflow:auto}.diagram-code{margin-bottom:20px}.diagram-code summary{cursor:pointer;padding:12px;font-weight:600}.diagram-code section{margin-top:8px}footer{padding:0 24px 24px;text-align:center;color:#64748b;font-size:13px}[hidden]{display:none!important}
@media(hover:none){.copy-table{opacity:1}}@media(max-width:600px){main{padding:12px}section{padding:14px}header{padding:14px}.table-heading h2{width:100%;margin-bottom:0}}
@media print{header{position:static}.downloads,.table-actions,.download-image,footer{display:none}main{padding:0}section{border:0;break-inside:avoid}.table-scroll{overflow:visible}table{font-size:10px}th,td{padding:5px;white-space:normal}}
)---"
  js <- r"---(
(() => {
  'use strict';
  function download(name, content, type) {
    const url = URL.createObjectURL(new Blob([content], {type}));
    const link = document.createElement('a');
    link.href = url; link.download = name; document.body.appendChild(link);
    link.click(); link.remove(); setTimeout(() => URL.revokeObjectURL(url), 1000);
  }
  document.getElementById('download-html').addEventListener('click', () => {
    download('cfa2-results.html', '<!doctype html>\n' + document.documentElement.outerHTML, 'text/html;charset=utf-8');
  });
  document.getElementById('download-md').addEventListener('click', () => {
    download('cfa2-results.md', document.getElementById('markdown-source').value, 'text/markdown;charset=utf-8');
  });
  function fallbackCopy(table, text, html) {
    const selection = window.getSelection();
    const saved = [];
    for (let i = 0; i < selection.rangeCount; i++) saved.push(selection.getRangeAt(i).cloneRange());
    const range = document.createRange(); range.selectNodeContents(table);
    const copy = event => {
      if (event.clipboardData) {
        event.clipboardData.setData('text/plain', text);
        if (html) event.clipboardData.setData('text/html', html); event.preventDefault();
      }
    };
    document.addEventListener('copy', copy);
    selection.removeAllRanges(); selection.addRange(range);
    try { if (!document.execCommand('copy')) throw new Error('Copy unavailable'); }
    finally {
      document.removeEventListener('copy', copy); selection.removeAllRanges();
      saved.forEach(item => selection.addRange(item));
    }
  }
  document.querySelectorAll('.copy-code').forEach(button => {
    button.addEventListener('click', async () => {
      const code = document.getElementById(button.getAttribute('data-copy-target'));
      const status = button.closest('.code-card').querySelector('.copy-status');
      const text = code.textContent;
      try {
        let copied = false;
        if (navigator.clipboard && navigator.clipboard.writeText) {
          try { await navigator.clipboard.writeText(text); copied = true; } catch (_) {}
        }
        if (!copied) fallbackCopy(code, text, null);
        status.textContent = '코드 복사 완료';
      } catch (_) { status.textContent = '코드를 복사할 수 없습니다. 코드를 선택해 복사해 주세요.'; }
    });
  });
  document.querySelectorAll('.copy-table').forEach(button => {
    button.addEventListener('click', async () => {
      const card = button.closest('.table-card'); const table = card.querySelector('table');
      const status = card.querySelector('.copy-status');
      const text = Array.from(table.rows, row => Array.from(row.cells, cell =>
        cell.innerText.replace(/[\t\r\n]+/g, ' ').trim()).join('\t')).join('\n');
      const html = '<html><head><meta charset="utf-8"></head><body>' + table.outerHTML + '</body></html>';
      try {
        let copied = false;
        if (navigator.clipboard && navigator.clipboard.write && window.ClipboardItem) {
          try {
            await navigator.clipboard.write([new ClipboardItem({
              'text/html': new Blob([html], {type:'text/html'}),
              'text/plain': new Blob([text], {type:'text/plain'})
            })]); copied = true;
          } catch (_) { /* Local-file browsers may require the selection fallback. */ }
        }
        if (!copied) fallbackCopy(table, text, html);
        status.textContent = '복사 완료';
      } catch (_) { status.textContent = '복사할 수 없습니다. 표를 선택해 복사해 주세요.'; }
    });
  });
})();
)---"
  html <- paste0('<!doctype html>\n<html lang="ko"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<title>CFA / SEM Results</title><style>', css, '</style></head><body>',
    '<header><div><h1>CFA / SEM Results</h1><p>표에 마우스를 올리면 복사 버튼이 나타납니다.</p></div>',
    '<div class="downloads"><button type="button" id="download-html">HTML 다운로드</button>',
    '<button type="button" id="download-md">Markdown (.md) 다운로드</button></div></header><main>',
    paste(sections, collapse = "\n"), '</main>',
    '<footer>jjstat · CFA / SEM report</footer>',
    '<textarea id="markdown-source" hidden aria-hidden="true">',
    esc(paste(markdown, collapse = "\n")), '</textarea>',
    '<script>', js, '</script></body></html>')
  writeLines(enc2utf8(html), file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}


# #cfa2 CFA lavaan SEM----
# cfa2 <- function(x,
#                  format="markdown",
#                  htmt_cut = 0.9,
#                  htmt2=FALSE,
#                  cut=0.7,
#                  angle=90,
#                  cex=11, hjust=0.9,
#                  val.size=4,
#                  dis.sort=TRUE,
#                  rename=F,
#                  var_name=NA,
#                  digits=3,
#                  type = "all"){
#
#   library(dplyr)
#   library(knitr)
#   library(lavaan)
#   library(semTools)
#   library(tibble)
#   library(semPlot)
#   library(ggplot2)
#   library(kableExtra)
#   library(tidyverse)
#
#   # tryCatch({
#
#   # 01 fit table-----
#   options(scipen = 100)
#
#   fit.indices=c("chisq","pvalue", "df","rmsea",
#                 "gfi","agfi","srmr","cfi","tli","nfi","aic","bic")
#   fitMeasures <- round(fitMeasures(x,fit.indices),3)
#   fitMeasures_s <- round(fitMeasures(x,fit.indices),3)
#
#   fitMeasures <- as.data.frame(fitMeasures) #check.names = TRUE
#   fitMeasures$critera <- c("",
#                            "*p.value >= 0.05",
#                            "_chisq/df <= 3(<5(ok)",
#                            "*RMSEA< 0.05(or 0.08)",
#                            "*GFI >= 0.95",
#                            "_AGFI>= 0.90",
#                            "*SRMR < 0.08",
#                            "*CFI >= 0.95",
#                            "_TLI >= 0.90",
#                            "_NFI >= 0.90",
#                            "_lower",
#                            "_lower")
#   fitMeasures$Ref <-c("-",
#                       "-",
#                       "Wheaton et al.(1977)",
#                       "Browne & Cudek(1993)",
#                       "Joreskog-Sorbom(1970)",
#                       "Tanaka & Huba(1985)",
#                       "Hu & Bentler(1999)",
#                       "Kline(2011)",
#                       "Bentler & Bonett(1980)",
#                       "Bollen(1989)",
#                       "Akaike(1973)",
#                       "-")
#   fitMeasures$chiq_df <- c("","",
#                            round(fitMeasures[1,1]/fitMeasures[3,1],2),
#                            "","","","","","","","","")
#
#   fit <- fitMeasures  %>%
#     knitr::kable(digits=3, format=format,
#                  caption="FitMeasure and criterian
#           (*)satisfy By kline(2011) Suggestion")
#
#   if(length(fitMeasures(x)) == length(fitMeasures(x))) {
#     fitdata_00 <- fitMeasures(x,c("chisq","df","pvalue",
#                                   "rmsea",
#                                   "rmsea.ci.lower",
#                                   "rmsea.ci.upper",
#                                   "rmsea.pvalue",
#                                   "srmr",
#                                   "gfi",
#                                   "cfi",
#                                   "tli",
#                                   "aic",
#                                   "bic"
#     ))
#
#     criteria_data_00 = c("Chisq",
#                          "df",
#                          "p > .05",
#                          "RMSEA < .05",
#                          "90%CI.L",
#                          "90%CI.U",
#                          "p <= .05",
#                          "SRMR < .08",
#                          "GFI > .90",
#                          "CFI > .90",
#                          "TLI > .90",
#                          "AIC lower ",
#                          "BIC lower "
#     )
#
#     modelfitdata <-cbind.data.frame("criterian" = criteria_data_00,
#                                     "Value" = round(fitdata_00,3))
#
#   } else {
#     fitdata <- fitMeasures(x,c("chisq","df","pvalue",
#                                "rmsea",
#                                "rmsea.ci.lower",
#                                "rmsea.ci.upper",
#                                "rmsea.pvalue",
#                                "srmr",
#                                "gfi",
#                                "cfi",
#                                "tli",
#                                "aic",
#                                "bic",
#                                "chisq.scaled", #roburst chisq
#                                "df.scaled",
#                                "pvalue.scaled",
#                                "chisq.scaling.factor",
#                                "cfi.robust",   # add
#                                "tli.robust",
#                                "rmsea.robust",
#                                "rmsea.ci.lower.robust",
#                                "rmsea.ci.upper.robust",
#                                "rmsea.pvalue.robust",
#                                "srmr_bentler",
#                                "srmr_mplus"
#     ))
#
#     criteria_data = c("Chisq",
#                       "df",
#                       "p > .05",
#                       "RMSEA < .05",
#                       "90%CI.L",
#                       "90%CI.U",
#                       "p <= .05",
#                       "SRMR < .08",
#                       "GFI > .90",
#                       "CFI > .90",
#                       "TLI > .90",
#                       "AIC lower ",
#                       "BIC lower ",
#                       "chisq.robust", #roburst chisq
#                       "df.robust",
#                       "p.robust",
#                       "Satorra-Bentler correction",
#                       "CFI.robust",   # add
#                       "TLI.robust",
#                       "RMSEA.robust",
#                       "RMSEA.ci.lower.robust",
#                       "RMSEA.ci.upper.robust",
#                       "RMASE.p.robust(blank=NA)",
#                       "SRMR_bentler",
#                       "SRMR_Mplus"
#     )
#
#     modelfitdata <-cbind("criterian"=criteria_data,
#                          "Value"=round(fitdata,3))
#
#   }
#
#   fitMeasures_s1 <- modelfitdata %>%
#     row2col("index")%>%
#     unite_rows(5, 6) %>% #unite ci
#     move_row(6,5) %>%
#     replace_df_rep("rmsea.ci.lower","90%CI.L",
#                    "rmsea.ci.upper", "90%CI.U") %>%
#     knitr::kable(format=format,
#                  caption = "01 Model fit information")
#
#   # fitMeasures_s1 <- modelfitdata %>%
#   #   knitr::kable(format=format,
#   #          caption = "01 Model fit information")
#
#   options(knitr.kable.NA="")
#
#   factorloading_0 <- lavaan::parameterEstimates(x, standardized=TRUE) %>%
#     dplyr::filter(op=="=~") %>%
#     mutate(stars=ifelse(pvalue < 0.001, "***",
#                         ifelse(pvalue < 0.01, "**",
#                                ifelse(pvalue < 0.05, "*", "")))) %>%
#     mutate(label=ifelse(std.all>0.7,"Good",
#                         ifelse(std.all>0.5,"fair","No")),
#            est = round(est,digits),
#            z = round(z,digits),
#            se = round(se,digits),
#            std.all = round(std.all,digits),
#            pvalue = ifelse(pvalue<0.001,"< .001",pvalue)
#     ) %>%
#     all_na_zero(imp="")%>%
#     dplyr::select("Latent"=lhs, Item=rhs,
#                   Est=est,
#                   SE=se,
#                   std=std.all,
#                   Z=z,
#                   Sig.=stars,
#                   "p"= pvalue,
#                   Accept=label)%>%
#     Unite(6,7)
#
#
#
#   if(rename == TRUE) {
#     factorloading <- factorloading_0 %>% mutate(Indicator= var_name) %>%
#       dplyr::select(Latent, Item, Indicator, Est, SE,std, Z, p,  Accept) %>%
#       knitr::kable(digits=3, format=format,
#                    caption="02 Indicator Validity(1)
#           Factor Loadings:
#           (1) cr(critical ratio = Estimate/S.E) p<0.05,
#           (2) std.damda >= 0.5(Bagozzi & Yi(1988)")
#   } else {
#     factorloading <- factorloading_0 %>%
#       knitr::kable(digits=3, format=format,
#                    caption="02 Indicator Validity(1)
#           Factor Loadings:
#           (1) cr(critical ratio = Estimate/S.E) p<0.05,
#           (2) std.damda >= 0.5(Bagozzi & Yi(1988)")
#   }
#
#   dataplot0 <-  lavaan::parameterEstimates(x, standardized=TRUE) %>%
#     dplyr::filter(op=="=~") %>%
#     mutate(stars=ifelse(pvalue < 0.001, "***",
#                         ifelse(pvalue < 0.01, "**",
#                                ifelse(pvalue < 0.05, "*", "")))) %>%
#     mutate(label=ifelse(std.all>0.7,"Yes(Good)",
#                         ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
#     dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
#                   cr=z, Sig.=stars, "p"=pvalue,
#                   std=std.all, beta_Accept=label)
#
#   dataplot <- dataplot0 %>% select(Item,Latent, std)
#
#   varnames_check = dataplot0[,"Item"]
#
#   if(rename == TRUE) {
#     dataplot$Item <- var_name
#
#     gg <- ggplot(dataplot, aes(x=Item, y=std, fill=Latent)) +
#       geom_bar(stat="identity", position='dodge') +
#       geom_hline(yintercept = cut , color= "darkred", linetype = 2) +
#       geom_hline(yintercept = cut-0.2, color= "gray40") +
#       ggtitle("Factor loadings") +
#       theme_bw() +
#       geom_text(aes(label=round(std,2)), vjust=-.3, size=val.size) +
#       ylim(0, 1.1) +
#       theme(axis.text.x = element_text(angle=angle, size = cex, hjust = hjust, face="bold"))
#   } else {
#     gg <- ggplot(dataplot, aes(x=Item, y=std, fill=Latent)) +
#       geom_bar(stat="identity", position='dodge') +
#       geom_hline(yintercept = cut , color= "darkred", linetype=2) +
#       geom_hline(yintercept = cut-0.2, color= "gray40") +
#       ggtitle("Factor loadings") +
#       theme_bw() +
#       geom_text(aes(label=round(std,2)), vjust=-.3, size=val.size) +
#       ylim(0, 1.1) +
#       theme(axis.text.x = element_text(angle= angle, size = cex, hjust = hjust, face="bold"))
#   }
#
#
#
#
#
#   alpha.1 <- semTools::reliability(x,return.total = F) %>%
#     t() %>%
#     as.data.frame() %>%
#     dplyr::select("Cronbach"=alpha, "CR" = omega3) %>%
#     mutate(alpha_Check=ifelse(Cronbach>0.7,"(>0.7) *",
#                               ifelse(Cronbach>0.6,"(poor) *", "Reject"))) %>%
#     mutate(CR_Check=ifelse(CR>0.7,"(>0.7) *","Reject")) %>%
#     dplyr::select(Cronbach,alpha_Check,CR,CR_Check)
#
#   AVE <- semTools::reliability(x, return.total = F) %>%
#     t() %>%
#     as.data.frame() %>%
#     dplyr::select("AVE"=avevar)
#
#   sqrt.AVE <- sqrt(AVE)
#   colnames(sqrt.AVE) <- "sqrt.AVE"
#
#   rho <- lavaan::lavInspect(x,"std")$beta
#
#   ####
#   FL.1 <-cbind(alpha.1)
#   FL.1 =  FL.1   %>%
#     mutate(AVE=  AVE) %>%
#     mutate(AVE_check=ifelse(AVE>0.5,"(>0.5) *","Reject"))
#
#   FL <- FL.1 %>% knitr::kable(digits=3, format=format,
#                               caption="03-1. Internal consistency and Convergent
#           (Cronbach's Alpha, 1951) and Composite Relibility(CR)
#           AVE(Average Variance extracted)")
#
#
#   alpha_AVE_CR_0 <- semTools::reliability(x, return.total = FALSE) %>%
#     t() %>%
#     as.data.frame() %>%
#     dplyr::select("Cronbach"=alpha, "CR" = omega3, "AVE"=avevar) #%>%
#
#   # dplyr::select(Cronbach, CR, AVE, AVE_check)
#
#   alpha_AVE_CR <- alpha_AVE_CR_0 %>%
#     knitr::kable(digits = 3, format = format,
#                  caption = "03 Convergent validity
#           Internal consistency(Cronbach's Alpha, 1951)(>0.7)
#           AVE(>0.5) & CR(>0.7): Fornell & Lacker(1981)")
#
#
#
#
#
#   betaa <- lavaan::lavInspect(x, "std")$beta
#
#   if(is.null(betaa)) {
#     psi <- lavaan::lavInspect(x, "std")$psi
#     psi[lower.tri(psi)==FALSE] <- 0
#
#     rho1 <- psi %>% as.data.frame()
#     rho1$max <- apply(rho1,1,max)
#
#     rho1 <- rho1 %>% mutate(max = apply(rho1,1, max), lv = rownames(rho1))
#     sqrt.AVE$lv <- rownames(sqrt.AVE)
#
#     diff_0 <- merge(x = rho1[,c("max","lv")], y = sqrt.AVE, by = "lv", all = TRUE, sort = FALSE)
#     diff <- merge(x = rho1[,-(ncol(rho1)-1)], y = diff_0, by = "lv", all = TRUE, sort = FALSE)
#     diff$delta <- diff[,(ncol(diff))]- diff[,(ncol(diff)-1)]
#     diff$sig <- ifelse(diff$delta >= 0, "*", "ns")
#
#     FornellNacker <- diff[,c(-(ncol(diff)-1))]
#
#     validity <- FornellNacker %>% cut_print()%>%
#       knitr::kable(digits=3, format=format,
#                    caption="04 Discriminant Validity:
#           rho < Square Root of(AVE)
#            By Fornell & Lacker(1981)")
#
#   } else {
#     lv.cor <- lavaan::lavInspect(x, what="cor.lv")
#     lv.cor1 <- lv.cor
#     diag(lv.cor1) <- 0
#
#     rho1 <- lv.cor1 %>% as.data.frame()
#     rho1[lower.tri(rho1)==FALSE] <- 0
#     rho1$max <- apply(rho1,1, max)
#
#     rho1 <- rho1 %>% mutate(max=apply(rho1,1, max), lv =rownames(rho1))
#     sqrt.AVE$lv <- rownames(sqrt.AVE)
#
#     diff_0 <- merge(x = rho1[,c("max","lv")], y = sqrt.AVE, by = "lv", all = TRUE, sort = FALSE)
#     diff <- merge(x = rho1[,-(ncol(rho1)-1)], y = diff_0, by = "lv", all = TRUE, sort = FALSE)
#     diff$delta <- diff[,(ncol(diff))]- diff[,(ncol(diff)-1)]
#     diff$sig <- ifelse(diff$delta >= 0, "*", "ns")
#
#     FornellNacker <- diff[,c(-(ncol(diff)-1))]
#
#     validity <- FornellNacker%>%cut_print()%>%
#       knitr::kable(digits=3, format=format,
#                    caption="04 Discriminant Validity:
#           rho < Square Root of(AVE)
#            By Fornell & Lacker(1981)")
#   }
#
#
#   htmt2 <- lav_htmt(x, cut = htmt_cut, htmt2 = htmt2 ,  digits= digits)
#   # htmt2=NULL
#   htmt <- htmt2  %>%
#     knitr::kable(format=format, digits = digits,
#                  caption="The heterotrait-monotrait ratio of correlations (HTMT).
#           All correalation < 0.9 --> discriminant Accept(roburst:0.85)
#           general accept: < 1
#           (Henseler, Ringlet & Sarstedt, 2015)
#           ")
#
#   lv.cor.sig0 <- lavaan::parameterEstimates(x, standardized = T) %>%
#     dplyr::filter(op=="~"|op=="~~"&lhs != rhs) %>%
#     dplyr::select(lhs,op,rhs, std.lv, pvalue) %>%
#     mutate(sig=ifelse(pvalue < 0.001, "***",
#                       ifelse(pvalue < 0.01, "**",
#                              ifelse(pvalue < 0.05, "*", "Not Sig")))) %>%
#     mutate(op=ifelse(op=="~","<--",
#                      ifelse(op=="~~","cor",""))) %>%
#     dplyr::select(lhs,op,rhs, std.lv, pvalue,sig)
#
#   lv.cor.sig <- lv.cor.sig0 %>%
#     knitr::kable(digits=3, format=format,
#                  caption="05 latent correlation Significant Check")
#
#
#   # factorloading_0
#   # Convergent
#   add_table = full_join(
#     factorloading_0%>%dplyr::select(-Accept,-p),
#     alpha_AVE_CR_0%>%row2col("Latent")%>%
#       dplyr::select(-Cronbach)%>%Round(3),
#     by= "Latent")%>%nice_table()
#
#
#   #model return
#   model= lav_return_model(x)
#
#
#
#
#   all.reuslt <- list(
#     model = model,
#     fit_criterian = fit,
#     model_fit = fitMeasures_s1,
#     factorloadings = factorloading,
#     Internal_Consistency = FL,
#     Convergent = alpha_AVE_CR,
#     Discriminant = validity,
#     Discriminant_HTMT = htmt,
#     betaMat_sig = lv.cor.sig,
#     loadings_Bar = gg,
#     item_CR_AVE = add_table%>%kable(format="markdown", digits=3),
#     variable_order = varnames_check
#   )
#
#   raw = list(
#     fit = fitMeasures,
#     model_fit = modelfitdata,
#     factorloading = factorloading_0,
#     convergent = alpha_AVE_CR_0,
#     discriminant = FornellNacker,
#     htmt = htmt2,
#     bar = gg,
#     lv.cor.sig = lv.cor.sig0
#   )
#
#   switch(type,
#          all = all.reuslt,
#          raw = raw,
#          data.frame = raw,
#          model = model,
#          modelfit = modelfitdata,
#          modelfit2 = fitMeasures_s1,
#          loadings = factorloading_0,
#          item = factorloading_0,
#          indicator = factorloading_0,
#          loadings_bar = gg,
#          alpha = FL.1,
#          CR_AVE = alpha_AVE_CR_0,
#          item_CR_AVE =add_table,
#          item_CR_AVE_web =add_table%>%web(),
#          Convergent = alpha_AVE_CR_0,
#          fl_criteria = FornellNacker,
#          Discriminant = FornellNacker,
#          htmt = htmt2,
#          HTMT = htmt2,
#          str_cor = lv.cor.sig0
#   )
# }

# cfa2 <- function(x,
#                  format="markdown",
#                  # dataset=NA, #dataset input htmt
#                  # model=NA, # lavaan Model htmt(<0.9)
#                  htmt_cut = 0.9,
#                  htmt2=FALSE,
#                  cut=0.7,
#                  angle=90,
#                  cex=11, hjust=0.9,
#                  val.size=4,
#                  dis.sort=TRUE,
#                  rename=F,
#                  var_name=NA,
#                  digits=3,
#                  type = "all"){
#
#   library(dplyr)
#   library(knitr)
#   library(lavaan)
#   library(semTools)
#   library(tibble)
#   library(semPlot)
#   library(ggplot2)
#   library(kableExtra)
#   library(tidyverse)
#
#   # tryCatch({
#
#   # 01 fit table-----
#   options(scipen = 100)
#
#   fit.indices=c("chisq","pvalue", "df","rmsea",
#                 "gfi","agfi","srmr","cfi","tli","nfi","aic","bic")
#   fitMeasures <- round(fitMeasures(x,fit.indices),3)
#   fitMeasures_s <- round(fitMeasures(x,fit.indices),3)
#
#
#   # fitMeasures <- as.data.frame(fitMeasures(x,fit.indices))
#   fitMeasures <- as.data.frame(fitMeasures) #check.names = TRUE
#   fitMeasures$critera <- c("",
#                            "*p.value >= 0.05",
#                            "_chisq/df <= 3(<5(ok)",
#                            "*RMSEA< 0.05(or 0.08)",
#                            "*GFI >= 0.95",
#                            "_AGFI>= 0.90",
#                            "*SRMR < 0.08",
#                            "*CFI >= 0.95",
#                            "_TLI >= 0.90",
#                            "_NFI >= 0.90",
#                            "_lower",
#                            "_lower")
#   fitMeasures$Ref <-c("-",
#                       "-",
#                       "Wheaton et al.(1977)",
#                       "Browne & Cudek(1993)",
#                       "Joreskog-Sorbom(1970)",
#                       "Tanaka & Huba(1985)",
#                       "Hu & Bentler(1999)",
#                       "Kline(2011)",
#                       "Bentler & Bonett(1980)",
#                       "Bollen(1989)",
#                       "Akaike(1973)",
#                       "-")
#   fitMeasures$chiq_df <- c("","",
#                            round(fitMeasures[1,1]/fitMeasures[3,1],2),
#                            "","","","","","","","","")
#   # fitMeasures$fit_chek  <- c("absolute fit","",
#   #                            "absolute fit",
#   #                            "absolute fit ",
#   #                            "absolute fit ",
#   #                            "absolute fit ",
#   #                            "absolute fit ",
#   #                            "incremental fit",
#   #                            "incremental fit",
#   #                            "incremental fit",
#   #                            "parsimonious fit",
#   #                            "parsimonious fit")
#   fit <- fitMeasures  %>%
#     knitr::kable(digits=3, format=format,
#           caption="FitMeasure and criterian
#           (*)satisfy By kline(2011) Suggestion")
#
#
#
#
#   # TEST[[2]]$test %in% c("satorra.bentler", "yuan.bentler.mplus", "yuan.bentler")
#   # if(length(fitMeasures(x)) == 45 ){
#
#
#   if(length(fitMeasures(x)) == length(fitMeasures(x)) ){
#
#     #generarl reasearch
#     #modelfit
#     fitdata_00 <- fitMeasures(x,c("chisq","df","pvalue",
#                                   "rmsea",
#                                   "rmsea.ci.lower",
#                                   "rmsea.ci.upper",
#                                   "rmsea.pvalue",
#                                   "srmr",
#                                   "gfi",
#                                   "cfi",
#                                   "tli",
#                                   "aic",
#                                   "bic"
#     ))
#
#     criteria_data_00 = c("Chisq",
#                          "df",
#                          "p > .05",
#                          "RMSEA < .05",
#                          "90%CI.L",
#                          "90%CI.U",
#                          "p <= .05",
#                          "SRMR < .08",
#                          "GFI > .90",
#                          "CFI > .90",
#                          "TLI > .90",
#                          "AIC lower ",
#                          "BIC lower "
#     )
#
#     modelfitdata <-cbind.data.frame("criterian" = criteria_data_00,
#                                     "Value" = round(fitdata_00,3))
#
#   }else{
#
#     #01-2 robust research--------
#     #modelfit
#     fitdata <- fitMeasures(x,c("chisq","df","pvalue",
#                                "rmsea",
#                                "rmsea.ci.lower",
#                                "rmsea.ci.upper",
#                                "rmsea.pvalue",
#                                "srmr",
#                                "gfi",
#                                "cfi",
#                                "tli",
#                                "aic",
#                                "bic",
#                                "chisq.scaled", #roburst chisq
#                                "df.scaled",
#                                "pvalue.scaled",
#                                "chisq.scaling.factor",
#                                "cfi.robust",   # add
#                                "tli.robust",
#                                "rmsea.robust",
#                                "rmsea.ci.lower.robust",
#                                "rmsea.ci.upper.robust",
#                                "rmsea.pvalue.robust",
#                                "srmr_bentler",
#                                "srmr_mplus"
#
#     ))
#
#     criteria_data = c("Chisq",
#                       "df",
#                       "p > .05",
#                       "RMSEA < .05",
#                       "90%CI.L",
#                       "90%CI.U",
#                       "p <= .05",
#                       "SRMR < .08",
#                       "GFI > .90",
#                       "CFI > .90",
#                       "TLI > .90",
#                       "AIC lower ",
#                       "BIC lower ",
#                       "chisq.robust", #roburst chisq
#                       "df.robust",
#                       "p.robust",
#                       "Satorra-Bentler correction",
#                       "CFI.robust",   # add
#                       "TLI.robust",
#                       "RMSEA.robust",
#                       "RMSEA.ci.lower.robust",
#                       "RMSEA.ci.upper.robust",
#                       "RMASE.p.robust(blank=NA)",
#                       "SRMR_bentler",
#                       "SRMR_Mplus"
#          )
#
#     modelfitdata <-cbind("criterian"=criteria_data,
#                          "Value"=round(fitdata,3))
#
#        }
#
#
#
#     #summary
#   fitMeasures_s1 <- modelfitdata %>%
#     row2col("index")%>%
#     unite_rows(5, 6) %>% #unite ci
#     move_row(6,5) %>%
#     replace_df_rep("rmsea.ci.lower","90%CI.L",
#                     "rmsea.ci.upper", "90%CI.U") %>%
#     knitr::kable(format=format,
#            caption = "01 Model fit information")
#
#
#
#
#   ## 02 factor loading-----
#   options(knitr.kable.NA="")
#
#   factorloading_0 <- lavaan::parameterEstimates(x, standardized=TRUE) %>%
#     dplyr::filter(op=="=~") %>%
#     mutate(stars=ifelse(pvalue < 0.001, "***",
#                         ifelse(pvalue < 0.01, "**",
#                                ifelse(pvalue < 0.05, "*", "")))) %>%
#     mutate(label=ifelse(std.all>0.7,"Yes(Good)",
#                         ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
#     dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
#                   cr=z, Sig.=stars, "p"=pvalue,
#                   std=std.all, Accept=label)
#
#
#
#   ### factpr loadings Input New variable  ------
#   if(rename == TRUE){
#
#     factorloading <- factorloading_0 %>% mutate(Indicator= var_name)%>%
#       dplyr::select(Latent,Item ,Indicator , Est, S.E., cr, Sig., p ,
#                     std, Accept
#       ) %>%
#       knitr::kable(digits=3, format=format,
#             caption="02 Indicator Validity(1)
#           Factor Loadings:
#           (1) cr(critical ratio = Estimate/S.E) p<0.05,
#           (2) std.damda >= 0.5(Bagozzi & Yi(1988)")
#
#   }else{
#
#     factorloading <-factorloading_0 %>%
#       knitr::kable(digits=3, format=format,
#             caption="02 Indicator Validity(1)
#           Factor Loadings:
#           (1) cr(critical ratio = Estimate/S.E) p<0.05,
#           (2) std.damda >= 0.5(Bagozzi & Yi(1988)")
#   }
#
#
#
#
#
#   dataplot0 <-  lavaan::parameterEstimates(x, standardized=TRUE) %>%
#     dplyr::filter(op=="=~") %>%
#     mutate(stars=ifelse(pvalue < 0.001, "***",
#                         ifelse(pvalue < 0.01, "**",
#                                ifelse(pvalue < 0.05, "*", "")))) %>%
#     mutate(label=ifelse(std.all>0.7,"Yes(Good)",
#                         ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
#     dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
#                   cr=z, Sig.=stars, "p"=pvalue,
#                   std=std.all, beta_Accept=label)
#
#
#
#
#
#
#   #plotting data
#   dataplot<- dataplot0%>% select(Item,Latent, std)
#
#
#
#   varnames_check = dataplot0[,"Item"]
#   #02 -2 loadings -ggplot------
#
#   if(rename == TRUE ){
#     #02 -2 loadings -ggplot------
#     dataplot$Item <- var_name
#
#     gg <-ggplot(dataplot, aes(x=Item, y=std,
#                               fill=Latent))+
#       geom_bar(stat="identity", position='dodge')+
#       geom_hline(yintercept = cut , color= "darkred", linetype = 2)+ #cut: critera >0.7
#       geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: critera >0.7
#       ggtitle("Factor loadings")+
#       theme_bw()+
#       geom_text(aes(label=round(std,2)),
#                 vjust=-.3, size=val.size)+
#       ylim(0, 1.1)+
#       theme(axis.text.x = element_text(
#         angle=angle,
#         size = cex, hjust = hjust,
#         face="bold")) #angle, cex
#
#   }else if(rename == FALSE){
#
#     gg <-ggplot(dataplot, aes(x=Item, y=std,
#                               fill=Latent))+
#       geom_bar(stat="identity", position='dodge')+
#       geom_hline(yintercept = cut ,  color= "darkred", linetype=2)+ #cut: critera >0.7
#       geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: critera >0.7
#       ggtitle("Factor loadings")+
#       theme_bw()+
#       geom_text(aes(label=round(std,2)),
#                 vjust=-.3, size=val.size)+
#       ylim(0, 1.1 )+
#       theme(axis.text.x = element_text(
#         angle= angle,
#         size = cex, hjust = hjust,
#         face="bold")) #angle, cex
#   }else{
#     gg <-ggplot(dataplot,aes(x=Item, y=std,
#                              fill=Latent))+
#       geom_bar(stat="identity", position='dodge')+
#       geom_hline(yintercept = cut,  color= "darkred", linetype=2)+ #cut: critera >0.7
#       geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: critera >0.7
#       ggtitle("Factor loadings")+
#       theme_bw()+
#       geom_text(aes(label=round(std,2)),
#                 vjust=-.3, size=val.size)+
#       ylim(0,1.1)+
#       theme(axis.text.x = element_text(
#         angle=angle,
#         size = cex, hjust = hjust,
#         face="bold")) #angle, cex
#   }
#
#
#
#   #Cronbach alpha
#   alpha.1 <-  semTools::reliability(x,return.total = F) %>%
#     # alpha.1 <- reliability(x) %>%
#     t() %>%
#     as.data.frame() %>%
#     dplyr::select("Cronbach"=alpha,"CR" = omega3) %>%
#     mutate(alpha_Check=ifelse(Cronbach>0.7,"Accept(>0.7) *",
#                               ifelse(Cronbach>0.6,"Yes(poor) *", "Reject"))) %>%
#     mutate(CR_Check=ifelse(CR>0.7,"Accept(>0.7) *","Reject")) %>%
#     dplyr::select(Cronbach,alpha_Check,CR,CR_Check )
#
#
#   #05 Reprort cronbach, AVE, C.R
#   FL.1 <- cbind(alpha.1)
#   FL <- FL.1%>%knitr::kable(digits=3, format=format,
#                     caption="03-1. Internal consistency
#           (Cronbach's Alpha, 1951) and Composite Relibility")
#
#
#
#
#   ## 03 CR,AVE-----
#   # ?semTools::reliability
#   AVE <-  semTools::reliability(x, return.total = F) %>%
#     t() %>%
#     as.data.frame() %>%
#     dplyr::select( "AVE"=avevar)
#
#   sqrt.AVE <- sqrt(AVE)
#   colnames(sqrt.AVE)="sqrt.AVE"
#
#   #correlations Matrix
#   rho <- lavaan::lavInspect(x,"std")$beta
#
#
#
#
#   ## 04 Convergent validity-----
#   alpha_AVE_CR_0 <-   semTools::reliability(x,
#                           return.total = FALSE) %>%
#     # alpha_AVE_CR <-  reliability(x) %>%
#     t() %>%
#     as.data.frame() %>%
#     dplyr::select("Cronbach"=alpha,
#                   "CR" = omega3, "AVE"=avevar) %>%
#     mutate(sqrt.AVE=sqrt(AVE))%>%
#     mutate(AVE_check=ifelse(AVE>0.5,"Accept(>0.5) *","Reject"))%>%
#     dplyr::select(Cronbach,CR, AVE,AVE_check, #sqrt.AVE
#     )
#   alpha_AVE_CR <-   alpha_AVE_CR_0 %>%
#     knitr::kable(digits = 3,format = format,
#           caption = "03 Convergent validity
#           Internal consistency(Cronbach's Alpha, 1951)(>0.7)
#           AVE(>0.5) & CR(>0.7): Fornell & Lacker(1981)")
#
#
#
#
#
#   #05-1 discriminant validity=====
#   betaa <- lavaan::lavInspect(x, "std")$beta
#
#   if(is.null(betaa)){
#
#     psi <-lavaan::lavInspect(x, "std")$psi
#     psi[lower.tri(psi)==FALSE]<-0
#
#     rho1<- psi %>% as.data.frame()
#     rho1$max<- apply(rho1,1,max)
#     # diff<- cbind(rho1$max, sqrt.AVE) # not rwow match(need calculate )
#     # diff$delta<-diff[,2]- diff[,1]
#     # diff$sig<-ifelse(diff$delta >= 0,"*","ns")
#     #
#     # FornellNacker <-cbind(psi, max_rho=diff[,1],
#     #                       sqrt.AVE,  # row 396
#     #                       sig=diff[,4]) %>% as.data.frame()
#
#     #New revise
#     rho1 <- rho1 %>% mutate(max = apply(rho1,1, max),
#                             lv = rownames(rho1))
#     sqrt.AVE$lv <- rownames(sqrt.AVE)
#
#     # diff <- merge(x=rho1, y=sqrt.AVE, by="lv",
#     #               all=TRUE, sort = dis.sort)
#     diff_0 <- merge(x = rho1[,c("max","lv")],
#                     y = sqrt.AVE, by = "lv",
#                     all = TRUE,
#                     sort = FALSE)
#     diff <- merge(x = rho1[,-(ncol(rho1)-1)],
#                   y = diff_0, by = "lv",
#                   all = TRUE,
#                   sort = FALSE)
#     # diff$sqrt.AVE[is.na(diff$sqrt.AVE)] <- 0
#
#
#     # diff <- cbind(rho1$max, sqrt.AVE)
#     # diff$delta <- diff$sqrt.AVE - diff$max
#     # diff$sig <-ifelse(sqrt.AVE == 0, "-",
#     #                   ifelse(diff$delta >= 0, "*", "ns"))
#
#     diff$delta <- diff[,(ncol(diff))]- diff[,(ncol(diff)-1)]
#     diff$sig<-ifelse(diff$delta >= 0,"*","ns")
#
#
#     FornellNacker <- diff[,c(-(ncol(diff)-1))]
#
#     validity <- FornellNacker %>%
#       knitr::kable(digits=3, format=format,
#             caption="04 Discriminant Validity:
#           rho < Square Root of(AVE)
#            By Fornell & Lacker(1981)")
#
#   }else{
#     lv.cor <- lavaan::lavInspect(x, what="cor.lv")
#     lv.cor1 <- lv.cor
#     diag(lv.cor1)<-0
#
#     rho1 <- lv.cor1 %>% as.data.frame()
#     rho1[lower.tri(rho1)==FALSE]<-0
#     rho1$max <- apply(rho1,1, max)
#
#     # bind data
#     rho1 <- rho1 %>% mutate(max=apply(rho1,1, max),
#                             lv =rownames(rho1))
#     sqrt.AVE$lv <- rownames(sqrt.AVE)
#
#     # diff <- merge(x=rho1, y=sqrt.AVE, by="lv",
#     #               all=TRUE, sort = dis.sort)
#     diff_0 <- merge(x=rho1[,c("max","lv")],
#                     y=sqrt.AVE, by="lv",
#                     all=TRUE, sort=FALSE)
#     diff <- merge(x= rho1[,-(ncol(rho1)-1)],
#                   y=diff_0, by="lv",
#                   all=TRUE, sort = FALSE)
#     # diff$sqrt.AVE[is.na(diff$sqrt.AVE)] <- 0
#
#     # diff <- cbind(rho1$max, sqrt.AVE)
#     diff$delta <- diff[,(ncol(diff))]- diff[,(ncol(diff)-1)]
#     diff$sig<-ifelse(diff$delta >= 0,"*","ns")
#
#
#
#     FornellNacker <- diff[,c(-(ncol(diff)-1))]
#     # cbind(rho1, max_rho=diff[,1], sqrt.AVE,
#     #                     sig=diff[,4]) %>% as.data.frame()
#     #
#
#     validity <- FornellNacker %>%
#       knitr::kable(digits=3, format=format,
#             caption="04 Discriminant Validity:
#           rho < Square Root of(AVE)
#            By Fornell & Lacker(1981)")
#   }
#
#
#
#   # 05-2 discriminant :HTMT #####
#   # Assessing Discriminant Validity using Heterotrait–Monotrait Ratio
#   # discriminant validity through the heterotrait-monotrait ratio (HTMT) of the correlations (Henseler, Ringlet & Sarstedt, 2015)
#
#
#
# # if( is.character(model)==TRUE |
# #     is.data.frame(dataset)==TRUE){
# #
# #   options(knitr.kable.NA = '') # hide NA
# #   # generate dataframe
# #   htmt0 <- semTools::htmt(model, dataset) %>%
# #     as.data.frame()
# #
# #   htmt0[lower.tri(htmt0)==FALSE]<-0 # diag =0
# #   htmt0NA <- htmt0 # NA remove
# #   htmt0NA[lower.tri(htmt0)==FALSE]<-NA   # upper to NA
# #   htmt1 <- htmt0 %>%   #make sig
# #     mutate(Max = apply(htmt0, 1, max, na.rm=T),  # max
# #            dis = ifelse(htmt_cut - Max== htmt_cut, 0, htmt_cut - Max),  #discriminant
# #            sig = ifelse(htmt_cut- Max >= 0,"*","ns")) #significant
# #   htmt2 <-cbind(htmt0NA,
# #                 htmt1[,c(ncol(htmt1)-2, #max
# #                          ncol(htmt1)-1, #dis
# #                          ncol(htmt1))] ) #sig
# #
# # #
# #
# #     htmt <- htmt2  %>%
# #       knitr::kable(format=format, digits = digits,
# #        caption="The heterotrait-monotrait ratio of correlations (HTMT).
# #           All correalation < 0.9 --> discriminant Accept(roburst:0.85)
# #           general accept: < 1
# #           (Henseler, Ringlet & Sarstedt, 2015)
# #
# #           ")
# #   #
#   #
#   # }else{
#   #   htmt <- print("Not calculation HTMT, input syntax is [ model = lavaan model,dataset = data] ")
#   #
#   # }
#
#
#     htmt2 = lav_htmt(x, cut = htmt_cut, htmt2 = htmt2 ,  digits= digits)
#     htmt <- htmt2  %>%
#       knitr::kable(format=format, digits = digits,
#       caption="The heterotrait-monotrait ratio of correlations (HTMT).
#           All correalation < 0.9 --> discriminant Accept(roburst:0.85)
#           general accept: < 1
#           (Henseler, Ringlet & Sarstedt, 2015)
#
#           ")
#
#
#
#
#
#   ##06 cor significant----
#   lv.cor.sig0 <- lavaan::parameterEstimates(x, standardized = T) %>%
#     dplyr::filter(op=="~"|op=="~~"&lhs != rhs) %>%
#     dplyr::select(lhs,op,rhs, std.lv, pvalue) %>%
#     mutate(sig=ifelse(pvalue < 0.001, "***",
#                       ifelse(pvalue < 0.01, "**",
#                              ifelse(pvalue < 0.05, "*", "Not Sig")))) %>%
#     mutate(op=ifelse(op=="~","<--",
#                      ifelse(op=="~~","cor",""))) %>%
#     dplyr::select(lhs,op,rhs, std.lv, pvalue,sig)
#
#   lv.cor.sig = lv.cor.sig0 %>%
#     knitr::kable(digits=3, format=format,
#           caption="05 latent correlation Significant Check")
#
#
#   #model return
#    model= lav_return_model(x)
#
#
#
#
#   ## final result  --------------
#   all.reuslt <-list(model = model,
#                     fit_criterian = fit,
#                     model_fit = fitMeasures_s1,
#                     factorloadings = factorloading,
#                     Internal_Consistency = FL,
#                     Convergent = alpha_AVE_CR_0,
#                     Discriminant = validity,
#                     Discriminant_HTMT = htmt,
#                     # Latent_Cor=lv.cor,
#                     betaMat_sig = lv.cor.sig,
#                     loadings_Bar = gg,
#                     variable_order = varnames_check
#   )
#
#
#   raw = list(fit=fitMeasures,
#              model_fit=modelfitdata,
#             factorloading = factorloading_0,
#
#             convergent = alpha_AVE_CR_0,
#             discriminant = FornellNacker,
#             htmt = htmt2,
#             bar = gg,
#             lv.cor.sig = lv.cor.sig0)
#   # all.reuslt
#   ## cfa2() output option ---------------
#   # switch(res,
#   #        all = all.reuslt,
#   #        model = model,
#   #        modelfit = fit,
#   #        modelfit2 = fitMeasures_s1,
#   #        loadings = factorloading,
#   #        alpha = FL,
#   #        CR_AVE = alpha_AVE_CR,
#   #        Convegent = alpha_AVE_CR,
#   #        fl_criteria = validity,
#   #        Discriminant = validity,
#   #        htmt = htmt,
#   #        HTMT = htmt,
#   #        # Latent_Cor=lv.cor,
#   #        str_cor = lv.cor.sig,
#   #        loadings_Bar = gg )
#   switch(type,
#          all = all.reuslt,
#          raw = raw,
#          data.frame = raw,
#          model = model,
#          modelfit = modelfitdata,
#          modelfit2 = fitMeasures_s1,
#          loadings = factorloading_0,
#          item = factorloading_0,
#          indicator = factorloading_0,
#          loadings_bar = gg,
#
#          alpha = FL.1,
#          CR_AVE = alpha_AVE_CR_0,
#
#          Convergent = alpha_AVE_CR_0,
#
#          fl_criteria = FornellNacker,
#          Discriminant = FornellNacker,
#          htmt = htmt2,
#          HTMT = htmt2,
#          # Latent_Cor=lv.cor,
#          str_cor = lv.cor.sig0
#          )
#
# }
#






#cfa3 using data treament =============
cfa3 <- function(x, graph=F, type="all"){

  library(dplyr)
  library(knitr)
  library(lavaan)
  library(semTools)
  library(tibble)
  library(semPlot)

  # tryCatch({

  #01 fit table
  options(scipen = 100)

  fit.indices=c("chisq","pvalue", "df","rmsea",
                "gfi","agfi","srmr","cfi","tli","nfi","aic","bic")
  fitMeasures <- round(fitMeasures(x,fit.indices),3)
  fitMeasures_s <- round(fitMeasures(x,fit.indices),3)


  # fitMeasures <- as.data.frame(fitMeasures(x,fit.indices))
  fitMeasures <- as.data.frame(fitMeasures)
  fitMeasures$critera <- c("",
                           "*p.value >= 0.05",
                           "_chisq/df <= 3(<5(ok)",
                           "*RMSEA< 0.05(or 0.08)",
                           "*GFI >= 0.95",
                           "_AGFI>= 0.90",
                           "*SRMR < 0.08",
                           "*CFI >= 0.95",
                           "_TLI >= 0.90",
                           "_NFI >= 0.90",
                           "_lower",
                           "_lower")
  fitMeasures$Ref <-c("-",
                      "-",
                      "Wheaton et al.(1977)",
                      "Browne & Cudek(1993)",
                      "Joreskog-Sorbom(1970)",
                      "Tanaka & Huba(1985)",
                      "Hu & Bentler(1999)",
                      "Kline(2011)",
                      "Bentler & Bonett(1980)",
                      "Bollen(1989)",
                      "Akaike(1973)",
                      "-")
  fitMeasures$chiq_df <- c("","",
                           round(fitMeasures[1,1]/fitMeasures[3,1],2),
                           "","","","","","","","","")
  # fitMeasures$fit_chek  <- c("absolute fit","",
  #                            "absolute fit",
  #                            "absolute fit ",
  #                            "absolute fit ",
  #                            "absolute fit ",
  #                            "absolute fit ",
  #                            "incremental fit",
  #                            "incremental fit",
  #                            "incremental fit",
  #                            "parsimonious fit",
  #                            "parsimonious fit")
  fit <- fitMeasures  #%>%
  # knitr::kable(digits=3, format=format,
  #       caption="FitMeasure and criterian
  #       (*)satisfy By kline(2011) Suggestion")
  #


  #modelfit
  fitdata <- fitMeasures(x,c("chisq","df","pvalue",
                             "rmsea",
                             "rmsea.ci.lower",
                             "rmsea.ci.upper",
                             "rmsea.pvalue",
                             "srmr",
                             "gfi",
                             "cfi",
                             "tli",
                             "aic",
                             "bic"
  ))

  criteria_data = c("Chisq",
                    "df",
                    "p >0.05",
                    "RMSEA <0.05",
                    "90%CI.lower",
                    "90%CI.upper",
                    "p >0.05",
                    "SRMR <0.08",
                    "GFI >0.95",
                    "CFI >0.95",
                    "TLI>0.90",
                    "lower ",
                    "lower "
  )

  modelfitdata <-cbind("criterian"=criteria_data,
                       "Value"=round(fitdata,3))

  fitMeasures_s1 <- modelfitdata# %>%
  # knitr::kable (format=format,
  #        caption = "01 Model fit information")
  #



  #04 factor loading
  options(knitr.kable.NA="")



  #
  #
  #     factorloading <- lavaan::parameterEstimates(x, standardized=TRUE) %>%
  #         dplyr::filter(op=="=~") %>%
  #         mutate(stars=ifelse(pvalue < 0.001, "***",
  #                             ifelse(pvalue < 0.01, "**",
  #                                    ifelse(pvalue < 0.05, "*", "")))) %>%
  #         mutate(label=ifelse(std.all>0.7,"Yes(Good)",
  #                             ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
  #         dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
  #                       cr=z, Sig.=stars, "p"=pvalue,
  #                       std=std.all, beta_Accept=label) #%>%
  #     # knitr::kable(digits=3, format=format,
  #     #       caption="02 Indicator Validity(1)-Factor Loadings::
  #     #       (1) cr(critical ratio =Estimate/S.E) p<0.05,
  #     #       (2) std.damda >= 0.5(Bagozzi & Yi(1988)")
  #
  ## 02 factor loading-----
  factorloading <- lavaan::parameterEstimates(x, standardized=TRUE) %>%
    dplyr::filter(op=="=~") %>%
    mutate(stars=ifelse(pvalue < 0.001, "***",
                        ifelse(pvalue < 0.01, "**",
                               ifelse(pvalue < 0.05, "*", "")))) %>%
    mutate(label=ifelse(std.all>0.7,"Yes(Good)",
                        ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
    dplyr::select("Latent"=lhs, Item=rhs, Est=est,S.E.=se,
                  cr=z, Sig.=stars, "p"=pvalue,
                  std=std.all, beta_Accept=label)


  if(graph==T){
    dataplot0 <- lavaan::parameterEstimates(x, standardized=TRUE) %>%
      dplyr::filter(op=="=~") %>%
      mutate(stars=ifelse(pvalue < 0.001, "***",
                          ifelse(pvalue < 0.01, "**",
                                 ifelse(pvalue < 0.05, "*", "")))) %>%
      mutate(label=ifelse(std.all>0.7,"Yes(Good)",
                          ifelse(std.all>0.5,"Yes(fair)","No"))) %>%
      dplyr::select("Latent"=lhs,
                    Item=rhs,
                    Est=est,
                    S.E.=se,
                    cr=z,
                    Sig.=stars,
                    "p"=pvalue,
                    std=std.all,
                    beta_Accept=label)
    dataplot<- dataplot0%>% select(Item,Latent, std)


    gg <-ggplot(dataplot,aes(x=Item, y=std, fill=Latent))+
      geom_bar(stat="identity", position='dodge')+
      geom_hline(yintercept = cut, color= "red")+ #cut:  0.7
      geom_hline(yintercept = cut-0.2, color= "gray40")+ #cut: 0.7
      ggtitle("factor loadings")+
      geom_text(aes(label=round(std,2)),vjust=-.3, size=val.size)+
      theme(axis.text.x = element_text(
        angle=angle,
        size = cex, hjust = hjust,
        face="bold")) #angle, cex
  }




  #Cronbach alpha
  alpha.1 <- semTools::reliability(x,return.total = F) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select( "Cronbach"=alpha,"CR" = omega3) %>%
    mutate(alpha_Check=ifelse(`Cronbach`>0.7,"Accept(>0.7) *",
                              ifelse(`Cronbach's alpha`>0.6,"Yes(poor) *", "Reject"))) %>%
    mutate(CR_Check=ifelse(CR>0.7,"Accept(>0.7) *","Reject")) %>%
    dplyr::select(Cronbach,alpha_Check,CR,CR_Check )


  #05 Reprort cronbach, AVE, C.R
  FL.1 <- cbind(alpha.1)
  FL <-FL.1 #%>% kable(digits=3, format=format,
  #  caption="03-1. Internal consistency
  # (Cronbach's Alpha, 1951) and Composite Validity")




  #CR,AVE

  AVE <- semTools::reliability(x,return.total = F) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select( "AVE"=avevar)

  sqrt.AVE <- sqrt(AVE)
  colnames(sqrt.AVE)="sqrt.AVE"

  #correlations Matrix
  rho <- lavaan::lavInspect(x,"std")$beta




  # Convergent validity
  alpha_AVE_CR <-  semTools::reliability(x,return.total = F) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select("Cronbach"=alpha, "CR" = omega3, "AVE"=avevar) %>%
    mutate(sqrt.AVE=sqrt(AVE))%>%
    mutate(AVE_check=ifelse(AVE>0.5,"Accept(>0.5) *","Reject"))%>%
    dplyr::select(Cronbach,CR, AVE,AVE_check, #sqrt.AVE
    ) #%>%
  # knitr::kable(digits = 3,format = format,
  #       caption = "03 Convergent validity
  #       Internal consistency(Cronbach's Alpha, 1951)(>0.7)
  #       AVE(>0.5) & CR(>0.7): Fornell & Lacker(1981)")



  #06 discriminant validity
  betaa <-lavaan::lavInspect(x, "std")$beta

  if(is.null(betaa)){

    psi <-lavaan::lavInspect(x, "std")$psi
    psi[lower.tri(psi)==FALSE]<-0

    rho1<- psi %>% as.data.frame()
    rho1$max<- apply(rho1,1,max)
    diff<- cbind(rho1$max,sqrt.AVE)
    diff$delta<-diff[,2]- diff[,1]
    diff$sig<-ifelse(diff$delta>0,"*","Not Sig")

    FornellNacker <-cbind.data.frame(psi, max_rho=diff[,1],sqrt.AVE,
                                     sig=diff[,4]) %>% as.data.frame()


    validity <- FornellNacker #%>%
    # knitr::kable(digits=3, format=format,
    #       caption="04 Discriminant Validity:
    #     rho < Square Root of(AVE)
    #      By Fornell & Lacker(1981)")

  }else{
    lv.cor <- lavaan::lavInspect(x, what="cor.lv")
    lv.cor1 <- lv.cor
    diag(lv.cor1)<-0

    rho1 <- lv.cor1 %>% as.data.frame()
    rho1[lower.tri(rho1)==FALSE]<-0
    rho1$max <- apply(rho1,1, max)

    #
    rho1 <- rho1 %>% mutate(max=apply(rho1,1, max), lv =rownames(rho1))
    sqrt.AVE$lv <- rownames(sqrt.AVE)

    diff <- merge(x=rho1, y=sqrt.AVE, by="lv", all=TRUE)

    diff$sqrt.AVE[is.na(diff$sqrt.AVE)] <- 0

    # diff <- cbind(rho1$max, sqrt.AVE)
    diff$delta <- diff$sqrt.AVE- diff$max
    diff$sig <-ifelse(diff$delta>0,"*","ns")


    FornellNacker <- diff[,c(-(ncol(diff)-1))]
    # cbind(rho1, max_rho=diff[,1], sqrt.AVE,
    #                     sig=diff[,4]) %>% as.data.frame()
    #

    validity <- FornellNacker# %>%
    #   knitr::kable(digits=3, format=format,
    #         caption="04 Discriminant Validity:
    # rho < Square Root of(AVE)
    #  By Fornell & Lacker(1981)")

  }



  # cor significant
  lv.cor.sig <- lavaan::parameterEstimates(x, standardized = T) %>%
    dplyr::filter(op=="~"|op=="~~"&lhs != rhs) %>%
    dplyr::select(lhs,op,rhs, std.lv, pvalue) %>%
    mutate(sig=ifelse(pvalue < 0.001, "***",
                      ifelse(pvalue < 0.01, "**",
                             ifelse(pvalue < 0.05, "*", "Not Sig")))) %>%
    mutate(op=ifelse(op=="~","<--",
                     ifelse(op=="~~","cor",""))) %>%
    dplyr::select(lhs,op,rhs, std.lv, pvalue,sig)# %>%
  # knitr::kable(digits=3, format=format,
  #       caption="05 latent correlation Significant Check")
  #



  all.reuslt <-list(
    # fit_criterian=fit,
    model_fit=fitMeasures_s1,
    factorloadings=factorloading,
    Internal_Consistency=FL,
    Convegent=alpha_AVE_CR,
    Discriminant=validity,
    # Latent_Cor=lv.cor,
    betaMat_sig=lv.cor.sig
  )

  switch(type,
         all= all.reuslt,
         fit_criterian=fit,
         model_fit=fitMeasures_s1,
         factorloadings=factorloading,
         reliability = FL,
         Convegent = alpha_AVE_CR,
         Discriminant=validity,
         Latent_Cor=lv.cor,
         betaMat_sig=lv.cor.sig
         )
  #
  # all.reuslt
}
