
#' ANOVA posthoc and plot, Contrast data
#'
#' Post-hoc analysis and plotting for ANOVA results. Supports various posthoc methods
#' including LSD, Duncan, Scheffe, Tukey HSD, SNK, FDR and Bonferroni correction.
#'
#' @param aov_data An object returned by \code{aov()}; ANOVA model object.
#' @param type Output type. One of: \code{"all"}, \code{"res"}, \code{"contrast"},
#' \code{"df"}, \code{"raw"}, \code{"g"}, \code{"aov"}, \code{"anova"}, \code{"call"},
#' \code{"formula"}, \code{"posthoc"}.
#' @param adjust Method for p-value adjustment for emmeans contrast. One of \code{"none"},
#' \code{"bonferroni"}, \code{"fdr"}, etc. Only used in contrast.
#' @param posthoc Logical; if \code{TRUE}, performs posthoc test using agricolae.
#' @param posthoc_method Posthoc method: one of \code{"LSD"}, \code{"BONFERRONI"},
#' \code{"FDR"}, \code{"SCHEFFE"}, \code{"DUNCAN"}, \code{"TUKEY"}, \code{"SNK"}.
#'
#' @return Depending on \code{type}, returns a list or specific object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(agricolae)
#' library(dplyr)
#' library(emmeans)
#' library(broom)
#' library(ggplot2)
#'
#' # Example 1: mtcars dataset (mpg ~ cyl)
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' fit1 <- aov(mpg ~ cyl, data = mtcars)
#' aov_post(fit1, type = "df", posthoc = TRUE, posthoc_method = "LSD")
#' aov_post(fit1, type = "df", posthoc = TRUE, posthoc_method = "BONFERRONI")
#' aov_post(fit1, type = "df", posthoc = TRUE, posthoc_method = "FDR")
#' aov_post(fit1, type = "df", posthoc = TRUE, posthoc_method = "SCHEFFE")
#' aov_post(fit1, type = "df", posthoc = TRUE, posthoc_method = "DUNCAN")
#' aov_post(fit1, type = "df", posthoc = TRUE, posthoc_method = "TUKEY")
#' aov_post(fit1, type = "df", posthoc = TRUE, posthoc_method = "SNK")
#'
#' # Example 2: ToothGrowth dataset (len ~ dose)
#' ToothGrowth$dose <- as.factor(ToothGrowth$dose)
#' fit2 <- aov(len ~ dose, data = ToothGrowth)
#' aov_post(fit2, type = "df", posthoc = TRUE, posthoc_method = "DUNCAN")
#'
#' # Example 3: iris dataset (Sepal.Length ~ Species)
#' fit3 <- aov(Sepal.Length ~ Species, data = iris)
#' aov_post(fit3, type = "df", posthoc = TRUE, posthoc_method = "SCHEFFE")
#' }
aov_post2 <- function(aov_data,
                      type = "df",
                      adjust = "none",
                      posthoc = TRUE,
                      posthoc_method = "LSD") {
  # library(dplyr)
  # library(broom)
  # library(ggplot2)
  # library(emmeans)
  # library(agricolae)


  # formula 파싱
  formula <- aov_data$call
  form <- aov_data$call[2] %>% formula() %>% as.character()
  dv <- form[2]
  iv <- form[3]

  # 기본 통계 요약
  summary_anova <- summary(aov_data)
  summary_anova_df <- broom::tidy(aov_data)

  # contrast 계산 (emmeans)
  contrast <- aov_data %>%
    emmeans::emmeans(iv) %>%
    pairs(simple = iv, adjust = adjust) %>%
    p_mark_sig()

  # 신뢰구간 시각화
  g <- aov_data %>%
    emmeans::emmeans(iv) %>%
    pairs(simple = iv, adjust = adjust) %>%
    plot() +
    ggplot2::geom_vline(xintercept = 0, col = "red", linetype = 2) +
    ggplot2::theme_bw()

  # 평균 요약
  means <- aov_data$model %>%
    dplyr::group_by(!!rlang::sym(iv)) %>%
    dplyr::summarise(Mean = mean(!!rlang::sym(dv), na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(level = !!rlang::sym(iv))

  # 사후검정
  LSD_test <- NULL
  posthoc_letters <- means %>% dplyr::mutate(POSTHOC = NA_character_)

  if (posthoc) {
    method <- toupper(posthoc_method)
    LSD_test <- switch(method,
                       "LSD" = agricolae::LSD.test(aov_data, iv, group = TRUE),
                       "BONFERRONI" = agricolae::LSD.test(aov_data, iv, group = TRUE, p.adj = "bonferroni"),
                       "FDR" = agricolae::LSD.test(aov_data, iv, group = TRUE, p.adj = "fdr"),
                       "SCHEFFE" = agricolae::scheffe.test(aov_data, iv, group = TRUE),
                       "DUNCAN" = agricolae::duncan.test(aov_data, iv, group = TRUE),
                       "SNK" = agricolae::SNK.test(aov_data, iv, group = TRUE),
                       "TUKEY" = agricolae::HSD.test(aov_data, iv, group = TRUE),
                       stop("Invalid posthoc_method. Choose from: LSD, DUNCAN, SCHEFFE, SNK, TUKEY, BONFERRONI, FDR.")
    )

    posthoc_letters <- LSD_test$groups %>%
      tibble::rownames_to_column("level") %>%
      dplyr::rename(Mean = !!names(LSD_test$groups)[1], POSTHOC = groups)
  }

  # 최종 출력 테이블 구성
  output_table <- posthoc_letters %>%
    dplyr::mutate(
      iv = iv,
      dv = dv,
      df1 = summary_anova_df$df[1],
      df2 = summary_anova_df$df[2],
      F_value = summary_anova_df$statistic[1],
      p_value = summary_anova_df$p.value[1]
    ) %>%
    dplyr::select(iv, dv, level, Mean, POSTHOC, df1, df2, F_value, p_value)

  # 출력 결과 구성
  res <- list(
    call = form,
    aov = summary_anova_df,
    contrast = contrast,
    confint_sig = g,
    LSD_test = LSD_test,
    table = output_table
  )

  res2 <- list(
    call = form,
    aov = summary_anova_df,
    contrast = contrast,
    confint_sig = g,
    table = output_table
  )

  switch(type,
         all = res,
         res = res2,
         contrast = contrast,
         df = output_table,
         raw = aov_data,
         call = form,
         formula = formula,
         g = g,
         aov = summary_anova,
         anova = summary_anova_df,
         posthoc = LSD_test)
}


#'
#'
#'
#' #' ANOVA posthoc and plot, Contrast data
#' #'
#' #' @param aov_data aov data
#' #' @param type type is all, res, contras, df, raq(plotdata), g(plot), aov, anova
#' #' @param adjust adjust ="none", bonferroni, sidak etc
#' #' @param lsd plosthoc LSD
#' #'
#' #' @return contrast and g
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' \dontrun{
#' #' Mtcars <- mtcars%>%as_trt("cyl")
#' #' aov(mpg ~ cyl , data=Mtcars)%>% aov_post()
#' #' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="all")
#' #' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="res")
#' #' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="df")
#' #' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="call")
#' #' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="formula")
#' #' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="g")
#' #' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="contrast")
#' #' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="aov")
#' #' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="anova")
#' #' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="posthoc", lsd=TRUE)
#' #'
#' #' #report
#' #' aov(mpg ~ cyl , data=Mtcars)%>%aov_post()%>% anova_apa()
#' #' }
#' #'
#' #'
#' aov_post = function(aov_data, type= "df",
#'                     adjust="none", lsd=FALSE){
#'   plot_data <- aov_data
#'   summary_anova <- summary(aov_data)
#'   summary_anova_df <- broom::tidy(aov_data)
#'
#'
#'   formula = plot_data$call
#'   form = plot_data$call[2]%>%formula()%>% as.character()
#'   iv  = form[3]
#'
#'   contrast = aov_data %>%
#'     emmeans::emmeans(iv)%>%
#'     pairs(simple = iv, adjust = adjust)%>%p_mark_sig()
#'
#'
#'   g = plot_data %>%
#'     emmeans::emmeans(iv)%>%
#'     pairs(simple = iv, adjust = adjust)%>%plot()+
#'     geom_vline(xintercept = 0, col="red", linetype=2)+theme_bw()
#'
#'
#'   if(lsd){
#'     LSD_test = agricolae::LSD.test(aov_data,iv, group=FALSE,console=TRUE)
#'     res = list(LSD_test = LSD_test,
#'                contrast = contrast,
#'                aov=summary_anova_df,
#'                confint_sig = g)
#'   }else{
#'     LSD_test = NULL
#'     res = list(LSD_test = LSD_test,
#'                call = form,
#'                contrast = contrast,
#'                aov=summary_anova_df,
#'                confint_sig = g)
#'   }
#'
#'   res2 = list(call = form,
#'               aov=summary_anova_df,
#'               contrast = contrast,
#'               confint_sig = g)
#'
#'   switch(type,
#'          all = res,
#'          res = res2,
#'          contrast = contrast,
#'          df = list(call = form, aov=summary_anova_df, contrast=contrast),
#'          raw = plot_data,
#'          call = form,
#'          formula = formula,
#'          g = g,
#'          aov = summary_anova,
#'          anova = summary_anova_df,
#'          posthoc = LSD_test)
#' }
