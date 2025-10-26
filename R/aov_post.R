
#' ANOVA posthoc and plot, Contrast data
#'
#' @param aov_data aov data
#' @param type type is all, res, contras, df, raq(plotdata), g(plot), aov, anova
#' @param adjust adjust ="none", bonferroni, sidak etc
#' @param lsd plosthoc LSD
#'
#' @return contrast and g
#' @export
#'
#' @examples
#'
#' \dontrun{
#' Mtcars <- mtcars%>%as_trt("cyl")
#' aov(mpg ~ cyl , data=Mtcars)%>% aov_post()
#' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="all")
#' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="res")
#' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="df")
#' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="call")
#' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="formula")
#' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="g")
#' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="contrast")
#' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="aov")
#' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="anova")
#' aov(mpg ~ cyl , data=Mtcars)%>% aov_post(type="posthoc", lsd=TRUE)
#'
#' #report
#' aov(mpg ~ cyl , data=Mtcars)%>%aov_post()%>% anova_apa()
#' }
#'
#'
aov_post = function(aov_data, type= "df",
                    adjust="none", lsd=FALSE){
  plot_data <- aov_data
  summary_anova <- summary(aov_data)
  summary_anova_df <- broom::tidy(aov_data)


  formula = plot_data$call
  form = plot_data$call[2]%>%formula()%>% as.character()
  iv  = form[3]

  contrast = aov_data %>%
    emmeans::emmeans(iv)%>%
    pairs(simple = iv, adjust = adjust)%>%p_mark_sig()


  g = plot_data %>%
    emmeans::emmeans(iv)%>%
    pairs(simple = iv, adjust = adjust)%>%plot()+
    geom_vline(xintercept = 0, col="red", linetype=2)+theme_bw()


  if(lsd){
    LSD_test = agricolae::LSD.test(aov_data,iv, group=FALSE,console=TRUE)
    res = list(LSD_test = LSD_test,
               contrast = contrast,
               aov=summary_anova_df,
               confint_sig = g)
  }else{
    LSD_test = NULL
    res = list(LSD_test = LSD_test,
               call = form,
               contrast = contrast,
               aov=summary_anova_df,
               confint_sig = g)
  }

  res2 = list(call = form,
              aov=summary_anova_df,
              contrast = contrast,
              confint_sig = g)

  switch(type,
         all = res,
         res = res2,
         contrast = contrast,
         df = list(call = form, aov=summary_anova_df, contrast=contrast),
         raw = plot_data,
         call = form,
         formula = formula,
         g = g,
         aov = summary_anova,
         anova = summary_anova_df,
         posthoc = LSD_test)
}
