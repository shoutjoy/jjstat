#' k df rep
#'
#' @param df data.frame
#' @param range range
#' @param slang en
#' @param elang ko
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #'
#' Authentic_Leadership <- data.frame(
#'   id = paste0("Q",1:16),
#'   factor= c(
#'     rep("self Awareness", 4),
#'     rep("Relational Transparency", 4),
#'     rep("Internalized Moral perspective", 4),
#'     rep("Balanced Processing", 4)
#'   ),
#'   Question = c(
#'     "I can list my three greatest weaknesses.",
#'     "My actions reflect my core values.",
#'     "I seek others' opinions before making up my own mind.",
#'     "I openly share my feelings with others.",
#'     "I can list my three greatest strengths.",
#'     "I do not allow group pressure to control me.",
#'     "I listen closely to the ideas of those who disagree with me.",
#'     "I let others know who I truly am as a person.",
#'     "I seek feedback as a way of understanding who I really am as a person.",
#'     "Other people know where I stand on controversial issues.",
#'     "I do not emphasize my own point of view at the expense of others.",
#'     "I rarely present a “false” front to others.",
#'     "I accept the feelings I have about myself.",
#'     "My morals guide what I do as a leader.",
#'     "I listen very carefully to the ideas of others before making decisions.",
#'     "I admit my mistakes to others."
#'   ))%>%tibble()
#' Authentic_Leadership
#' # 데이터프레임 번역
#' Alp_ko = k_df(Authentic_Leadership,
#'               range = 2:3,
#'               slang = "en",
#'               elang = "ko") %>%select(3,2)
#' Alp_ko
#' Alp_ko %>%replace_df_rep("받아들입니다.","받아들인다.", "압니다","안다")
#' #'
#'
#'
#'
#'
#'
#' }
#'
#'
#'
#'
k_df <- function(df, range =1:ncol(df),
                 slang = "en", elang = "ko") {
  if (is.character(range)) {
    cols <- unlist(strsplit(range, ":"))
    col_indices <- which(names(df) %in% cols)
  } else {
    col_indices <- range
  }

  df_translated <- df

  for (col in col_indices) {
    translated_col <- sapply(df[[col]], k, slang = slang, elang = elang, show = "data")
    df_translated[[col]] <- translated_col
  }

  return(df_translated)
}
