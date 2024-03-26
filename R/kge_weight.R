#' generative weight variable, kge(2024)
#'
#' @param df data.frame
#' @param type result res, res1, res2, res3, res4
#' @param pattern pattern = "", If you want something different, you can specify a different pattern.
#' @param remove ubite option
#'
#' @return  add weight
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res")
#' }
kge_weigth = function(df,
                      type= "res3",
                      pattern ="",
                      remove=FALSE){

  df0 = df %>% tibble::tibble()%>%
    mutate(
      w1 = ifelse(A3 == pattern, 0, 1),
      w2 = ifelse(B3 == pattern, 0, 1),
      w3 = ifelse(C3 == pattern, 0, 1)
    ) %>% as.data.frame()

  df00 = df0 %>% tidyr::unite(W , w1:w3, sep="",remove = remove )


  df1 = df00 %>%
    dplyr::mutate(
      weight = dplyr::case_when(
        W == "000" ~ 1,
        W == "001" ~ 2,
        W == "010" ~ 3,
        W == "011" ~ 4,
        W == "100" ~ 5,
        W == "101" ~ 6,
        W == "110" ~ 7,
        W == "111" ~ 8)
    )

  df2 = df1%>%
    dplyr::mutate(
      weigth_comb = dplyr::case_when(
        W == "000" ~ "light-light-light",
        W == "001" ~ "light-light-heavy",
        W == "010" ~ "light-heavy-light",
        W == "011" ~ "light-heavy-heavy",
        W == "100" ~ "heavy-light-light",
        W == "101" ~ "heavy-light-heavy",
        W == "110" ~ "heavy-heavy-light",
        W == "111" ~ "heavy-heavy-heavy")
    )

  df3 = df2 %>% dplyr::select(-weight)


  switch(type,
         res1 = df0,
         res2 = df00,
         res3 = df1,
         res4 = df3,
         res = df2)
}

