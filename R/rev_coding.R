
#' Reverse Coding for Likert Scale Items
#'
#' This function performs reverse coding on a numeric vector, commonly used in Likert scale data.
#' The function assumes the Likert scale ranges from 1 to (n - 1), where `n` is typically 6 for a 5-point scale.
#'
#' @param vec Numeric vector to be reverse-coded.
#' @param n Integer value indicating the maximum Likert scale value plus 1 (e.g., use `n = 6` for 1 to 5 scale).
#' @param check Logical; if TRUE, returns a data frame showing the original and reverse-coded values side-by-side.
#'
#' @return A reverse-coded vector or a data frame if `check = TRUE`. Returns an error message if input is not a vector
#'         or values exceed the expected maximum.
#' @export
#'
#' @examples
#' # Example 1: Basic reverse coding (Likert 1~5, n=6)
#' x <- c(1, 2, 3, 4, 5)
#' rev_coding(x)
#'
#' # Example 2: View original and reverse-coded values
#' rev_coding(x, check = TRUE)
#'
#' # Example 3: 7-point Likert scale (1~7), use n = 8
#' x7 <- c(1, 3, 5, 7)
#' rev_coding(x7, n = 8)
#'
#' # Example 4: Using data from mtcars - reverse code 'gear' variable
#' rev_coding(mtcars$gear, n = 6)
#'
#' # Example 5: Check = TRUE for inspection
#' rev_coding(mtcars$gear, check = TRUE)
#'
#' # Example 6: Using 'cyl' column from mtcars (values 4, 6, 8), Likert scale mismatch
#' rev_coding(mtcars$cyl, n = 6)
#'
#' # Example 7: Using 'Sepal.Length' from iris (continuous values), inappropriate for Likert
#' rev_coding(iris$Sepal.Length, n = 6)
#'
#' # Example 8: Using ToothGrowth dataset - 'dose' column
#' rev_coding(ToothGrowth$dose, n = 6)
#'
#' # Example 9: Inputting a data frame (error case)
#' rev_coding(mtcars)
#'
#' # Example 10: Reverse coding with missing values
#' x_miss <- c(1, 2, NA, 4, 5)
#' rev_coding(x_miss, n = 6)
#'
#' # Example 11: Reverse coding with missing values and check = TRUE
#' rev_coding(x_miss, n = 6, check = TRUE)
rev_coding = function(vec,
                      n = 6,
                      check = FALSE){
  if (is.vector(vec)) {
    if (max(vec, na.rm = TRUE) < n) {
      vec_r <- ifelse(is.na(vec), NA, n - vec)
    } else {
      vec_r <- "Please check the value again. It does not match the value to be decoded. Please check the scale again. For example, enter n=6 for 1~5 Likert and n=8 for 1~7 Likert"
    }
  } else {
    vec_r <- "You can only input vectors for inverse coding. If you have entered a data frame, please find the variable and enter it correctly."
  }

  if (check && is.numeric(vec) && is.numeric(vec_r)) {
    res <- data.frame(input = vec, direct = "->", reverse = vec_r)
  } else {
    res <- vec_r
  }

  return(res)
}

#'
#' #' REversse codings
#' #'
#' #' @param vec input data
#' #' @param n   likert max+1
#' #' @param check  View the original source and reverse coded results
#' #'
#' #' @return reverse coding data
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' ## error  :Error message output when data frames are entered
#' #' rev_coding(mtcars)
#' #'
#' #' #reverse coding
#' #' rev_coding(mtcars$gear)
#' #'
#' #' ## View the original source and decoded results
#' #' rev_coding(mtcars$gear, check=TRUE)
#' #'
#' #' ###If the Likert scale is wrong, the highest scale is 5, but the highest scale is 8.
#' #' rev_coding(mtcars$cyl)
#' #' }
#' rev_coding = function(vec,
#'                       n = 6,
#'                       check = FALSE){
#'   if(is.vector(vec)){
#'     if(max(vec) < n){
#'       vec_r = n - vec
#'     }else{
#'       vec_r = "Please check the value again. It does not match the value to be decoded. Please check the scale again. For example, enter n=6 for 1~5 Likert and n=8 for 1~7 Likert"
#'     }
#'   }else{
#'     vec_r = "You can only input vectors for inverse coding. If you have entered a data frame, please find the variable and enter it correctly."
#'   }
#'   if(check){
#'     res = data.frame(input= vec, direct="->",reverse= vec_r)
#'   }else{
#'     res = vec_r
#'   }
#'   res
#' }


