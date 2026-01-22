#' item_parceling: Mutate-like Item Parceling Function
#'
#' This function creates composite (parcel) variables in a data.frame
#' similar to `dplyr::mutate()`. The left-hand side specifies the name
#' of the new parcel variable, and the right-hand side specifies the
#' columns to be aggregated.
#'
#' @param data A data.frame.
#' @param ... Named arguments. The name is the new parcel variable,
#'   and the value specifies columns using names, indices, or ranges.
#' @param fun A function to apply across selected columns (default = mean).
#' @param type Output type:
#'   \describe{
#'     \item{data}{Return data.frame with parcel variables (default).}
#'     \item{res}{Same as data.}
#'     \item{new}{Return only newly created parcel variables.}
#'     \item{cols_check}{Return columns used for each parcel.}
#'     \item{all}{Return list(data, cols_check).}
#'   }
#'
#' @return A data.frame or list depending on `type`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ############################################################
#' ## Part 1. mtcars examples
#' ############################################################
#'
#' ## 1. Create parcels and return full data
#' item_parceling(
#'   mtcars,
#'   Mean_col1 = c("mpg", "cyl"),
#'   Mean_col2 = c("disp", "hp")
#' )
#'
#'
#' ## 2. Return only newly created parcel variables
#' item_parceling(
#'   mtcars,
#'   Mean_col1 = c("mpg", "cyl"),
#'   Mean_col2 = c("disp", "hp"),
#'   type = "new"
#' )
#'
#'
#' ############################################################
#' ## Part 2. stat_onl examples (survey data)
#' ############################################################
#'
#' ## 3. Satisfaction-related parcels
#' stat_onl <- item_parceling(
#'   stat_onl,
#'   Satisfy_Mean = c("satisfy", "On_Satisfy", "On_Joy"),
#'   Learning_Engage = c("learning:S_Focus_on"),
#'   fun  = mean,
#'   type = "data"
#' )
#'
#'
#' ## 4. Extract only parcel scores for SEM / CFA
#' parcel_scores <- item_parceling(
#'   stat_onl,
#'   Satisfy_Mean = c("satisfy", "On_Satisfy", "On_Joy"),
#'   Learning_Engage = c("learning:S_Focus_on"),
#'   Online_Exp = c("On_Easy:Intension_use"),
#'   type = "new"
#' )
#'
#' }
item_parceling <- function(data, ..., fun = mean, type = "data") {

  terms <- list(...)
  parcel_names <- names(terms)

  if (is.null(parcel_names) || any(parcel_names == "")) {
    stop("All parcel specifications must be named (e.g., NewVar = c(\"x:y\")).")
  }

  cols_check <- list()
  new_data <- data.frame(row.names = rownames(data))

  for (i in seq_along(terms)) {

    cols <- terms[[i]]

    ## Numeric index selection
    if (is.numeric(cols)) {
      cols <- colnames(data)[cols]
    }

    ## Range selection (e.g., "a:b")
    if (is.character(cols) && length(cols) == 1 && grepl(":", cols)) {
      rng <- strsplit(cols, ":")[[1]]
      cols <- names(data)[
        which(names(data) == rng[1]) :
          which(names(data) == rng[2])
      ]
    }

    cols_check[[parcel_names[i]]] <- cols

    new_data[[parcel_names[i]]] <-
      apply(data[cols], 1, fun, na.rm = TRUE)

    data[[parcel_names[i]]] <- new_data[[parcel_names[i]]]
  }

  res <- list(
    data = data,
    new  = new_data,
    cols_check = cols_check
  )

  switch(
    type,
    data       = data,
    res        = data,
    new        = new_data,
    cols_check = cols_check,
    all        = res
  )
}
