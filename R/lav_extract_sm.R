#' lav_extract_sm, Extracting a Structural Path Model from a Model
#'
#' @param model lavaan model
#' @param hypo hypothesis, default TRUE
#' @param prefix "a"
#' @param all all paths
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Example usage
#' model1 <- "
#' IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5
#' EXPE =~ expe1 + expe2 + expe3 + expe4 + expe5
#' QUAL =~ qual1 + qual2 + qual3 + qual4 + qual5
#' VAL =~ val1 + val2 + val3 + val4
#' SAT =~ sat1 + sat2 + sat3 + sat4
#' LOY =~ loy1 + loy2 + loy3 + loy4
#'
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~  QUAL+ EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#'
#'
#' lav_extract_sm(model1)%>%cat("\n")
#' lav_extract_sm(model1, hypo=FALSE)%>%cat("\n")
#' lav_extract_sm(model1, prefix="lamda")%>%cat("\n")
#' lav_extract_sm(model1, prefix="H")%>%cat("\n")
#' lav_extract_sm(model1, all=FALSE) %>%cat("\n")
#' lav_extract_sm(model1, all=TRUE) %>%cat("\n")
#' lav_extract_sm(model1,FALSE) %>%cat("\n")
#'
#'
#' lav_extract_sm(model1, all=TRUE) %>%
#'   diagram_model(sizeLat=6)
#' #'
#'
#' }
#'
#'
lav_extract_sm <- function(model, hypo = TRUE, prefix = "a", all = FALSE) {
  mm_model <- lav_extract_mm(model)

  # Split the model string by newline to separate each line
  lines <- strsplit(model, "\n")[[1]]

  # Use regular expression to find lines that indicate paths (contain ~ but not =~ or ~~)
  path_lines <- grep("^[^~=]*~[^~=]*$", lines, value = TRUE)

  path_lines_expanded <- c()

  for (line in path_lines) {
    # Split the line by '~' to separate the dependent and independent variables
    parts <- strsplit(line, "~")[[1]]
    dependent <- trimws(parts[1])
    independents <- trimws(parts[2])

    # Split the independent variables by '+' and trim whitespace
    independents <- unlist(strsplit(independents, "\\+"))
    independents <- trimws(independents)

    # Create new lines without identifiers
    for (independent in independents) {
      new_line <- paste(dependent, "~", independent)
      path_lines_expanded <- c(path_lines_expanded, new_line)
    }
  }

  # Check if hypothesis identifiers are already present
  has_hypo <- any(grepl("\\*", path_lines_expanded))

  # If hypo is TRUE and no hypothesis identifiers are present, add identifiers
  if (hypo && !has_hypo) {
    for (i in seq_along(path_lines_expanded)) {
      parts <- strsplit(path_lines_expanded[i], "~")[[1]]
      dependent <- parts[1]
      independent <- parts[2]

      # Add the identifier
      path_lines_expanded[i] <- paste(dependent, "~", paste0(prefix, i, "*", independent))
    }
  }

  # Return the lines that contain paths as a single string
  res <- paste0(path_lines_expanded, collapse = "\n")

  if (all) {
    all <- paste(mm_model, res, sep = "\n")
  } else {
    res
  }
}
