#' plspm_grp_comp_apa: Generate APA-style Group Comparison Descriptions
#'
#' This function generates APA-style text descriptions for PLS-PM group comparisons.
#'
#' @param df A data frame containing the group comparison results. It should have the columns:
#'   - `comp`: Group comparison identifiers (e.g., "1_vs_2").
#'   - `Paths`: The path being analyzed.
#'   - `t`: The t-value for the comparison.
#'   - `p_value`: The p-value for the comparison.
#'   - `df`: Degrees of freedom for the t-test.
#'   - `sig`: Significance indicator ("yes" if significant, otherwise "no").
#' @param edteck Logical. If `TRUE`, predefined group names are used for educational technology profiles.
#'   If `FALSE`, group names are auto-generated as "그룹1", "그룹2", etc.
#'
#' @return None. The function prints APA-style descriptions for significant group comparisons.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   comp = c("1_vs_2", "2_vs_3"),
#'   Paths = c("Engagement", "Performance"),
#'   t = c(3.21, 2.45),
#'   p_value = c(0.0023, 0.0184),
#'   df = c(45, 50),
#'   sig = c("yes", "no")
#' )
#' plspm_grp_comp_apa(df, edteck = TRUE)
#' }
plspm_grp_comp_apa <- function(df, edteck = TRUE) {
  results <- c()

  # Function to map group names based on 'comp' and 'edteck' flag
  get_group_names <- function(comp, edteck) {
    if (edteck) {
      # Predefined profile group names for edteck = TRUE
      if (comp == "1_vs_2") {
        return(c("Profile-1:적극적 학습자(Active Learners)", "Profile-2:기술 수용자(Tech Adopters)"))
      } else if (comp == "1_vs_3") {
        return(c("Profile-1:적극적 학습자(Active Learners)", "Profile-3:소극적 사용자(Passive Users)"))
      } else if (comp == "1_vs_4") {
        return(c("Profile-1:적극적 학습자(Active Learners)", "Profile-4:기술 회의론자(Tech Skeptics)"))
      } else if (comp == "2_vs_3") {
        return(c("Profile-2:기술 수용자(Tech Adopters)", "Profile-3:소극적 사용자(Passive Users)"))
      } else if (comp == "2_vs_4") {
        return(c("Profile-2:기술 수용자(Tech Adopters)", "Profile-4:기술 회의론자(Tech Skeptics)"))
      } else if (comp == "3_vs_4") {
        return(c("Profile-3:소극적 사용자(Passive Users)", "Profile-4:기술 회의론자(Tech Skeptics)"))
      }
    } else {
      # Auto-generate group names for edteck = FALSE
      groups <- strsplit(comp, "_vs_")[[1]]
      return(c(paste0("그룹", groups[1]), paste0("그룹", groups[2])))
    }
  }

  # Iterate through the data frame and generate descriptions for significant results
  for (i in 1:nrow(df)) {
    if (df$sig[i] == "yes") {
      # Map group names
      group_names <- get_group_names(df$comp[i], edteck)
      class1 <- group_names[1]
      class2 <- group_names[2]

      # Extract relevant values for the description
      path <- df$Paths[i]
      t_value <- round(df$t[i], 1)
      p_value <- round(df$p_value[i], 4)
      df_value <- df$df[i]

      # Construct the APA-style description
      description <- paste(
        class1, "와", class2, "간의", path, "경로에서 그룹 간 차이가 통계적으로 유의하게 나타났다",
        "(t(", df_value, ") =", t_value, ", p =", p_value, ")."
      )
      results <- c(results, description)
    }
  }

  # Combine all results into a single output and print
  results <- paste(results, collapse = "\n")
  cat("\n")
  cat(results)
  cat("\n\n")
}
