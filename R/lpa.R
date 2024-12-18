#' Latent Profile Analysis (LPA) with Visualization
#'
#' @param df A data frame containing numeric variables for latent profile analysis.
#' @param profiles Integer. Number of latent profiles to identify. Default is 4.
#' @param modelNames Character. Model name to use in Mclust for Gaussian mixture modeling. Default is "EII".
#' @param g_type Character vector. Types of plots to generate for visualization (e.g., "uncertainty", "classification"). Default is c("uncertainty", "classification").
#'
#' @return A list containing the following elements:
#'   \item{result}{Parameters of the Gaussian mixture model.}
#'   \item{lpa_reuslt}{A data frame summarizing profile means and counts.}
#'   \item{graph}{Plot of the GMM results based on g_type.}
#'   \item{profile_plot}{A bar plot visualizing profile frequencies.}
#'   \item{table}{Markdown-styled table summarizing profile results.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- iris[, 1:4]
#' result <- lpa(data, profiles = 3, modelNames = "VII")
#' print(result$table)
#' }
lpa <- function(df, profiles = 4, modelNames = "EII",
                g_type = c("uncertainty", "classification")) {
  set.seed(123)

  # Fit the Gaussian mixture model
  gmm_models <- Mclust(df, G = profiles, modelNames = modelNames)
  lpa <- gmm_models$parameters

  # Frequency table of profiles
  profiles_n <- table(gmm_models$classification)
  profiles_classification <- Freq_table(gmm_models$classification, prop = TRUE)

  # Add profile names and visualize frequency table
  profiles_classification <- profiles_classification %>%
    mutate(term = paste0("Profile_", 1:nrow(profiles_classification)))
  graph_profiles <- table_df_bar(profiles_classification, x.title = "")

  # Summarize results
  resdata <- lpa$mean
  lpa_reuslt <- rbind(profiles_n, freq = lpa$mean)

  # Rename columns for clarity
  colnames(lpa_reuslt) <- c(paste0("profile_", 1:ncol(lpa_reuslt)))
  lpa_reuslt <- lpa_reuslt %>% row2col("Var")

  # Generate visualization of GMM
  g <- plot(gmm_models, what = g_type)

  # Prepare results for output
  res <- list(
    result = lpa,
    lpa_reuslt = lpa_reuslt,
    graph = g,
    profile_plot = graph_profiles,
    table = md(lpa_reuslt)
  )

  return(res)
}
