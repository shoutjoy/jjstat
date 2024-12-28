#' lpa_gmm: Latent Profile Analysis using Gaussian Mixture Models
#'
#' This function performs latent profile analysis (LPA) using Gaussian Mixture Models (GMM).
#' It provides model parameters, profile classifications, and visualizations for the analysis.
#'
#' @param df A data frame containing the variables to be analyzed.
#' @param profiles An integer specifying the number of profiles to extract. Default is 4.
#' @param modelNames A character string specifying the GMM covariance model. Default is "EII".
#' @param g_type A character string specifying the type of GMM plot to generate.
#' Choices are "uncertainty" or "classification". Default is "uncertainty".
#'
#' @return A list containing:
#' \describe{
#'   \item{result}{The GMM parameters for the fitted model.}
#'   \item{lpa_reuslt}{A summary table of profile means and frequencies.}
#'   \item{graph}{The GMM plot based on the specified type.}
#'   \item{profile_plot}{A bar plot visualizing profile frequencies.}
#'   \item{table}{A formatted markdown table summarizing profile results.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage of lpa_gmm
#' library(mclust)
#' library(dplyr)
#' library(ggplot2)
#'
#' # Example data
#' set.seed(123)
#' df <- data.frame(x = rnorm(100), y = rnorm(100))
#'
#' # Perform LPA with 3 profiles
#' lpa_results <- lpa_gmm(df, profiles = 3)
#'
#' # View results
#' print(lpa_results$lpa_reuslt)
#' plot(lpa_results$graph)
#' }
lpa_gmm <- function(df, profiles = 4, modelNames = "EII",
                    g_type = c("uncertainty", "classification")) {
  set.seed(123)

  # Fit Gaussian Mixture Model
  gmm_models <- Mclust(df, G = profiles, modelNames = modelNames)
  lpa <- gmm_models$parameters

  # Frequency analysis of profiles
  profiles_n <- table(gmm_models$classification)
  profiles_classification <- Freq_table(gmm_models$classification, prop = TRUE)

  # Add profile names
  profiles_classification <- profiles_classification %>%
    mutate(term = paste0("Profile_", 1:nrow(profiles_classification)))

  # Bar plot for profile frequencies
  graph_profiles <- table_df_bar(profiles_classification, x.title = "")

  # Summary table of results
  resdata <- lpa$mean
  lpa_reuslt <- rbind(profiles_n, freq = lpa$mean)
  colnames(lpa_reuslt) <- c(paste0("profile_", 1:ncol(lpa_reuslt)))
  lpa_reuslt <- lpa_reuslt %>% row2col("Var")

  # GMM plot based on specified type
  g <- plot(gmm_models, what = g_type)

  # Prepare final results
  res <- list(result = lpa,
              lpa_reuslt = lpa_reuslt,
              graph = g,
              profile_plot = graph_profiles,
              table = md(lpa_reuslt))

  return(res)
}
