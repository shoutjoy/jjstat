#' mysummary_apa: Generate descriptive statistics in APA style (English or Korean)
#'
#' @param df A data frame containing descriptive statistics, including variables such as N, MEAN, SD, Skew, and Kurt.
#' @param sd A logical value. If TRUE, includes standard deviation (SD) along with the mean in the output. Default is FALSE.
#' @param eng A logical value. If TRUE, the output is in English. If FALSE, the output is in Korean. Default is TRUE.
#'
#' @return A string formatted in APA style (English or Korean), summarizing the descriptive statistics in order of mean values and assessing normality based on skewness and kurtosis.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   var = c("Q33_1", "Q33_2", "Q33_3"),
#'   N = c(4542, 4542, 4542),
#'   MEAN = c(1.9489, 1.9337, 1.9243),
#'   SD = c(0.84679, 0.86568, 0.86960),
#'   Skew = c(0.72576, 0.63900, 0.70717),
#'   Kurt = c(0.63369, 0.087763, 0.27470)
#' )
#' mysummary_apa(df, sd = TRUE, eng = TRUE)
#' mysummary_apa(df, sd = TRUE, eng = FALSE)
#' }
mysummary_apa <- function(df, sd = FALSE, eng = TRUE) {
  # Order variables by mean
  df <- df[order(-df$MEAN), ]

  # Extract mean (and optionally SD)
  if (sd) {
    mean_desc <- paste0(df$var, "(M=", sprintf("%.4f", df$MEAN),
                        ", SD=", sprintf("%.5f", df$SD), ")")
  } else {
    mean_desc <- paste0(df$var, "(M=", sprintf("%.4f", df$MEAN), ")")
  }

  # Create ordered string of means
  mean_order <- paste(mean_desc, collapse = if (eng) " > " else " > ")

  # Check skewness and kurtosis
  skew_exceed <- df$var[abs(df$Skew) > 3]
  kurt_exceed <- df$var[abs(df$Kurt) > 8]

  # Assess normality
  if (length(skew_exceed) == 0 && length(kurt_exceed) == 0) {
    normality <- if (eng) {
      "Skewness and kurtosis met the criteria for normality."
    } else {
      "왜도와 첨도는 정규성을 만족하였다."
    }
  } else {
    skew_info <- if (length(skew_exceed) > 0) {
      paste0(skew_exceed, "(skew=", sprintf("%.5f", df$Skew[df$var %in% skew_exceed]), ")")
    } else {
      NULL
    }
    kurt_info <- if (length(kurt_exceed) > 0) {
      paste0(kurt_exceed, "(kurt=", sprintf("%.5f", df$Kurt[df$var %in% kurt_exceed]), ")")
    } else {
      NULL
    }
    normality <- if (eng) {
      paste("Variables that did not meet normality:",
            paste(c(skew_info, kurt_info), collapse = ", "))
    } else {
      paste("정규성을 만족하지 않는 변수:",
            paste(c(skew_info, kurt_info), collapse = ", "))
    }
  }

  # Combine results
  skew_range <- paste0(sprintf("%.5f", range(df$Skew)), collapse = "~")
  kurt_range <- paste0(sprintf("%.5f", range(df$Kurt)), collapse = "~")

  if (eng) {
    result <- paste0(
      "The results of the descriptive statistics analysis are as follows. Variables ordered by mean are ",
      mean_order, ". Skewness ranged from ", skew_range, ", and kurtosis ranged from ", kurt_range,
      ". Following Kline (2005), skewness values exceeding an absolute value of 3 and kurtosis values exceeding an absolute value of 8 or 10 are considered non-normal. ",
      normality
    )
  } else {
    result <- paste0(
      "기술통계분석결과는 다음과 같다. 평균의 크기순으로 변수를 나열하면, ",
      mean_order, " 순으로 나타났다. 왜도(skewness)는 ", skew_range, " 사이에 존재하며, ",
      "첨도(kurtosis)는 ", kurt_range, "였다. 왜도의 기준을 절대값 3을 초과하지 않고, 첨도는 절대값 8 또는 10을 초과하지 않으면 정규분포로 볼 수 있다(Kline, 2005). ",
      normality
    )
  }

  return(result)
}



#' mean_apa: Generate descriptive statistics in APA style
#'
#' @param df A data frame containing descriptive statistics, including variables such as N, MEAN, SD, Skew, and Kurt.
#' @param sd A logical value. If TRUE, includes standard deviation (SD) along with the mean in the output. Default is FALSE.
#'
#' @return A string formatted in APA style, summarizing the descriptive statistics in order of mean values and assessing normality based on skewness and kurtosis.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   var = c("Q33_1", "Q33_2", "Q33_3"),
#'   N = c(4542, 4542, 4542),
#'   MEAN = c(1.9489, 1.9337, 1.9243),
#'   SD = c(0.84679, 0.86568, 0.86960),
#'   Skew = c(0.72576, 0.63900, 0.70717),
#'   Kurt = c(0.63369, 0.087763, 0.27470)
#' )
#' mean_apa(df, sd = TRUE)
#'
#' # Example Output:
#' # "The results of the descriptive statistics analysis are as follows.
#' # Variables ordered by mean are Q33_1 (M=1.9489, SD=0.84679) > Q33_2 (M=1.9337, SD=0.86568) >
#' # Q33_3 (M=1.9243, SD=0.86960). Skewness ranged from 0.63900 to 0.72576, and kurtosis ranged from
#' # 0.087763 to 0.63369. Following Kline (2005), skewness values exceeding an absolute value of 3
#' # and kurtosis values exceeding an absolute value of 8 or 10 are considered non-normal. Skewness
#' # and kurtosis met the criteria for normality."
mean_apa <- function(df, sd = FALSE) {
  # 평균을 기준으로 변수 정렬
  df <- df[order(-df$MEAN), ]

  # 변수와 평균 및 (옵션에 따라 표준편차) 추출
  if (sd) {
    mean_desc <- paste0(df$var, "(M=", sprintf("%.4f", df$MEAN),
                        ", SD=", sprintf("%.5f", df$SD), ")")
  } else {
    mean_desc <- paste0(df$var, "(M=", sprintf("%.4f", df$MEAN), ")")
  }

  # 평균 순으로 나열
  mean_order <- paste(mean_desc, collapse = " > ")

  # 왜도와 첨도 검토
  skew_exceed <- df$var[abs(df$Skew) > 3]
  kurt_exceed <- df$var[abs(df$Kurt) > 8]

  # 정규성 평가 결과
  if (length(skew_exceed) == 0 && length(kurt_exceed) == 0) {
    normality <- "왜도와 첨도는 정규성을 만족하였다."
  } else {
    skew_info <- if (length(skew_exceed) > 0) {
      paste0(skew_exceed, "(skew=", sprintf("%.5f", df$Skew[df$var %in% skew_exceed]), ")")
    } else {
      NULL
    }
    kurt_info <- if (length(kurt_exceed) > 0) {
      paste0(kurt_exceed, "(kurt=", sprintf("%.5f", df$Kurt[df$var %in% kurt_exceed]), ")")
    } else {
      NULL
    }
    normality <- paste("정규성을 만족하지 않는 변수:",
                       paste(c(skew_info, kurt_info), collapse = ", "))
  }

  # 결과 조합
  skew_range <- paste0(sprintf("%.5f", range(df$Skew)), collapse = "~")
  kurt_range <- paste0(sprintf("%.5f", range(df$Kurt)), collapse = "~")
  result <- paste0(
    "기술통계분석결과는 다음과 같다. 평균의 크기순으로 변수를 나열하면, ",
    mean_order, " 순으로 나타났다. ",
    "왜도(skewness)는 ", skew_range, " 사이에 존재하며, ",
    "첨도(kurtosis)는 ", kurt_range, "였다. ",
    "왜도의 기준을 절대값 3을 초과하지 않고, 첨도는 절대값 8 또는 10을 초과하지 않으면 정규분포로 볼 수 있다(Kline, 2005). ",
    normality
  )

  return(cat("\n",result,"\n\n"))
}
