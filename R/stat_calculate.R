#' Statistics Calculation
#'
#' This function calculates statistics over all of the DRG codes for average Medicare payments.
#' It can calculate the mean, median, or standard deviation of the DRG codes.
#'
#' @param df a dataframe
#' @param varname a variable name of the payments
#' @param alternative Type of the statistics. Choose from 'mean', 'median', or 'standard deviation'
#'
#' @return A value of the choosen statistics
#' @export
#'
#' @examples
#' stat_calculate(DRG, "Average.Medicare.Payments", alternative = "mean")
stat_calculate <- function(df, varname, alternative) {

  if (alternative == "mean") {
    result <- mean(df[[varname]], na.rm = TRUE)
  }
  else if (alternative == "median") {
    result <- median(df[[varname]], na.rm = TRUE)
  }
  else if (alternative == "standard deviation") {
    result <- sd(df[[varname]], na.rm = TRUE)
  }

  # Validate statistics type argument
  else {
    stop("Invalid alternative. Choose from 'mean', 'median', or 'standard deviation'.")
  }
  return(result)
}
