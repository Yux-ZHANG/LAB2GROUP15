#' Boxplot of Payments by DRG
#'
#'This function produces a boxplot of payments by DRG code.
#'It can plot the average Medicare payments, the average total payment, or the average covered charges.
#'
#' @param df a dataframe
#' @param payment_type a variable name, choose from 'Average.Medicare.Payments', 'Average.Total.Payments', or 'Average.Covered.Charges'
#'
#' @return A boxplot of the choosen payment by DRG code
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#'
#' @examples
#' boxplot_payments_by_DRG(DRG, "Average.Medicare.Payments")
#'
boxplot_payments_by_DRG <- function(df, payment_type) {

  # Validate payment_type argument
  if (!payment_type %in% c("Average.Medicare.Payments", "Average.Total.Payments", "Average.Covered_Charges")) {
    stop("Invalid payment_type. Choose from 'Average.Medicare.Payments', 'Average.Total.Payments', or 'Average.Covered.Charges'.")
  }

  yvalue <- df[[payment_type]]

  # Use ggplot2 to create the boxplot
  p <- ggplot(df, aes(x = DRG.Definition, y = yvalue)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90,size = 7)) +  # This line adjusts the x-axis label font size to 5
    #theme(axis.text.x = element_blank()) +  # This line hides the x-axis labels
    labs(title = paste("Boxplot of", gsub("\\.", " ", payment_type), "by DRG Code"),
         x = "DRG Code",
         y = gsub("\\.", " ", payment_type))

  return(p)
}
