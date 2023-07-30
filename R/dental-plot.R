#' Plotting dental features
#'
#' @param .data Data frame containing...
#' @param fill What variable is used for the 'fill' argument. Passed to ggplot().
#' @param notation Currently not in use. May be used to support different notation in the future.
#' @importFrom dplyr filter mutate
#' @import patchwork
#' @export dental_plot

dental_plot <- function(.data, fill, notation){

  upper_order <- c(paste0("t", 18:11), paste0("t", 21:28))
  lower_order <- c(paste0("t", 48:41), paste0("t", 31:38))

  tooth_names <- c("M3", "M2", "M1", "PM2", "PM1", "C1", "I2", "I1")
  #upper_order <- c(tooth_names, rev(tooth_names))
  #lower_order <- upper_order
  tooth_data <- .data %>%
    mutate(tooth = factor(tooth, levels = c(upper_order, lower_order)),
           group = as.factor( {{ fill }} ))

  maxilla <- tooth_data %>%
    filter(region == "maxilla") %>%
    ggplot(aes(x = tooth, fill = group)) +
    geom_bar(position = "fill") +
    scale_x_discrete(position = "top") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          panel.grid = element_blank()) +
    scale_y_reverse()

  mandible <- tooth_data %>%
    filter(region == "mandible") %>%
    ggplot(aes(x = tooth, fill = group)) +
    geom_bar(position = "fill") +
    scale_x_discrete(position = "bottom") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          panel.grid = element_blank())

  #requireNamespace(patchwork)
  maxilla / mandible + plot_layout(guides = "collect")
}
