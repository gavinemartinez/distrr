#'Plot the Normal Distribution
#'
#' @param mu population mean
#' @param sigma population standard deviation
#' @param level confidence level
#' @param crit (T/F) whether or not you want critical values displayed
#'
#' @return A plot of the normal distribution with user-specified details
#'
#' @import ggplot2
#' @import dplyr
#' @import ggtext
#'
#' @export
plot_normal <- function(mu, sigma, level = .95, crit = T){
  df <- make_normal_df(mu, sigma)

  # Calculate critical values and confidence interval
  q <- critical_values_norm(level, mu, sigma)
  ci <- c(pnorm(q[1], mu, sigma), pnorm(q[2], mu, sigma))

  annotate_x <- mu - mu*.1

  crit_plot <- ggplot(df, mapping = aes(x = quantiles, y = density)) +
    geom_line() +
    geom_vline(xintercept = q, linetype = "dashed") +
    geom_vline(xintercept = c(q[1], q[2]), color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    annotate("text", x = q[1] - q[1]*.05, y = 0.03, label = round(q[1], 2), col = "red") +
    annotate("text", x = q[2] + q[2]*.05, y = 0.03, label = round(q[2], 2), col = "red") +
    annotate("text", x = mu, y = .045, label = paste0(glue::glue("{as.numeric(level*100)}%"), "CI for mu: [", round(q[1], 1), ", ", round(q[2], 1), "]"), col = "black", fontface = "bold", size = 3.5) +
    annotate("text", x = mu, y = .055, label = glue::glue("mean = {mu}"), col = "black", fontface = "bold", size = 3.5) +
    annotate("text", x = mu, y = .05, label = glue::glue("sd = {sigma}"), col = "black", fontface = "bold", size = 3.5) +
    geom_ribbon(data = df %>% filter(quantiles >= q[1] & quantiles <= q[2]),
                aes(ymin = 0, ymax = density, x = quantiles),
                fill = "gold", alpha = 0.5) +
    labs(title = "Normal Distribution",
         x = "Quantiles",
         y = "Probability Density") +
    theme_classic()

  non_crit_plot <- ggplot(df, mapping = aes(x = quantiles, y = density)) +
    geom_line() +
    geom_vline(xintercept = q, linetype = "dashed") +
    geom_vline(xintercept = c(q[1], q[2]), color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    annotate("text", x = mu, y = .045, label = paste0(glue::glue("{as.numeric(level*100)}%"), "CI for mu: [", round(q[1], 1), ", ", round(q[2], 1), "]"), col = "black", fontface = "bold", size = 3.5) +
    annotate("text", x = mu, y = .055, label = glue::glue("mean = {mu}"), col = "black", fontface = "bold", size = 3.5) +
    annotate("text", x = mu, y = .05, label = glue::glue("sd = {sigma}"), col = "black", fontface = "bold", size = 3.5) +
    geom_ribbon(data = df %>% filter(quantiles >= q[1] & quantiles <= q[2]),
                aes(ymin = 0, ymax = density, x = quantiles),
                fill = "gold", alpha = 0.5) +
    labs(title = "Normal Distribution",
         x = "Quantiles",
         y = "Probability Density") +
    theme_classic()

  if(crit == T){
    return(crit_plot)
  }else{
    return(non_crit_plot)
  }

}

#'Make a vector of quantiles for the critical values of a normal distribution
#'
#' @param level confidence level
#' @param mu population mean
#' @param sigma population standard deviation
#'
#' @return A vector of length 2 containing the upper and lower critical values
#'
#'
#' @export
critical_values_norm <- function(level, mu , sigma){
  qs <- qnorm(c((level + ((1-level)/2)) - level, level + ((1-level)/2)), mu, sigma)
  return(qs)
}


#'Make a data frame of quantiles and probabilities for a normal distribution
#'
#' @param mu population mean
#' @param sigma population standard deviation
#'
#' @return A data frame containing quantiles and their corresponding probabilities for a given normal distribution
#'
#'
#' @export
make_normal_df <- function(mu , sigma){
  quantiles <- seq((mu-(3*sigma)),(mu+(3*sigma)), by = .01)
  density <- dnorm(quantiles, mean = mu, sd = sigma)

  df <- data.frame(quantiles = quantiles, density = density)
}
