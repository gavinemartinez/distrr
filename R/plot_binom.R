#'Plot the Binomial Distribution
#'
#' @param n number of trials
#' @param p probability of success
#' @param level confidence level
#' @param crit (T/F) whether or not you want critical values displayed
#'
#' @return A plot of the binomial distribution with user-specified details
#'
#' @import ggplot2
#' @import dplyr
#' @import ggtext
#'
#' @export
plot_binom <- function(n, p, level = .95, crit = T){
  df <- data.frame(successes = 0:n, density = dbinom(0:n, n, p))

  # Calculate critical values and confidence interval
  q <- qbinom(c((level + ((1-level)/2)) - level, level + ((1-level)/2)), n, p)
  ci <- c(pbinom(q[1], n, p), pbinom(q[2], n, p))
  if(p >= .5){
    annotate_x <- (n - n*.75)
  }
  if(p < .5){
    annotate_x <- (n - n*.25)
  }

 crit_plot <- ggplot(df, mapping = aes(x = successes, y = density)) +
   geom_line() +
   geom_vline(xintercept = q, linetype = "dashed") +
   geom_vline(xintercept = c(q[1], q[2]), color = "red") +
   annotate("text", x = q[1] - n*.05, y = 0.03, label = round(q[1], 2), col = "red") +
   annotate("text", x = q[2] + n*.05, y = 0.03, label = round(q[2], 2), col = "red") +
   annotate("text", x = annotate_x, y = .14, label = paste0("95% CI for p: [", round(ci[1], 2), ", ", round(ci[2], 2), "]"), col = "black", fontface = "bold") +
   annotate("text", x = annotate_x, y = .169, label = glue::glue("n = {n}"), col = "black", fontface = "bold") +
   annotate("text", x = annotate_x, y = .159, label = glue::glue("p = {p}"), col = "black", fontface = "bold") +
   geom_ribbon(data = df %>% filter(successes >= q[1] & successes <= q[2]),
               aes(ymin = 0, ymax = density, x = successes),
               fill = "gold", alpha = 0.5) +
   labs(title = "Binomial Distribution",
        x = "Number of Successes",
        y = "Probability Density")

 non_crit_plot <- ggplot(df, mapping = aes(x = successes, y = density)) +
   geom_line() +
   geom_vline(xintercept = q, linetype = "dashed") +
   geom_vline(xintercept = c(q[1], q[2]), color = "red") +
   annotate("text", x = annotate_x, y = .14, label = paste0("95% CI for p: [", round(ci[1], 2), ", ", round(ci[2], 2), "]"), col = "black", fontface = "bold") +
   annotate("text", x = annotate_x, y = .169, label = glue::glue("n = {n}"), col = "black", fontface = "bold") +
   annotate("text", x = annotate_x, y = .159, label = glue::glue("p = {p}"), col = "black", fontface = "bold") +
   geom_ribbon(data = df %>% filter(successes >= q[1] & successes <= q[2]),
               aes(ymin = 0, ymax = density, x = successes),
               fill = "gold", alpha = 0.5) +
   labs(title = "Binomial Distribution",
        x = "Number of Successes",
        y = "Probability Density")

 if(crit == T){
   return(crit_plot)
 }else{
   return(non_crit_plot)
 }

}
