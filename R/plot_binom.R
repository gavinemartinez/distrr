#'Plot the Binomial Distribution
#'
#' @param n number of trials
#' @param p probability of success
#' @param level confidence level
#'
#' @return A plot of the binomial distribution with user-specified details
#'
#' @import ggplot2
#' @import dplyr
#' @import ggtext
#'
#' @export
plot_binom <- function(n, p, level = .95){
  df <- data.frame(successes = 0:n, density = dbinom(0:n, n, p))

  # Calculate critical values and confidence interval
  q <- critical_values_binom(level, n, p)
  ci <- get_ci_binom(q, n, p)
  annotate_x <- get_annotation_location(n, p)

 crit_plot <- crit_plot(df, level, n, p, q, ci, annotate_x)
 non_crit_plot <- non_crit_plot(df, level, n, p, q, ci, annotate_x)
 result <- list("Input" = data.frame("n" = n,
                                     "p" = p,
                                     "Confidence" = level),
                "Critical Values" = data.frame("Lower" = q[1], "Upper" = q[2]),
                "Confidence Bounds" = data.frame("Lower" = ci[1], "Upper" = ci[2]),
                "Critical Value Plot" = crit_plot,
                "Non-Critical Value Plot" = non_crit_plot
                )
 return(result)
}

#'Make a vector of quantiles for the critical values of a binomial distribution
#'
#' @param level confidence level
#' @param n number of trials
#' @param p probability of success
#'
#' @return A vector of length 2 containing the upper and lower critical values
#'
#'
#' @export
critical_values_binom <- function(level, n , p){
  qs <- qbinom(c((level + ((1-level)/2)) - level, level + ((1-level)/2)), n, p)
  return(qs)
}

#'Make a vector of confidence interval bounds
#'
#' @param q vector containing critical values
#' @param n number of trials
#' @param p probability of success
#'
#' @return A vector of length 2 containing the upper and lower confidence bounds
#'
#'
#' @export
get_ci_binom <- function(q, n , p){
  ci <- c(pbinom(q[1], n, p), pbinom(q[2], n, p))
  return(ci)
}

#'Get x-coordinate for annotation location on plot
#'
#' @param p probability of success
#' @param n number of trials
#'
#' @return a value
get_annotation_location <- function(n, p){
  annotate_x <- if(p >= .5){
    annotate_x <- (n - n*.75)
  }
  if(p < .5){
    annotate_x <- (n - n*.25)
  }
  return(annotate_x)
}


#'Create Binomial Plot with Critical Values
#'
#' @param df dataframe
#' @param level confidence level specified by user
#' @param n number of trials
#' @param p probability of success
#' @param q critical values
#' @param ci confidence bounds
#' @param annotate_x x-coordinate for the annotation of n, c, p, and the confidence interval
#'
#' @return a plot displaying critical values
crit_plot <- function(df, level, n, p, q, ci, annotate_x){
  crit <- ggplot(df, mapping = aes(x = successes, y = density)) +
    geom_line() +
    geom_vline(xintercept = q, linetype = "dashed") +
    geom_vline(xintercept = c(q[1], q[2]), color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    annotate("text", x = q[1] - n*.05, y = 0.03, label = round(q[1], 2), col = "red") +
    annotate("text", x = q[2] + n*.05, y = 0.03, label = round(q[2], 2), col = "red") +
    annotate("text", x = annotate_x, y = .14, label = paste0(glue::glue("{level*100}%"), "CI for p: [", round(ci[1], 2), ", ", round(ci[2], 2), "]"), col = "black", fontface = "bold") +
    annotate("text", x = annotate_x, y = .169, label = glue::glue("n = {n}"), col = "black", fontface = "bold") +
    annotate("text", x = annotate_x, y = .159, label = glue::glue("p = {p}"), col = "black", fontface = "bold") +
    geom_ribbon(data = df %>% filter(successes >= q[1] & successes <= q[2]),
                aes(ymin = 0, ymax = density, x = successes),
                fill = "gold", alpha = 0.5) +
    labs(title = "Binomial Distribution",
         x = "Number of Successes",
         y = "Probability Density") +
    theme_classic()
  return(crit)
}

#'Create Binomial Plot with Critical Values
#'
#' @param df dataframe
#' @param level confidence level specified by user
#' @param n number of trials
#' @param p probability of success
#' @param q critical values
#' @param ci confidence bounds
#' @param annotate_x x-coordinate for the annotation of n, c, p, and the confidence interval
#'
#' @return a plot without critical values displayed
non_crit_plot <- function(df, level, n, p, q, ci, annotate_x){
  non_crit <- ggplot(df, mapping = aes(x = successes, y = density)) +
    geom_line() +
    geom_vline(xintercept = q, linetype = "dashed") +
    geom_vline(xintercept = c(q[1], q[2]), color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    annotate("text", x = annotate_x, y = .14, label = paste0(glue::glue("{level*100}%"), "CI for p: [", round(ci[1], 2), ", ", round(ci[2], 2), "]"), col = "black", fontface = "bold") +
    annotate("text", x = annotate_x, y = .169, label = glue::glue("n = {n}"), col = "black", fontface = "bold") +
    annotate("text", x = annotate_x, y = .159, label = glue::glue("p = {p}"), col = "black", fontface = "bold") +
    geom_ribbon(data = df %>% filter(successes >= q[1] & successes <= q[2]),
                aes(ymin = 0, ymax = density, x = successes),
                fill = "gold", alpha = 0.5) +
    labs(title = "Binomial Distribution",
         x = "Number of Successes",
         y = "Probability Density") +
    theme_classic()
  return(non_crit)
}
