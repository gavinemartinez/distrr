#'Plot the Normal Distribution
#'
#' @param mu population mean
#' @param sigma population standard deviation
#' @param level confidence level
#'
#' @return A plot of the normal distribution with user-specified details
#'
#' @import ggplot2
#' @import dplyr
#' @import ggtext
#'
#' @export
plot_normal <- function(mu, sigma, level = .95){
  df <- make_normal_df(mu, sigma)

  # Calculate critical values and confidence interval
  q <- critical_values_norm(level, mu, sigma)
  ci <- get_ci_norm(mu, sigma, q)

  annotate_x <- mu - mu*.1

  crit_plot <- crit_plot_norm(df, level, mu, sigma, q, ci, annotate_x)
  non_crit_plot <- non_crit_plot_norm(df, level, mu, sigma, q, ci, annotate_x)
  non_crit_non_ann <- non_crit_non_ann(df, level, mu, sigma, q, ci)
  crit_non_ann <- crit_non_ann(df, level, mu, sigma, q, ci)

  result <- return_list(df,
                        level,
                        q,
                        ci,
                        mu,
                        sigma,
                        crit_plot,
                        crit_non_ann,
                        non_crit_plot,
                        non_crit_non_ann)
  return(result)

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


#'Get the confidence bounds of normal distrbution
#'
#' @param mu population mean
#' @param sigma population standard deviation
#' @param q quantiles
#'
#' @return a vector of the lower and upper bounds
#'
#'
get_ci_norm <- function(mu , sigma, q){
  ci <- c(pnorm(q[1], mu, sigma), pnorm(q[2], mu, sigma))
  return(ci)
}

#'Get a normal distribution displaying critical values
#'
#' @param df dataframe
#' @param level confidence level
#' @param mu population mean
#' @param sigma population standard deviation
#' @param q quantiles
#' @param ci confidence bounds
#' @param annotate_x x-coordinate for annotation
#'
#' @return a plot displaying critical values
crit_plot_norm <- function(df, level, mu, sigma, q, ci, annotate_x){
  crit <- ggplot(df, mapping = aes(x = quantiles, y = density)) +
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
  return(crit)
}


#'Get a normal distribution without critical values
#'
#' @param df dataframe
#' @param level confidence level
#' @param mu population mean
#' @param sigma population standard deviation
#' @param q quantiles
#' @param ci confidence bounds
#' @param annotate_x x-coordinate for annotation
#'
#' @return a plot displaying critical values
non_crit_plot_norm <- function(df, level, mu, sigma, q, ci, annotate_x){
  non_crit <- ggplot(df, mapping = aes(x = quantiles, y = density)) +
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
  return(non_crit)
}


#'Get a normal distribution without critical values and without annotation
#'
#' @param df dataframe
#' @param level confidence level
#' @param mu population mean
#' @param sigma population standard deviation
#' @param q quantiles
#' @param ci confidence bounds
#'
#' @return a plot not displaying critical values and not displaying annotation
non_crit_non_ann <- function(df, level, mu, sigma, q, ci){
  non_crit_non_ann <- ggplot(df, mapping = aes(x = quantiles, y = density)) +
    geom_line() +
    geom_vline(xintercept = q, linetype = "dashed") +
    geom_vline(xintercept = c(q[1], q[2]), color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    geom_ribbon(data = df %>% filter(quantiles >= q[1] & quantiles <= q[2]),
                aes(ymin = 0, ymax = density, x = quantiles),
                fill = "gold", alpha = 0.5) +
    labs(title = "Normal Distribution",
         x = "Quantiles",
         y = "Probability Density") +
    theme_classic()
  return(non_crit_non_ann)
}

#'Get a normal distribution critical values but without annotation
#'
#' @param df dataframe
#' @param level confidence level
#' @param mu population mean
#' @param sigma population standard deviation
#' @param q quantiles
#' @param ci confidence bounds
#'
#' @return a plot displaying critical values
crit_non_ann <- function(df, level, mu, sigma, q, ci){
  crit_non_ann <- ggplot(df, mapping = aes(x = quantiles, y = density)) +
    geom_line() +
    geom_vline(xintercept = q, linetype = "dashed") +
    geom_vline(xintercept = c(q[1], q[2]), color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    annotate("text", x = q[1] - q[1]*.05, y = 0.03, label = round(q[1], 2), col = "red") +
    annotate("text", x = q[2] + q[2]*.05, y = 0.03, label = round(q[2], 2), col = "red") +
    geom_ribbon(data = df %>% filter(quantiles >= q[1] & quantiles <= q[2]),
                aes(ymin = 0, ymax = density, x = quantiles),
                fill = "gold", alpha = 0.5) +
    labs(title = "Normal Distribution",
         x = "Quantiles",
         y = "Probability Density") +
    theme_classic()
  return(crit_non_ann)
}


#'Build result list at the end of plot_normal()
#'
#' @param df data frame
#' @param level confidence level
#' @param q quantiles
#' @param ci confidence bounds
#' @param mu population mean
#' @param sigma population standard deviation
#' @param crit_plot critical value plot with annotation
#' @param crit_non_ann critical value plot without annotation
#' @param non_crit_plot non-critical value plot with annotation
#' @param non_crit_non_ann non-critical value plot without annotation
#'
#' @return a list
return_list <- function(df, level,  q, ci, mu, sigma, crit_plot, crit_non_ann, non_crit_plot, non_crit_non_ann){
  if(sigma >= 2.8){
    result <- list("DF" = df,
                   "Inputs" = data.frame("Mu" = mu,
                                         "Sigma" = sigma,
                                         "Confidence" = level),
                   "Critical Values" = data.frame("Lower" = q[1],
                                                  "Upper" = q[2]),
                   "Confidence Bounds" = data.frame("Lower" = ci[1],
                                                    "Upper" = ci[2]),
                   "Critical Value Plot" = crit_plot,
                   "Non-Critical Value Plot" = non_crit_plot)
    }
    if(sigma < 2.8){
    result <- list("DF" = df,
           "Inputs" = data.frame("Mu" = mu,
                                 "Sigma" = sigma,
                                 "Confidence" = level),
           "Critical Values" = data.frame("Lower" = q[1],
                                          "Upper" = q[2]),
           "Confidence Bounds" = data.frame("Lower" = ci[1],
                                            "Upper" = ci[2]),
           "Critical Value Plot" = crit_non_ann,
           "Non-Critical Value Plot" = non_crit_non_ann)
    }
  return(result)
  }
