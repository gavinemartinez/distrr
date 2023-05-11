#'Specify a plot of any part of the poisson process
#'
#'@param dist the specified distribution(Poisson, Exponential, Gamma)
#'@param rate rate paramater AKA rate at which a single event occurs
#'@param shape shape parameter (neccessary for Gamma) AKA number of events waiting to occur
#'@param level confidence level
#'
#'@return a plot of specified distribution
#'@export
poisson_process <- function(dist, rate, shape = 1, level = .95){
  if(dist == "poisson"){
    x <- 0:(1/rate * 7)
    y <- dpois(x, lambda = 1/rate)

    alpha <- 1-level
    lower_crit_value <- qpois(alpha/2, lambda = 1/rate)
    upper_crit_value <- qpois(1 - alpha/2, lambda = 1/rate)

    pois_data <- data.frame(x = x, y = y)
    pois_data$color <- ifelse(pois_data$x >= lower_crit_value & pois_data$x <= upper_crit_value, "gold", "white")

    pois_plot <- ggplot(pois_data, aes(x = x, y = y, fill = color)) +
      geom_bar(stat = "identity", color = "black", alpha = 0.5) +
      scale_fill_manual(values = c("gold", "white")) +
      xlab("Number Of Occurrences") +
      ylab("Probability Density") +
      ggtitle("Poisson Distribution") +

      # Add confidence lines at critical values
      geom_vline(xintercept = lower_crit_value, color = "red") +
      geom_vline(xintercept = upper_crit_value, color = "red") +

      # Annotate the plot with rate parameter and confidence interval
      annotate("text", x = (1/rate * 7) - ((1/rate * 5) * .35), y = 0.12, label = paste("Lambda =", 1/rate), fontface = "bold") +
      annotate("text", x = (1/rate * 7) - ((1/rate * 5) * .35), y = 0.1, label = paste(glue::glue("{level*100}%"), "Confidence Interval for mean= [", lower_crit_value, ", ", upper_crit_value, "]"), size = 3.2, fontface = "bold")+
      theme_classic() +
      theme(legend.position = "none")

    # Display the plot
    return(pois_plot)
  }

  if(dist == "exponential"){
    x = 0:(rate*40)
    y <- dexp(x, rate = rate)
    exp_data <- data.frame(x= x, y = y)

    q <- critical_values_exp(level, rate)

    exp_plot <- ggplot(data = exp_data, aes(x = x, y = y)) +
      geom_line() +
      geom_vline(xintercept = round(q[1]), color = "red") +
      geom_vline(xintercept = round(q[2]), color = "red") +
      geom_hline(yintercept = 0, color = "black") +
      ggtitle("Exponential Distribution") +
      xlab("Time Until Occurrence") +
      ylab("Probability Density") +
      annotate("text", x = ((rate*40) - (rate*40)*.25), y = .1, label = glue::glue("Rate Parameter = {rate}"), fontface = "bold") +
      annotate("text", x = ((rate*40) - (rate*40)*.25), y = .15, label = glue::glue("{level*100}% CI for mean: [{round(q[1], 2)}, {round(q[2], 2)}]"), fontface = "bold") +
      geom_ribbon(data = exp_data %>% filter(x >= round(q[1]) & x <= round(q[2])),
                  aes(ymin = 0, ymax = y, x = x),
                  fill = "gold", alpha = 0.5) +
      theme_classic()
    return(exp_plot)
  }

  if(dist == "gamma"){
    x = 0:(shape*10)
    y <- dgamma(x, rate = rate, shape = shape)
    gamma_data <- data.frame(x= x, y = y)

    q <- critical_values_gamma(level, rate, shape)

    gamma_plot <- ggplot(data = gamma_data, aes(x = x, y = y)) +
      geom_line() +
      geom_vline(xintercept = round(q[1]), color = "red") +
      geom_vline(xintercept = round(q[2]), color = "red") +
      geom_hline(yintercept = 0, color = "black") +
      ggtitle("Gamma Distribution") +
      xlab("Time Between Occurrences") +
      ylab("Probability Density") +
      annotate("text", x = (shape*5) - ((shape*5)*.25), y = .1, label = glue::glue("Rate Parameter = {rate}"), fontface = "bold") +
      annotate("text", x = (shape*5) - ((shape*5)*.25), y = .12, label = glue::glue("Shape Parameter = {shape}"), fontface = "bold") +
      annotate("text", x = (shape*5) - ((shape*5)*.25), y = .14, label = glue::glue("{level*100}% CI for mean: [{round(q[1], 2)}, {round(q[2], 2)}]"), fontface = "bold") +
      geom_ribbon(data = gamma_data %>% filter(x >= round(q[1]) & x <= round(q[2])),
                  aes(ymin = 0, ymax = y, x = x),
                  fill = "gold", alpha = 0.5) +
      theme_classic()
    return(gamma_plot)
  }
}



#'Make a vector of quantiles for the critical values of a exponential distribution
#'
#' @param level confidence level
#' @param rate rate of occurence
#'
#' @return A vector of length 2 containing the upper and lower critical values
#'
#'
#' @export
critical_values_exp <- function(level, rate){
  qs <- qexp(c((level + ((1-level)/2)) - level, level + ((1-level)/2)), rate = rate)
  return(qs)
}


#'Make a vector of quantiles for the critical values of a gamma distribution
#'
#' @param level confidence level
#' @param rate rate of occurence
#' @param shape shape parameter
#'
#' @return A vector of length 2 containing the upper and lower critical values
#'
#'
#' @export
critical_values_gamma <- function(level, rate, shape){
  qs <- qgamma(c((level + ((1-level)/2)) - level, level + ((1-level)/2)), rate = rate, shape = shape)
  return(qs)
}
