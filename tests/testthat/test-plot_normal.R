test_that("plot_normal works", {
  plot <- plot_normal(50, 3, level = .85, crit = F)

  expect_equal(is.ggplot(plot), TRUE)

  expect_equal(plot$labels$title, "Normal Distribution")
  expect_equal(plot$labels$x, "Quantiles")
  expect_equal(plot$labels$y, "Probability Density")


})
