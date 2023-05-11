

test_that("plot binom returns a plot", {
  plot <- plot_binom(50, .6, .98)
  expect_equal(is.ggplot(plot), TRUE)
})


test_that("plot binom labels", {
  plot <- plot_binom(50, .6, .98)

  expect_equal(plot$labels$title, "Binomial Distribution")
  expect_equal(plot$labels$x, "Number of Successes")
  expect_equal(plot$labels$y, "Probability Density")
})
