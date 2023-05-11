test_that("poisson_process works", {
  plot1 <- poisson_process("poisson", .3, level = .8)
  plot2 <- poisson_process("exponential", .3, level = .98)
  plot3 <- poisson_process("gamma", rate = .6, level = .5)

  expect_equal(is.ggplot(plot1), TRUE)
  expect_equal(is.ggplot(plot2), TRUE)
  expect_equal(is.ggplot(plot3), TRUE)
})

test_that("poisson_process labels work", {
  plot1 <- poisson_process("poisson", .3, level = .8)
  plot2 <- poisson_process("exponential", .3, level = .98)
  plot3 <- poisson_process("gamma", rate = .6, level = .5)

  expect_equal(plot1$labels$title, "Poisson Distribution")
  expect_equal(plot1$labels$x, "Number Of Occurrences")
  expect_equal(plot1$labels$y, "Probability Density")

  expect_equal(plot2$labels$title, "Exponential Distribution")
  expect_equal(plot2$labels$x, "Time Until Occurrence")
  expect_equal(plot2$labels$y, "Probability Density")

  expect_equal(plot3$labels$title, "Gamma Distribution")
  expect_equal(plot3$labels$x, "Time Between Occurrences")
  expect_equal(plot3$labels$y, "Probability Density")
})
