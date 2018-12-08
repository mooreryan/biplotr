context("Biplot")
library(ggplot2)
library(gridExtra)
library(biplotr)

test_that("biplot returns a ggplot thing", {
  dat <- matrix(c(1, 2, 3,
                  4, 5, 6,
                  7, 8, 9),
                nrow = 3,
                ncol = 3,
                byrow = TRUE)
  rownames(dat) <- c("apple", "orange", "pear")
  colnames(dat) <- c("price", "amt", "coolness")

  chart <- biplotr::biplot(dat)
  grid.arrange(chart)

  expect_is(chart, "gg")
})
