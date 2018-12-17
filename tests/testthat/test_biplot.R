context("Biplot")
library(ggplot2)
library(grid)
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

  chart <- biplotr::pca_biplot(dat)

  expect_is(chart, "gg")
})
