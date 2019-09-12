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

  expect_is(chart$biplot, "gg")
})

describe("data subsetting", {
  describe("the user can specify which data columns to keep", {
    it("keeps all columns by default", {

      # Explicitly state the data columns.
      expected <- biplotr::pca_biplot(
        data = iris[, 1:4],
        data_cols = 1:4
      )$pca

      # data_cols defaults to all columsn
      actual <- biplotr::pca_biplot(
        data = iris[, 1:4]
      )$pca

      expect_equal(actual, expected)
    })

    it("handles simple ranges", {
      expected <- biplotr::pca_biplot(
        data = iris[, 1:3]
      )$pca

      actual <- biplotr::pca_biplot(
        data = iris,
        data_cols = 1:3,
      )$pca

      expect_equal(actual, expected)
    })


    it("handles skips in sequential ranges", {
      expected <- biplotr::pca_biplot(
        data = iris[, c(1:2, 4)]
      )$pca

      actual <- biplotr::pca_biplot(
        data = iris,
        data_cols = c(1:2, 4)
      )$pca

      expect_equal(actual, expected)
    })

    it("keeps the original data column order regardless of data_cols ordering", {
      expected <- biplotr::pca_biplot(
        data = iris[, 1:4]
      )$pca

      actual <- biplotr::pca_biplot(
        data = iris,
        data_cols = c(2, 1, 4, 3) # specifying columns in a weird order
      )$pca

      expect_equal(actual, expected)
    })
  })
})

describe("user provides data which isn't all numeric", {
  it("raises an error and gives a decent message", {
    expect_error(
      pca_biplot(iris),
      regexp = "all_columns_are_numeric are not all TRUE",
      fixed = TRUE
    )
  })
})

describe("coloring data points by group", {
  describe("things that raise errors", {
    it("raises an error if the point_color column name doesn't exist", {
      expect_error(
        pca_biplot(iris,
                   data_cols = 1:4,
                   point_color = "apple_pie"),
        regexp = "point_color column was not present in the data"
      )
    })
  })
})

# Uncomment this section to check on the look of the charts.
describe("coloring data points by group", {
  it("makes a biplot colored by a group", {
    p <- pca_biplot(iris,
                    arrows = TRUE,
                    point_color = "Species",
                    arrow_labels = TRUE,
                    arrow_labels_nudge_y = 0.3,
                    limits_nudge_x = 1,
                    limits_nudge_y = -1)

    pdf("haha.pdf")
    print(p$biplot)
    dev.off()
  })
})
