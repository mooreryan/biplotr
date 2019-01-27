# BiplotR

Give me data, and I'll give you a snazzy biplot!

A simple little package for creating nice looking PCA biplots.  Just input your data matrix and out pops a beautiful biplot for your data analyzing pleasure!

## Install

You need the `devtools` R package to install.  If you don't have it, install it first.

```
install.packages("devtools")
```

Now you can install `biplotr`!

```
library(devtools)
install_github("mooreryan/biplotr")
```

## R dependencies

When you use `biplotR`, you'll need to make sure to import the libraries that it depends on at the beginning of your R program.  Something like this:

```R
library(ggplot2)
library(grid)
library(ggrepel)
library(biplotr)
```

I use the `gridArrange` function in the example. To use that also add this to the other `library` function calls listed above:

```R
library(gridExtra)
```

## Usage

The main function provided is `pca_biplot`.  It makes PCA biplots with principal component scores and loadings (scaled to fit with the points)!

Here is an example with the included data frame `team_shooting_df`, which has shot data for NBA teams so far this season.

`pca_biplot` takes a matrix.  So we want to convert our data frame to a matrix.  The data we care about starts in the 4th column.

```R
team_shooting_mat <- as.matrix(team_shooting_df[, 4:ncol(team_shooting_df)])
```

We also want to include the team names so our plot looks nice!

```R
rownames(team_shooting_mat) <- team_shooting_df$Team
```

Note that `team_shooting_mat` is also included, but I wanted you to see how to change a data frame into a matrix that `pca_biplot` can read.

Then, make a lovely biplot!  The only required option is `data`, but I'll throw in a couple more to make things look nice.

```R
library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(biplotr)

# pca_biplot returns a ggplot object
chart <- pca_biplot(
  # The data matrix
  data = team_shooting_mat,
  
  # Add a custom title
  chart_title = "NBA Team Shooting 2018",
  
  # Increase the x axis limits by 1 in + and - directions
  limits_nudge_x = 1,
  
  # Center the data (important for PCA)
  center_data = T,
  # Scale the data (since some of our variables have much higher magnitude than others)
  scale_data = T,
  
  # Show point labels
  point_labels = T,
  # Push the labels 0.35 units on the y axis
  point_labels_nudge_y = 0.35,
  
  # Show arrow labels
  arrow_labels = T,
  # Push arrow labels away from arrow heads by 0.35 units
  arrow_labels_nudge_y = 0.35
)

# Draw the plot
grid.arrange(chart)

# Or save the plot as a pdf
pdf("snazzy_biplot.pdf", width = 10, height = 10)
grid.arrange(chart)
dev.off()
```

