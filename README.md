# BiplotR

Give me data, and I'll give you a snazzy biplot!

A simple little package for creating nice looking PCA biplots.  Just input your data matrix/frame and out pops a beautiful biplot for your data analyzing pleasure!

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

For usage examples, check out the built in documentation for the `pca_biplot` function like this:

```R
?pca_biplot
```

## Changes

- `v0.0.7`: Add option for custom arrow scaling factor.
- `v0.0.6`: Add option to draw or not draw arrows and points

## To do list

* Add option to change arrow and point colors
