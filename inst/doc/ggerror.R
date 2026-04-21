## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 6,
  fig.height = 4,
  dpi = 96,
  warning = FALSE,
  message = FALSE
)

## ----imports------------------------------------------------------------------
library(ggplot2)
library(ggerror)

## ----demo-data----------------------------------------------------------------
set.seed(1)
mt <- mtcars[sample(nrow(mtcars), 5), ]
mt$rn <- rownames(mt)

base_theme <- theme_minimal() + theme(
  plot.title = element_text(hjust = 0, family = "consolas", size = 12),
  axis.title = element_blank()
)


## ----base-plot----------------------------------------------------------------
p <- ggplot(mt, aes(mpg, rn)) +
  geom_point() + base_theme

p + geom_error(aes(error = drat))

## ----argument_or_pinned-------------------------------------------------------
p + geom_error(aes(error = drat), error_geom = "crossbar")
p + geom_error_crossbar(aes(error = drat))

## ----functional-example, eval = requireNamespace("purrr", quietly = TRUE)-----
supported_geoms <- c('errorbar', 'crossbar', 'linerange', 'pointrange')
purrr::map(supported_geoms, \(err) p + geom_error(aes(error = drat), error_geom = err))

## ----asymmetric-basic---------------------------------------------------------
  p + geom_error(aes(
    error_neg = drat / 2,
    error_pos = drat)
    )

## ----one-sided----------------------------------------------------------------
p +
  geom_error(aes(
    error_neg = 0,
    error_pos = drat),
    width_neg = 0
  )

## ----asymmetric-styled--------------------------------------------------------
p +
  geom_error(aes(
    error_neg = drat / 2,
    error_pos = drat),

    colour_neg = "steelblue",
    colour_pos = "firebrick",
    linewidth = 2 # you can still control both sides of the error bar
  )

## ----asymmetric-styled2-------------------------------------------------------
p +
  geom_error(aes(
    error_neg = drat / 2,
    error_pos = drat),
    
    error_geom = "crossbar",
    fill_neg = "grey90",
    fill_pos = "grey30",
    width_neg = 0.2,
    width_pos = 0.6
  )

