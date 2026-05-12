## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 6,
  fig.height = 4,
  dpi = 96,
  warning = FALSE,
  message = FALSE
)

## ----viz-setup, include=FALSE-------------------------------------------------
library(ggplot2)
library(ggerror)

set_theme(
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        family = "mono",
        size = 11,
        hjust = 0),
      plot.caption = element_text(face = 'italic'),
      legend.position = 'none'
      )
  )

## ----data-prep----------------------------------------------------------------
data("airquality"); airq <- airquality

# It wouldn't be an R workflow without minimal data cleaning...
day_in_month <- function(day_in_month, month, year) {
  days_abbr <- format(as.Date(sprintf("%d-%02d-%02d", year, month, day_in_month)), "%a")
  factor(days_abbr, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), ordered = TRUE)
}
airq$Day <- day_in_month(airq$Day, airq$Month, 1973)
airq$Month <- factor(airq$Month, labels = month.abb[5:9])


## ----fit-model----------------------------------------------------------------
model   <- lm(Temp ~ Wind, data = airq)
airq_fit <- cbind(airquality[,c('Temp','Wind')],
                  predicted = predict(model),
                  residuals = resid(model)
                 )

## ----residuals-plot, fig.height = 4.5-----------------------------------------
ggplot(airq_fit, aes(x = Wind, y = predicted)) +
  geom_line(linewidth = 0.4, colour = "grey40") +
  geom_point(aes(y = Temp), alpha = 0.5) +
  geom_error(aes(error = residuals),
             sign_aware  = TRUE,
             orientation = "x",
             color_pos  = "firebrick",
             color_neg  = "steelblue",
             linewidth   = 0.5,
             alpha       = 0.85) +
  labs(title = paste(
    "[1] residuals = resid(lm(Temp ~ Wind))",
    "[2] geom_error(error = residuals, sign_aware = TRUE)",
    sep = "\n"
  ))

## ----stat-default-------------------------------------------------------------
ggplot(airq, aes(Month, Temp)) +
  stat_error(width = 0.2) +
  stat_summary(geom='point') +
  labs(title = "stat_error() with default = mean_se") 

## ----stat-ci------------------------------------------------------------------
ggplot(airq, aes(Month, Temp)) +
  stat_error(fun = "mean_ci", error_geom = "pointrange") +
  labs(title = 'stat_error(fun = "mean_ci", error_geom = "pointrange")')

## ----stat-conf----------------------------------------------------------------
ggplot(airq, aes(Month, Temp)) +
  stat_error(fun = "mean_ci", conf.int = 0.99,
             error_geom = "linerange") + 
  stat_summary(geom="point") +
  labs(title = 'stat_error(fun = "mean_ci", conf.int = 0.99)')

## ----stat-custom--------------------------------------------------------------
mae_summary <- function(my_vec, scale_by = 1) {
  md  <- median(my_vec)
  mae <- mean(abs(my_vec - md)) * scale_by
  data.frame(y = md, ymin = md - mae, ymax = md + mae)
}

ggplot(airq, aes(Month, Temp)) +
  stat_error(fun = mae_summary, error_geom = "crossbar",
             fill = "grey90") +
  labs(title = "stat_error(fun = median ± mean(|x − median|))")

## ----aditional-code-collapsed-------------------------------------------------
ggplot(airq, aes(Month, Wind)) +
  stat_error(fun = mae_summary, scale_by = 2, error_geom = "crossbar") +
  stat_summary(geom = "point")

