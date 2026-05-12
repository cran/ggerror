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


# summary table, grouped by month with Temp's means and standard deviation 
aq_monthly_avg <- data.frame(
  Month = unique(airq$Month),
  Temp = tapply(airq$Temp, airq$Month, mean),
  sd_temp_monthly   = tapply(airq$Temp, airq$Month, sd)
)

p <- ggplot(aq_monthly_avg, aes(Temp, Month))

## ----simple-default-----------------------------------------------------------

p + 
  geom_error(aes(error = sd_temp_monthly),
             width = 0.4) +
  labs(title = "p + geom_error(aes(error = sd_temp_monthly))",
       caption = "p <- ggplot(aq_monthly_avg, aes(Temp, Month))"
       )

## ----simple-pinned------------------------------------------------------------
p +
  geom_error_pointrange(aes(error = sd_temp_monthly),
                        size = 0.8,
                        shape = "x") +
  labs(title = "p + geom_error_pointrange(aes(error = sd_temp_monthly))")

## ----simple-arg---------------------------------------------------------------
p +
  geom_error(aes(error = sd_temp_monthly),
             error_geom = "linerange") +
  labs(title = "geom_error(error_geom = \"linerange\")")

## ----fp-tip, eval = FALSE-----------------------------------------------------
# purrr::map(c("errorbar", "linerange", "crossbar", "pointrange"),
#            ~ p + geom_error(aes(error = sd_temp_monthly), error_geom = .x))

## ----asym-prep-plot-----------------------------------------------------------
may_week <- subset(airq[1:7,], Month == 'May')

may_summary <- data.frame(
  Day = may_week$Day,
  Temp = may_week$Temp,
  dist2min = may_week$Temp - min(may_week$Temp, na.rm = TRUE),
  dist2max = max(may_week$Temp, na.rm = TRUE) - may_week$Temp
) # Each Temp point now has its distance from the minimum Temp in the
  # dataset, and its distance from the maximum Temp in the dataset.


## ----asym-dist2minmax---------------------------------------------------------

ggplot(may_summary, aes(x = Temp, y = Day)) +
  geom_error(aes(error_neg =  dist2min,
                 error_pos = dist2max),
             error_geom = "pointrange",
             color_neg = "steelblue",
             color_pos = "firebrick",
             linewidth  = 1
             ) +
  labs(title = "geom_error(error_neg = dist2min, error_pos = dist2max)")

## ----asym-crossbar------------------------------------------------------------
ggplot(may_summary, aes(Temp, Day)) +
  geom_error(aes(error_neg = dist2min,
                 error_pos = dist2max),
             error_geom = "crossbar",
             fill_neg = "#d97757", fill_pos = "#ffaa66",
             width_neg = 0.3,     width_pos = 0.6) +
  labs(title = "crossbar with per-side fills + widths")

## ----one-sided-basic----------------------------------------------------------
ggplot(may_summary, aes(Temp, Day)) +
  geom_error(aes(error_neg = dist2min, error_pos = NA),
             color = "steelblue",
             linewidth = 1,
             linetype = 9) +
  geom_point(size = 1.5) +
  labs(title = "geom_error(aes(error_neg=dist2min, error_pos = NA))")

