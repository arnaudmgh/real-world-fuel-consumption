library(readr)
library(tidyverse)

odo <- read_csv("data/odometer_sienna.csv", col_names = FALSE) %>%
  select(-1) %>%
  set_names(c("date", "odometer"))

  odo <- odo %>%
    mutate(
      date = lubridate::ymd(date),                 # adjust to lubridate::dmy/ymd as needed
    ) %>%
    arrange(date) %>%
    mutate(
      date_lag = lag(date),
      odometer_lag = lag(odometer),
      odometer_diff = odometer - odometer_lag,    # difference with previous odometer reading
      date_diff = as.numeric(date - date_lag), # days between readings
      miles_per_day = odometer_diff / date_diff
    )
  odo

library(plotly)

p <- ggplot(odo, aes(x = odometer_diff, y = miles_per_day,
                     text = paste0("date: ", date, "\n",
                                   "odometer: ", odometer, "\n",
                                   "date_lag: ", date_lag, "\n",
                                   "odometer_lag: ", odometer_lag, "\n",
                                   "odometer_diff: ", odometer_diff, "\n",
                                   "date_diff: ", date_diff, "\n",
                                   "miles_per_day: ", round(miles_per_day, 3)))) +
  geom_point(color = "steelblue", size = 2, alpha = 0.8)

ggplotly(p, tooltip = "text")
