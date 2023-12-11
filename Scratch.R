library(ChartVizFunctions)
library(quantmod)
library(Quandl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(fredr)
library(lubridate)
library(scales)
library(zoo)
library(stringr)
library(devtools)

monthly_data <- read.csv("data/monthly_data.csv")
monthly_data$date <- as.Date(monthly_data$date)

create_heatmap_plot(data = monthly_data, var_name = "initial_claims_nsa", start_year = 2000,
                    end_year = 2023, x_axis_breaks = 5, title = "Initial Claims NSA",
                    frequency = "monthly", change_space = "yoy", num_decimals = 1,
                    include_cell_numbers = TRUE, scaling_power = NULL, flip_colors = TRUE)

create_line_plot(data = monthly_data, var_name = "corp_profits_less_fed_profits", start_year = 2000, end_year = 2023, x_axis_breaks = 5, x_axis_title = "", y_axis_title = "Billions of Dollars", title = "BEA Corporate Profits Less Fed Profits",
                 plot_type = "level", y_axis_breaks = NULL,
                 include_smooth = FALSE)
