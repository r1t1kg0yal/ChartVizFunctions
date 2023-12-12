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

weekdaily_data <- read.csv("data/weekdaily_data.csv")
weekdaily_data$date <- as.Date(weekdaily_data$date)

create_heatmap_plot(data = monthly_data, var_name = "initial_claims_nsa", start_year = 2000,
                    end_year = 2023, x_axis_breaks = 5, title = "Initial Claims NSA",
                    frequency = "monthly", change_space = "yoy", num_decimals = 1,
                    include_cell_numbers = TRUE, scaling_power = NULL, flip_colors = TRUE)

create_line_plot(data = monthly_data, var_name = "corp_profits_less_fed_profits", start_year = 2000, end_year = 2023, x_axis_breaks = 5, x_axis_title = "", y_axis_title = "Billions of Dollars", title = "BEA Corporate Profits Less Fed Profits",
                 plot_type = "level", y_axis_breaks = NULL,
                 include_smooth = FALSE)

create_returns_plot(price_data = weekdaily_data, start_date = "2023-01-01", variables = c("SPY", "TLT"),
                    combo_bets = list(combo_bet("SPY", "TLT", 1, -1)), add_risk_parity = FALSE)

create_multi_line_plot(data = monthly_data, var_name_list = c("bank_assets_sa", "bank_loans", "bank_securities_sa"),
                       start_year = 1980, end_year = 2023, var_label_list = c("Bank Assets", "Bank Loans", "Bank Securities"),
                       x_axis_breaks = 5, x_axis_title = "", y_axis_title = "Billions of Dollars", title = "Commercial Banks",
                       plot_type = "level", include_smooth = TRUE)
