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

fredr_set_key("e5fede734aaa8bd57688678bdfe94620")

create_heatmap_plot(data = monthly_data, var_name = "initial_claims_nsa", start_year = 2000,
                    end_year = 2023, x_axis_breaks = 5, title = "Initial Claims NSA",
                    frequency = "monthly", change_space = "yoy", num_decimals = 1,
                    include_cell_numbers = TRUE, scaling_power = NULL, flip_colors = TRUE)

create_line_plot(data = monthly_data, var_name = "employee_comp", start_year = 2000, end_year = 2023, x_axis_breaks = 5,
                 x_axis_title = "", y_axis_title = "Billions of Dollars", title = "Employee Comp",
                 plot_change = "yoy", y_axis_breaks = NULL,
                 include_smooth = FALSE)

create_returns_plot(price_data = weekdaily_data, start_date = "2023-01-01", variables = c("SPY", "TLT"),
                    combo_bets = list(combo_bet("SPY", "TLT", 1, -1)), add_risk_parity = TRUE)

create_multi_line_plot(data = monthly_data, var_name_list = c("bank_assets_sa", "bank_loans", "bank_securities_sa"),
                       start_year = 1980, end_year = 2023, var_label_list = c("Bank Assets", "Bank Loans", "Bank Securities"),
                       x_axis_breaks = 5, x_axis_title = "", y_axis_title = "Billions of Dollars", title = "Commercial Banks",
                       include_smooth = FALSE, var_changes = c("yoy", "mom", "yoy"))

create_two_axis_line_plot(data = monthly_data, variables = c("fed_funds_rate", "cpi"),
                          var_labels = c("Fed Funds Rate (%)", "CPI"), start_date = "2018-01-01",
                          end_date = "2024-01-01", x_axis_breaks = 1, plot_title = "Fed Funds and YoY CPI",
                          var_1_type = NULL, var_2_type = "mom")

create_scatter_plot(data = monthly_data, x_var = "total_under_construct_nsa", y_var = "total_housing_starts_nsa",
                    x_label = "Under Construction (1000s)", y_label = "Starts (1000s)",
                    title = "Housings Starts vs Under Construction",
                    highlight_dates = c("2022-01-01", "2021-01-01", "2023-01-01", "2020-01-01", "2019-01-01", "2018-01-01"),
                    highlight_latest = TRUE,
                    include_lof = NULL, x_change = "qoq", y_change = "qoq",
                    log_x = FALSE, log_y = FALSE)
