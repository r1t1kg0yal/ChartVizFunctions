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
library(roxygen2)

monthly_data <- read.csv("data/monthly_data.csv")
monthly_data$date <- as.Date(monthly_data$date)

weekly_data <- read.csv("data/weekly_data.csv")
weekly_data$date <- as.Date(weekly_data$date)

weekdaily_data <- read.csv("data/weekdaily_data.csv")
weekdaily_data$date <- as.Date(weekdaily_data$date)

fm_impulse_data <- read.csv("data/fm_impulse_data.csv")
fm_impulse_data$date <- as.Date(fm_impulse_data$date)

fredr_set_key("e5fede734aaa8bd57688678bdfe94620")

create_multi_line_plot(data = fm_impulse_data, var_name_list = c("m2", "m2_less_reserves"),
                       start_date = "2000-01-01", end_date = "2023-01-01", var_label_list = c("M2", "M2 Less Reserves"),
                       x_axis_title = "", y_axis_title = "$b", title = "M2", x_axis_breaks = 24,
                       y_axis_breaks = 1000, lof_span = 0.1, use_month_intervals = TRUE,
                       var_changes = NULL, include_smooth = TRUE, diagonal_x_labels = TRUE)

create_heatmap_plot(data = monthly_data, var_name = "initial_claims_nsa", start_year = 2000,
                    end_year = 2024, x_axis_breaks = 5, title = "Initial Claims NSA",
                    frequency = "monthly", change_space = "level", num_decimals = 1,
                    include_cell_numbers = TRUE, scaling_power = 6, flip_colors = TRUE,
                    color_2 = "green", mute_color_2 = TRUE, text_size = 1.8, text_bold = FALSE)

create_heatmap_plot(data = weekly_data, var_name = "initial_claims_nsa", start_year = 1992,
                    end_year = 2024, x_axis_breaks = 5, title = "Initial Claims NSA",
                    frequency = "weekly", change_space = "level", num_decimals = 0,override_subtitle = "1000s",
                    include_cell_numbers = TRUE, scaling_power = 3, flip_colors = TRUE,
                    legend_name = "Legend Name", x_axis_label = NULL, y_axis_label = NULL)

create_line_plot(data = monthly_data, var_name = "employee_comp", start_date = "2000-01-01", end_date = "2023-01-01", x_axis_breaks = 24,
                 x_axis_title = "", y_axis_title = "Billions of Dollars", title = "Employee Comp", use_month_intervals = TRUE,
                 plot_change = NULL, y_axis_breaks = NULL,
                 include_smooth = FALSE, diagonal_x_labels = TRUE)

create_line_plot(data = fm_impulse_data, var_name = "rolling_annual_gross_ust_outlays", start_year = 2000, end_year = 2023,
                 x_axis_breaks = 5, x_axis_title = "", y_axis_title = "Billions of Dollars", title = "Rolling Annual Gross Treasury Outlays",
                 plot_change = NULL, y_axis_breaks = 1000,
                 include_smooth = FALSE)

create_multi_line_plot(data = monthly_data, var_name_list = c("bank_assets_sa", "bank_loans", "bank_securities_sa"),
                       start_year = 1980, end_year = 2023, var_label_list = c("Bank Assets", "Bank Loans", "Bank Securities"),
                       x_axis_breaks = 5, x_axis_title = "", y_axis_title = "Billions of Dollars", title = "Commercial Banks",
                       include_smooth = FALSE, var_changes = NULL)

create_two_axis_line_plot(data = monthly_data, variables = c("fed_funds_rate", "cpi"),
                          var_labels = c("Fed Funds Rate (%)", "CPI"), start_date = "2000-01-01",
                          end_date = "2024-01-01", x_axis_breaks = 1, plot_title = "Fed Funds and YoY CPI",
                          var_1_type = "yoy", var_2_type = "mom", horizontal_line = "left")

create_scatter_plot(data = monthly_data, x_var = "total_under_construct_nsa", y_var = "total_housing_starts_nsa", subtitle = "Y Lagged by 2y",
                    x_label = "Under Construction (1000s)", y_label = "Starts (1000s)", start_date = "2000-01-01",
                    title = "Housings Starts vs Under Construction", highlight_dates = c("2022-01-01"),
                    y_lag = NULL, lof_color = "red", dot_size = 1, highlight_size = 3,
                    include_lof = "lm", x_change = "yoy", diagonal_x_labels = FALSE,
                    log_x = FALSE, log_y = FALSE, x_lower_bound = 0, lof_bounds = c("2002-01-01", "2003-01-01"))

create_scatter_plot(data = fm_impulse_data, x_var = "fed_funds_rate", y_var = "loans_ex_ppp_loans", start_date = "2000-01-01",
                    x_label = "Fed Funds Rate (%)", y_label = "Monetary Impulse",
                    title = "Monetary Impulse vs Fed Funds Rate",
                    highlight_dates = c("2022-01-01", "2021-01-01", "2023-01-01", "2020-01-01", "2019-01-01", "2018-01-01"),
                    highlight_latest = TRUE, y_change = "yoy",
                    include_lof = "lm",
                    log_x = FALSE, log_y = FALSE, x_lower_bound = 5, y_lower_bound = 0)

create_returns_plot(price_data = weekdaily_data, start_date = "2023-01-01", variables = c("SPY", "TLT"),
                    combo_bets = list(combo_bet("SPY", "TLT", 1, -1)), add_risk_parity = TRUE)



get_cumulative_table(df = monthly_data,
                                                 var_names = c("nf_employees", "resi_const_employees_nsa"),
                                                 interval_short = "decades",
                                                 interval_manual = NULL)

get_cumulative_table(df = monthly_data, c("labor_prod_monthly", "labor_prod"), interval_short = "decades", interval_manual = NULL)


manual_intervals <- list(
  c(2000, 2005),
  c(2006, 2010),
  c(2011, 2015),
  c(2016, 2020)
)

cumulative_table_decades <- get_cumulative_table(df = monthly_data,
                                                 var_names = c("nf_employees", "resi_const_employees_nsa"),
                                                 interval_short = NULL,
                                                 interval_manual = manual_intervals)


create_returns_table(price_data = weekdaily_data, start_date = "2023-01-01", variables = c("SPY", "TLT"),
                     combo_bets = list(combo_bet("SPY", "TLT", "IWM", 1, -1, -1)), add_risk_parity = FALSE)

create_returns_plot(price_data = weekdaily_data, start_date = "2003-01-01", variables = c("SPY", "TLT"),
                     combo_bets = list(combo_bet("TLT", "SPY", "IWM", -1, -.5, 1)), add_risk_parity = FALSE)

create_multi_scatter_plot(
  data = monthly_data,
  var_name_list = c("initial_claims_nsa", "initial_claims_sa"),
  var_label_list = c("Not Seasonally Adjusted", "Seasonally Adjusted"),
  start_year = 1980,
  end_year = 2025,
  x_axis_breaks = 5,
  x_axis_title = "",
  y_axis_title = "Number of Claims",
  title = "Initial Claims",
  plot_type = "level",
  y_axis_breaks = NULL,
  include_smooth = FALSE,
  y_axis_lower_bound = 0,
  y_axis_upper_bound = 2000000
)

create_line_plot(data = monthly_data, var_name = "ahe",
                 start_year = 1980, end_year = 2023, x_axis_breaks = 1, x_axis_title = "",
                 y_axis_title = "1000s of Persons", title = "Monthly Non-Farm Payrolls", plot_change = "yoy",
                 y_axis_breaks = NULL,
                 include_smooth = FALSE)
