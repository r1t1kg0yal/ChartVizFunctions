#' Create a Line Plot
#'
#' This function generates a line plot for a given variable over time, with options for different types of transformations and additional smoothing.
#'
#' @param data A data frame containing the dataset for plotting. Must include a 'date' column.
#' @param var_name Name of the variable in 'data' to be plotted.
#' @param start_year The starting year for the plot.
#' @param end_year The ending year for the plot.
#' @param x_axis_breaks Breaks for the x-axis, usually years.
#' @param x_axis_title Title for the x-axis.
#' @param y_axis_title Title for the y-axis.
#' @param title Title of the plot.
#' @param plot_change Type of the plot transformation ('mom' for month-over-month, 'qoq' for quarter-over-quarter, 'yoy' for year-over-year, or NULL for no change).
#' @param y_axis_breaks Break points for the y-axis (optional).
#' @param include_smooth A boolean indicating whether to include a Loess smoothed line. Only applies when plot_change is NULL.
#' @param y_axis_lower_bound Lower bound for the y-axis (optional).
#' @param y_axis_upper_bound Upper bound for the y-axis (optional).
#'
#' @return A ggplot object representing the line plot.
#' @export
#'
#' @examples
#' create_line_plot(data = monthly_data, var_name = "corp_profits_less_fed_profits", start_year = 2000,
#'                  end_year = 2023, x_axis_breaks = 5, x_axis_title = "", y_axis_title = "Billions of Dollars", title = "BEA Corporate Profits Less Fed Profits",
#'                  plot_change = NULL, y_axis_breaks = NULL,
#'                  include_smooth = FALSE)
#' create_line_plot(data = monthly_data, var_name = "corp_profits_less_fed_profits", start_year = 2000,
#'                  end_year = 2023, x_axis_breaks = 5, x_axis_title = "", y_axis_title = "YoY % Change", title = "BEA Corporate Profits Less Fed Profits",
#'                  plot_change = "yoy", y_axis_breaks = 2,
#'                  include_smooth = TRUE)
create_line_plot <- function(data, var_name, start_year, end_year, x_axis_breaks,
                             x_axis_title, y_axis_title, title, plot_change = NULL,
                             y_axis_breaks = NULL, include_smooth = TRUE,
                             y_axis_lower_bound = NULL, y_axis_upper_bound = NULL) {

  library(dplyr)
  library(ggplot2)
  library(lubridate)

  # Get recession data and merge with existing data
  data <- add_recession_data(data)

  # Validate plot_change parameter
  valid_changes <- c("mom", "qoq", "yoy", NULL)
  if (!is.null(plot_change) && !(plot_change %in% valid_changes)) {
    stop("plot_change must be 'mom', 'qoq', 'yoy', or NULL.")
  }

  # Determine the frequency of the data
  date_diffs <- diff(data$date)
  median_diff <- median(date_diffs)

  # Set the lag based on the frequency
  determine_lag <- function(change_type) {
    if (change_type == "mom") {
      return(ifelse(median_diff <= 7, 4, 1)) # Weekly data uses 4 weeks lag, else monthly
    } else if (change_type == "qoq") {
      return(ifelse(median_diff <= 31, 13, ifelse(median_diff <= 92, 3, 1))) # Weekly, Monthly, Quarterly
    } else if (change_type == "yoy") {
      return(ifelse(median_diff <= 7, 52, ifelse(median_diff <= 31, 12, ifelse(median_diff <= 92, 4, 1)))) # Weekly, Monthly, Quarterly, Annually
    } else {
      return(0)
    }
  }

  # Apply the transformation
  lag_days <- determine_lag(plot_change)
  if (lag_days > 0) {
    data <- data %>%
      mutate(change_variable = (get(var_name, data) / lag(get(var_name, data), lag_days) - 1) * 100)
  } else {
    data <- data %>%
      mutate(change_variable = get(var_name, data))
  }

  # Filter data based on the provided date range
  data <- data %>%
    filter(date >= as.Date(paste0(start_year, "-01-01")), date <= as.Date(paste0(end_year, "-12-31")))

  # Create the plot
  p <- ggplot(data, aes(x = date, y = change_variable)) +
    geom_rect(data = get_recession_periods(data) %>%
                filter(start >= as.Date(paste0(start_year, "-01-01")) & end <= as.Date(paste0(end_year, "-12-31"))),
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
    geom_line() +
    scale_x_date(breaks = year_breaks_lineplot_x_axis(data$date, x_axis_breaks), labels = date_format("%Y")) +
    xlab(x_axis_title) +
    ylab(y_axis_title) +
    ggtitle(title) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major = element_line(color = "#d3d3d3", linewidth = 0.2), # Light gray color for major gridlines
      panel.grid.minor = element_line(color = "#d3d3d3", linewidth = 0.1)  # Light gray color for minor gridlines
    )

  # Add Loess smooth line if include_smooth is TRUE and the data is not a single change type
  if(include_smooth && is.null(plot_change)) {
    p <- p + geom_smooth(se = FALSE, method = "loess", span = 0.1)
  }

  # Modify the scale_y_continuous call
  if (!is.null(y_axis_breaks)) {
    p <- p + scale_y_continuous(
      breaks = function(limits) {
        seq(from = floor(limits[1] / y_axis_breaks) * y_axis_breaks,
            to = ceiling(limits[2] / y_axis_breaks) * y_axis_breaks,
            by = y_axis_breaks)
      },
      labels = scales::number_format(big.mark = ","),
      limits = if (!is.null(y_axis_lower_bound) || !is.null(y_axis_upper_bound)) {
        c(y_axis_lower_bound, y_axis_upper_bound)
      }
    )
  } else {
    p <- p + scale_y_continuous(
      labels = scales::number_format(big.mark = ","),
      limits = if (!is.null(y_axis_lower_bound) || !is.null(y_axis_upper_bound)) {
        c(y_axis_lower_bound, y_axis_upper_bound)
      }
    )
  }

  # Return the plot
  return(p)
}
