#' Create a Line Plot
#'
#' This function generates a line plot for a given variable over time, with options for different types of transformations and additional smoothing.
#'
#' @param data A data frame containing the dataset for plotting. Must include a 'date' column.
#' @param var_name Name of the variable in 'data' to be plotted.
#' @param start_date The starting date for the plot.
#' @param end_date The ending date for the plot.
#' @param x_axis_breaks Breaks for the x-axis, can be years or months depending on 'use_month_intervals'.
#' @param use_month_intervals A boolean to determine if the x-axis should use month intervals.
#' @param x_axis_title Title for the x-axis.
#' @param y_axis_title Title for the y-axis.
#' @param title Title of the plot.
#' @param plot_change Type of the plot transformation ('mom' for month-over-month, 'qoq' for quarter-over-quarter, 'yoy' for year-over-year, or NULL for no change).
#' @param y_axis_breaks Break points for the y-axis (optional).
#' @param include_smooth A boolean indicating whether to include a Loess smoothed line. Only applies when plot_change is NULL.
#' @param lof_span Span for the loess smoothing (optional).
#' @param y_axis_lower_bound Lower bound for the y-axis (optional).
#' @param y_axis_upper_bound Upper bound for the y-axis (optional).
#' @param recession_bars A boolean indicating whether to include recession bars in the plot.
#' @param diagonal_x_labels A boolean indicating whether x-axis labels should be diagonal.
#'
#' @return A ggplot object representing the line plot.
#' @export
#'
#' @examples
#' create_line_plot(data = monthly_data, var_name = "corp_profits_less_fed_profits",
#'                  start_date = "2000-01-01", end_date = "2023-12-31",
#'                  x_axis_breaks = 5, use_month_intervals = FALSE,
#'                  x_axis_title = "", y_axis_title = "Billions of Dollars",
#'                  title = "BEA Corporate Profits Less Fed Profits",
#'                  plot_change = NULL, y_axis_breaks = NULL,
#'                  include_smooth = FALSE, lof_span = 0.1,
#'                  y_axis_lower_bound = NULL, y_axis_upper_bound = NULL,
#'                  recession_bars = FALSE, diagonal_x_labels = FALSE)
#' create_line_plot(data = monthly_data, var_name = "corp_profits_less_fed_profits",
#'                  start_date = "2000-01-01", end_date = "2023-12-31",
#'                  x_axis_breaks = 5, use_month_intervals = FALSE,
#'                  x_axis_title = "", y_axis_title = "YoY % Change",
#'                  title = "BEA Corporate Profits Less Fed Profits",
#'                  plot_change = "yoy", y_axis_breaks = 2,
#'                  include_smooth = TRUE, lof_span = 0.1,
#'                  y_axis_lower_bound = NULL, y_axis_upper_bound = NULL,
#'                  recession_bars = FALSE, diagonal_x_labels = FALSE)

create_line_plot <- function(data, var_name, start_date, end_date, x_axis_breaks,
                             use_month_intervals = FALSE, x_axis_title, y_axis_title, title,
                             plot_change = NULL, y_axis_breaks = NULL, include_smooth = TRUE,
                             lof_span = 0.1, y_axis_lower_bound = NULL,
                             y_axis_upper_bound = NULL, recession_bars = FALSE,
                             diagonal_x_labels = FALSE) {

  library(dplyr)
  library(ggplot2)
  library(lubridate)

  # Check if the dataset 'data' exists
  if (!exists("data")) {
    stop("Dataset 'data' not found.")
  }

  # Check if 'data' is a data frame
  if (!is.data.frame(data)) {
    stop("The provided 'data' is not a data frame.")
  }

  # Check if the variable in 'var_name' is found in 'data'
  if (!var_name %in% names(data)) {
    stop(paste("Variable '", var_name, "' not found in the dataset.", sep = ""))
  }

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

  # Define the determine_lag function
  # Determine the lag for each change type
  # Define the determine_lag function with median_diff as an additional parameter
  determine_lag <- function(change_type, median_diff) {
    if (change_type == "mom") {
      if (median_diff <= 1) {
        return(365)  # Daily data
      } else if (median_diff <= 7) {
        return(4)    # Weekly data
      } else {
        return(1)    # Monthly data
      }
    } else if (change_type == "qoq") {
      if (median_diff <= 1) {
        return(365/3)  # Daily data
      } else if (median_diff <= 7) {
        return(13)     # Weekly data
      } else if (median_diff <= 31) {
        return(3)      # Monthly data
      } else if (median_diff <= 92) {
        return(1)      # Quarterly data
      }
    } else if (change_type == "yoy") {
      if (median_diff <= 1) {
        return(365)    # Daily data
      } else if (median_diff <= 7) {
        return(52)     # Weekly data
      } else if (median_diff <= 31) {
        return(12)     # Monthly data
      } else if (median_diff <= 92) {
        return(4)      # Quarterly data
      } else {
        return(1)      # Annual data
      }
    } else {
      # For no change or invalid change type, no lag is applied
      return(0)
    }
  }

  # Apply the transformation based on plot_change
  if (!is.null(plot_change)) {
    lag_days <- determine_lag(plot_change, median_diff)
    if (lag_days > 0) {
      data <- data %>%
        mutate(change_variable = (get(var_name, data) / lag(get(var_name, data), lag_days) - 1) * 100)
    } else {
      data <- data %>%
        mutate(change_variable = get(var_name, data))
    }
  } else {
    data <- data %>%
      mutate(change_variable = get(var_name, data))
  }

  # Filter data based on the provided date range
  data <- data %>%
    filter(date >= as.Date(start_date), date <= as.Date(end_date))

  # Create the plot
  p <- ggplot(data, aes(x = date, y = change_variable)) +
    geom_line()

  # Add recession bars if recession_bars is TRUE
  if (recession_bars) {
    p <- p + geom_rect(data = get_recession_periods(data) %>%
                         filter(start >= as.Date(start_date) & end <= as.Date(paste0(end_year, "-12-31"))),
                       aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5, inherit.aes = FALSE)
  }

  # Modify x-axis scale based on the use of month or year intervals
  if (use_month_intervals) {
    p <- p + scale_x_date(breaks = month_breaks_lineplot_x_axis(start_date, end_date, x_axis_breaks),
                          date_labels = "%Y-%m")
  } else {
    p <- p + scale_x_date(breaks = year_breaks_lineplot_x_axis(data$date, x_axis_breaks),
                          labels = date_format("%Y"))
  }

    p <- p +
    xlab(x_axis_title) +
    ylab(y_axis_title) +
    ggtitle(title) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major = element_line(color = "#d3d3d3", linewidth = 0.2), # Light gray color for major gridlines
      panel.grid.minor = element_line(color = "#d3d3d3", linewidth = 0.1)  # Light gray color for minor gridlines
    )

    # Apply diagonal labels if diagonal_x_labels is TRUE
    if (diagonal_x_labels) {
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

  # Add Loess smooth line if include_smooth is TRUE and the data is not a single change type
  if(include_smooth && is.null(plot_change)) {
    p <- p + geom_smooth(se = FALSE, method = "loess", span = lof_span)
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
