#' Create a Multi-Line Plot
#'
#' This function generates a multi-line plot for a list of variables over time,
#' with options for different types of data transformations and additional smoothing.
#'
#' @param data A data frame containing the dataset for plotting.
#' Must include a 'date' column.
#' @param var_name_list A vector of variable names in 'data' to be plotted.
#' @param var_label_list A vector of labels corresponding to the variables for the plot legend.
#' @param start_date The starting date for the plot.
#' @param end_date The ending date for the plot.
#' @param x_axis_breaks Breaks for the x-axis, can be years or months depending on 'use_month_intervals'.
#' @param use_month_intervals A boolean to determine if the x-axis should use month intervals.
#' @param x_axis_title Title for the x-axis.
#' @param y_axis_title Title for the y-axis.
#' @param title Title of the plot.
#' @param subtitle Subtitle for the plot (optional).
#' @param var_changes A list of transformations to apply to each variable.
#' Acceptable values are 'mom' for month-over-month, 'qoq' for quarter-over-quarter,
#' 'yoy' for year-over-year, or 'level' for no change.
#' The list must be the same length as 'var_name_list'.
#' @param y_axis_breaks Break points for the y-axis (optional).
#' @param include_smooth A boolean indicating whether to include a Loess smoothed line.
#' Applicable only when no transformation is applied (i.e., var_changes is NULL or contains all NULLs).
#' @param lof_span Span for the loess smoothing (optional).
#' @param y_axis_lower_bound Lower bound for the y-axis (optional).
#' @param y_axis_upper_bound Upper bound for the y-axis (optional).
#' @param recession_bars A boolean indicating whether to include recession bars in the plot.
#' @param diagonal_x_labels A boolean indicating whether x-axis labels should be diagonal.
#'
#' @return A ggplot object representing the multi-line plot.
#' @export
#'
#' @examples
#' create_multi_line_plot(data = monthly_data,
#'                        var_name_list = c("bank_assets_sa", "bank_loans", "bank_securities_sa"),
#'                        var_label_list = c("Bank Assets", "Bank Loans", "Bank Securities"),
#'                        start_date = "1980-01-01", end_date = "2023-12-31",
#'                        x_axis_breaks = 5, use_month_intervals = FALSE,
#'                        x_axis_title = "Year", y_axis_title = "Billions of Dollars",
#'                        title = "Commercial Banks", subtitle = "Financial Indicators Over Time",
#'                        var_changes = NULL, include_smooth = FALSE, lof_span = 0.1,
#'                        y_axis_lower_bound = NULL, y_axis_upper_bound = NULL,
#'                        recession_bars = FALSE, diagonal_x_labels = FALSE)

create_multi_line_plot <- function(data, var_name_list, var_label_list, start_date, end_date,
                                   x_axis_breaks, use_month_intervals = FALSE,
                                   x_axis_title, y_axis_title, title, var_changes = NULL,
                                   y_axis_breaks = NULL, include_smooth = FALSE, lof_span = 0.1,
                                   subtitle = NULL, y_axis_lower_bound = NULL,
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

  # Check if each variable in 'var_name_list' is found in 'data'
  missing_vars <- var_name_list[!var_name_list %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(paste("The following variables are not found in the dataset:", paste(missing_vars, collapse = ", "), "."))
  }

  # Get recession data and merge with existing data
  data <- add_recession_data(data)

  # Validate inputs
  if (length(var_name_list) != length(var_label_list)) {
    stop("The lengths of 'var_name_list' and 'var_label_list' must be the same")
  }
  if (is.null(var_changes)) {
    var_changes <- rep(list("level"), length(var_name_list))
  }
  if (length(var_name_list) != length(var_changes)) {
    stop("The lengths of 'var_name_list' and 'var_changes' must be the same")
  }
  valid_changes <- c("mom", "qoq", "yoy", "level")
  if (any(sapply(var_changes, function(x) !is.null(x) && !x %in% valid_changes))) {
    stop("Elements of 'var_changes' must be 'mom', 'qoq', 'yoy', or 'level'")
  }

  # Determine the frequency of the data
  date_diffs <- diff(data$date)
  median_diff <- median(date_diffs)

  # Determine the lag for each change type
  determine_lag <- function(change_type) {
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

  # Apply the transformation to each variable
  for (i in seq_along(var_name_list)) {
    var_name <- var_name_list[i]
    change_type <- var_changes[i]
    lag_days <- determine_lag(change_type)
    if (lag_days > 0) {
      data <- data %>%
        mutate(!!sym(var_name) := (get(var_name, data) / lag(get(var_name, data), lag_days) - 1) * 100)
    }
  }

  # Reshape data to long format
  long_data <- data %>%
    select(date, one_of(var_name_list)) %>%
    gather(key = "variable", value = "value", -date) %>%
    mutate(
      variable_label = factor(variable, levels = var_name_list, labels = var_label_list)
    ) %>%
    filter(date >= as.Date(start_date) & date <= as.Date(end_date))

  # Base plot
  p <- ggplot(long_data, aes(x = date, y = value, color = variable_label))
    geom_line()

  # Add recession bars if recession_bars is TRUE
  if (recession_bars) {
    p <- p + geom_rect(data = get_recession_periods(data) %>%
                         filter(start >= as.Date(start_date) & end <= as.Date(end_date)),
                       aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5, inherit.aes = FALSE)
  }

  # Add Loess smooth line if include_smooth is TRUE
  if(include_smooth) {
    p <- p + geom_smooth(se = FALSE, method = "loess", span = lof_span)
  }

  # Custom color palette
  custom_colors <- c("blue3", "red3", "green4", "darkgrey", "darkorange2", "yellow3", "purple2", "sienna4", "deepskyblue3", "orchid2")

  # Modify x-axis scale based on the use of month or year intervals
  if (use_month_intervals) {
    p <- p + scale_x_date(breaks = month_breaks_lineplot_x_axis(start_date, end_date, x_axis_breaks),
                          date_labels = "%Y-%m")
  } else {
    p <- p + scale_x_date(breaks = year_breaks_lineplot_x_axis(data$date, x_axis_breaks),
                          labels = date_format("%Y"))
  }

  # Add additional plot features
  p <- p +
    xlab(x_axis_title) +
    ylab(y_axis_title) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major = element_line(color = "#d3d3d3", linewidth = 0.2), # Light gray color for major gridlines
      panel.grid.minor = element_line(color = "#d3d3d3", linewidth = 0.1),  # Light gray color for minor gridlines
      legend.key.height = unit(1, "lines"),  # Adjust the height of the legend keys if needed
      legend.text = element_text(margin = margin(t = 0, b = 0)),  # Set top and bottom margin, adjust line height
      legend.spacing.y = unit(2, "lines"),  # Adjust spacing between legend keys
      legend.position = "bottom",  # Adjust if needed to place the legend on the right
      legend.margin = margin(t = -10, b = 0, l = 0, r = 0) # Reduce space around the legend
    ) +
    ggtitle(title) +
    scale_color_manual(values = custom_colors) +
    guides(color = guide_legend(title = NULL))

  # Apply diagonal labels if diagonal_x_labels is TRUE
  if (diagonal_x_labels) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

  # Add subtitle if provided
  if (!is.null(subtitle)) {
    p <- p + labs(subtitle = subtitle)
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
