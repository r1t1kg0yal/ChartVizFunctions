#' Create a Line Plot
#'
#' This function generates a line plot for a given variable over time, with options for different types of plots and additional smoothing.
#'
#' @param data A data frame containing the dataset for plotting. Must include a 'date' column.
#' @param var_name Name of the variable in 'data' to be plotted.
#' @param start_year The starting year for the plot.
#' @param end_year The ending year for the plot.
#' @param x_axis_breaks Breaks for the x-axis, usually years.
#' @param x_axis_title Title for the x-axis.
#' @param y_axis_title Title for the y-axis.
#' @param title Title of the plot.
#' @param plot_type Type of the plot ('level' or 'yoy' for year-over-year).
#' @param y_axis_breaks Break points for the y-axis (optional).
#' @param include_smooth A boolean indicating whether to include a Loess smoothed line.
#' @param y_axis_lower_bound Lower bound for the y-axis (optional).
#' @param y_axis_upper_bound Upper bound for the y-axis (optional).
#'
#' @return A ggplot object representing the line plot.
#' @export
#'
#' @examples
#' # Assuming 'data' is your dataset with required columns
#' line_plot <- create_line_plot(data, "var1", 2010, 2020, 1, "X Axis Title", "Y Axis Title",
#'                              "Line Plot Title", "level", NULL, TRUE, NULL, NULL)
#' print(line_plot)
create_line_plot <- function(data, var_name, start_year, end_year, x_axis_breaks,
                             x_axis_title, y_axis_title, title, plot_type, y_axis_breaks = NULL, include_smooth = TRUE,
                             y_axis_lower_bound = NULL, y_axis_upper_bound = NULL) {

  # Get recession data and merge with existing data
  data <- add_recession_data(data)

  # Validate inputs
  if (!plot_type %in% c('level', 'yoy')) {
    stop("Invalid plot type: must be 'level' or 'yoy'")
  }

  # Determine the frequency of the data
  date_diffs <- diff(data$date)
  median_diff <- median(date_diffs)

  # Set the lag based on the frequency
  if (median_diff <= 1) { # Daily
    lag_days <- 365
  } else if (median_diff <= 7) { # Weekly
    lag_days <- 52
  } else if (median_diff <= 31) { # Monthly
    lag_days <- 12
  } else if (median_diff <= 92) { # Quarterly
    lag_days <- 4
  } else { # Annual or longer
    lag_days <- 1
  }

  plot_data <- data %>%
    mutate(
      level_variable = get(var_name, data),
      month = factor(lubridate::month(date, label = TRUE, abbr = TRUE), levels = month.abb),
      year = year(date),
      yoy_change = (get(var_name, data) / lag(get(var_name, data), lag_days) - 1) * 100
    ) %>%
    filter(!is.na(yoy_change)) # Remove NA values for YoY change

  if(plot_type == "level"){
    # Base plot
    p <- ggplot(subset(plot_data, year(date) >= start_year & year(date) <= end_year), aes(x = date, y = level_variable)) +
      geom_rect(data = get_recession_periods(data) %>%
                  filter(start >= as.Date(paste0(start_year, "-01-01")) & end <= as.Date(paste0(end_year, "-12-31"))),
                aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
      geom_line()

    # Add Loess smooth line if include_smooth is TRUE
    if(include_smooth) {
      p <- p + geom_smooth(se = FALSE, method = "loess", span = 0.1)
    }

    # Add additional plot features
    p <- p +
      scale_x_date(breaks = year_breaks_lineplot_x_axis(plot_data$date, x_axis_breaks), labels = date_format("%Y")) +
      xlab(x_axis_title) +
      ylab(y_axis_title) +
      theme_minimal(base_size = 10) +
      theme(
        panel.grid.major = element_line(color = "#d3d3d3", linewidth = 0.2), # Light gray color for major gridlines
        panel.grid.minor = element_line(color = "#d3d3d3", linewidth = 0.1)  # Light gray color for minor gridlines
      ) +
      ggtitle(title)
  }

  if(plot_type == "yoy"){
    # Base plot
    p <- ggplot(subset(plot_data, year(date) >= start_year & year(date) <= end_year), aes(x = date, y = yoy_change)) +
      geom_rect(data = get_recession_periods(data) %>%
                  filter(start >= as.Date(paste0(start_year, "-01-01")) & end <= as.Date(paste0(end_year, "-12-31"))),
                aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
      geom_line()

    # Add Loess smooth line if include_smooth is TRUE
    if(include_smooth) {
      p <- p + geom_smooth(se = FALSE, method = "loess", span = 0.1)
    }

    # Add additional plot features
    p <- p +
      scale_x_date(breaks = year_breaks_lineplot_x_axis(plot_data$date, x_axis_breaks), labels = date_format("%Y")) +
      xlab(x_axis_title) +
      ylab(y_axis_title) +
      theme_minimal() +
      theme_minimal(base_size = 10) +
      theme(
        panel.grid.major = element_line(color = "#d3d3d3", linewidth = 0.2), # Light gray color for major gridlines
        panel.grid.minor = element_line(color = "#d3d3d3", linewidth = 0.1)  # Light gray color for minor gridlines
      ) +
      geom_hline(yintercept = 0, size = .1, color = "black") + # Horizontal line at 0%
      ggtitle(title)
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
