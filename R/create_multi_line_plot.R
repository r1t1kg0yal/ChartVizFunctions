#' Create a Multi-Line Plot
#'
#' This function generates a multi-line plot for a list of variables over time, with options for different types of data transformations and additional smoothing.
#'
#' @param data A data frame containing the dataset for plotting. Must include a 'date' column.
#' @param var_name_list A vector of variable names in 'data' to be plotted.
#' @param var_label_list A vector of labels corresponding to the variables for plot legend.
#' @param start_year The starting year for the plot.
#' @param end_year The ending year for the plot.
#' @param x_axis_breaks Breaks for the x-axis, usually years.
#' @param x_axis_title Title for the x-axis.
#' @param y_axis_title Title for the y-axis.
#' @param title Title of the plot.
#' @param var_changes A list of transformations to apply to each variable. Acceptable values for each element are 'mom' for month-over-month, 'qoq' for quarter-over-quarter, 'yoy' for year-over-year, or NULL for no change. The list must be the same length as 'var_name_list'.
#' @param y_axis_breaks Break points for the y-axis (optional).
#' @param include_smooth A boolean indicating whether to include a Loess smoothed line. Applicable only when no transformation is applied (i.e., var_changes is NULL or contains all NULLs).
#' @param y_axis_lower_bound Lower bound for the y-axis (optional).
#' @param y_axis_upper_bound Upper bound for the y-axis (optional).
#'
#' @return A ggplot object representing the multi-line plot.
#' @export
#'
#' @examples
#' create_multi_line_plot(data = monthly_data, var_name_list = c("bank_assets_sa", "bank_loans", "bank_securities_sa"),
#'                        start_year = 1980, end_year = 2023, var_label_list = c("Bank Assets", "Bank Loans", "Bank Securities"),
#'                        x_axis_breaks = 5, x_axis_title = "", y_axis_title = "Billions of Dollars", title = "Commercial Banks",
#'                        var_changes = NULL, include_smooth = FALSE)
#' create_multi_line_plot(data = monthly_data, var_name_list = c("bank_assets_sa", "bank_loans", "bank_securities_sa"),
#'                        start_year = 1980, end_year = 2023, var_label_list = c("Bank Assets", "Bank Loans", "Bank Securities"),
#'                        x_axis_breaks = 5, x_axis_title = "", y_axis_title = "YoY % Change", title = "Commercial Banks",
#'                        var_changes = c("yoy", "yoy", "yoy"), y_axis_breaks = 10, include_smooth = TRUE)

create_multi_line_plot <- function(data, var_name_list, var_label_list, start_year, end_year,
                                   x_axis_breaks, x_axis_title, y_axis_title, title, var_changes = NULL,
                                   y_axis_breaks = NULL, include_smooth = FALSE,
                                   y_axis_lower_bound = NULL, y_axis_upper_bound = NULL) {

  library(dplyr)
  library(ggplot2)
  library(lubridate)

  # Get recession data and merge with existing data
  data <- add_recession_data(data)

  # Validate inputs
  if (length(var_name_list) != length(var_label_list)) {
    stop("The lengths of 'var_name_list' and 'var_label_list' must be the same")
  }
  if (is.null(var_changes)) {
    var_changes <- rep(list(NULL), length(var_name_list))
  }
  if (length(var_name_list) != length(var_changes)) {
    stop("The lengths of 'var_name_list' and 'var_changes' must be the same")
  }
  valid_changes <- c("mom", "qoq", "yoy", NULL)
  if (any(sapply(var_changes, function(x) !is.null(x) && !x %in% valid_changes))) {
    stop("Elements of 'var_changes' must be 'mom', 'qoq', 'yoy', or NULL.")
  }

  # Determine the frequency of the data
  date_diffs <- diff(data$date)
  median_diff <- median(date_diffs)

  # Determine the lag for each change type
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
      year = year(date),
      variable_label = factor(variable, levels = var_name_list, labels = var_label_list)
    ) %>%
    filter(year(date) >= start_year & year(date) <= end_year)

  # Base plot
  p <- ggplot(long_data, aes(x = date, y = value, color = variable_label)) +
    geom_rect(data = get_recession_periods(data) %>%
                filter(start >= as.Date(paste0(start_year, "-01-01")) & end <= as.Date(paste0(end_year, "-12-31"))),
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
    geom_line()

  # Add Loess smooth line if include_smooth is TRUE
  if(include_smooth) {
    p <- p + geom_smooth(se = FALSE, method = "loess", span = 0.1)
  }

  # Custom color palette
  custom_colors <- c("blue3", "red3", "green4", "darkgrey", "darkorange2", "yellow3", "purple2", "sienna4", "deepskyblue3", "orchid2")

  # Add additional plot features
  p <- p +
    scale_x_date(breaks = year_breaks_lineplot_x_axis(data$date, x_axis_breaks), labels = date_format("%Y")) +
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
