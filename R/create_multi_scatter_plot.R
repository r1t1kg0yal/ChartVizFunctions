#' Create a Multi-Scatter Plot
#'
#' This function generates a scatter plot for multiple variables over time, with options for different plot types and additional smoothing.
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
#' @param plot_type Type of the plot ('level' or 'yoy' for year-over-year).
#' @param y_axis_breaks Break points for the y-axis (optional).
#' @param include_smooth A boolean indicating whether to include a Loess smoothed line.
#' @param lof_span Span of the lof
#' @param y_axis_lower_bound Lower bound for the y-axis (optional).
#' @param y_axis_upper_bound Upper bound for the y-axis (optional).
#'
#' @return A ggplot object representing the multi-scatter plot.
#' @export
#'
#' @examples
#' # Assuming 'data' is your dataset with required columns
#' multi_scatter_plot <- create_multi_scatter_plot(data, c("var1", "var2"), c("Label 1", "Label 2"), 2010, 2020, 1,
#'                                                  "X Axis Title", "Y Axis Title", "Multi-Scatter Plot Title", "level", NULL,
#'                                                  FALSE, NULL, NULL)
#' print(multi_scatter_plot)
create_multi_scatter_plot <- function(data, var_name_list, var_label_list, start_year, end_year,
                                      x_axis_breaks, x_axis_title, y_axis_title, title, plot_type, y_axis_breaks = NULL,
                                      include_smooth = FALSE, lof_span = 0.1,
                                      y_axis_lower_bound = NULL, y_axis_upper_bound = NULL) {

  # Assuming 'data' is your dataframe and 'var_name_list' contains the names of the variables you are plotting
  data <- data %>%
    filter(!is.na(date)) %>%  # Remove NAs in date if necessary
    filter_at(vars(one_of(var_name_list)), all_vars(!is.na(.)))  # Remove NAs in all plotting variables

  # Get recession data and merge with existing data
  data <- add_recession_data(data)

  # Validate inputs
  if (!plot_type %in% c('level', 'yoy')) {
    stop("Invalid plot type: must be 'level' or 'yoy'")
  }

  if (length(var_name_list) != length(var_label_list)) {
    stop("The lengths of 'var_name_list' and 'var_label_list' must be the same")
  }

  # Determine the frequency of the data
  date_diffs <- diff(data$date)
  median_diff <- median(date_diffs)

  # Set the lag based on the frequency
  lag_days <- if (median_diff <= 1) { # Daily
    365
  } else if (median_diff <= 7) { # Weekly
    52
  } else if (median_diff <= 31) { # Monthly
    12
  } else if (median_diff <= 92) { # Quarterly
    4
  } else { # Annual or longer
    1
  }

  # Wrap labels to a specified width
  wrapped_labels <- sapply(var_label_list, function(label) str_wrap(label, width = 20))

  # Reshape data to long format
  long_data <- data %>%
    select(date, one_of(var_name_list)) %>%
    gather(key = "variable", value = "value", -date) %>%
    mutate(
      year = year(date),
      variable_label = factor(variable, levels = var_name_list, labels = wrapped_labels)
    )

  if(plot_type == "level") {
    long_data <- long_data %>%
      filter(year(date) >= start_year & year(date) <= end_year)
  } else if(plot_type == "yoy") {
    long_data <- long_data %>%
      group_by(variable) %>%
      mutate(yoy_change = (value / lag(value, lag_days) - 1) * 100) %>%
      ungroup() %>%
      filter(year(date) >= start_year & year(date) <= end_year, !is.na(yoy_change))
  }

  # Base plot
  y_value <- if (plot_type == "level") "value" else "yoy_change"
  p <- ggplot(long_data, aes(x = date, y = get(y_value, long_data), color = variable_label)) +
    geom_rect(data = get_recession_periods(data) %>%
                filter(start >= as.Date(paste0(start_year, "-01-01")) & end <= as.Date(paste0(end_year, "-12-31"))),
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
    geom_point(size = 0.1)

  # Add Loess smooth line if include_smooth is TRUE
  if(include_smooth && plot_type == "level") {
    p <- p + geom_smooth(se = FALSE, method = "loess", span = lof_span, size = 0.2)
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
      legend.text = element_text(margin = margin(t = 1, b = 1)),  # Set top and bottom margin, adjust line height
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

  if(plot_type == "yoy") {
    p <- p + geom_hline(yintercept = 0, size = .2, color = "black") # Horizontal line at 0%
  }

  # Return the plot
  return(p)
}
