#' Create a Two-Axis Line Plot
#'
#' This function generates a line plot for two variables, each with its own y-axis, over a given time period. The plot includes recession bars and supports different types of data transformations.
#'
#' @param data A data frame containing the dataset for plotting.
#' @param variables A vector of two variable names in 'data' to be plotted.
#' @param var_labels A vector of two labels corresponding to the variables.
#' @param start_date The starting date for the plot, formatted as 'YYYY-MM-DD'.
#' @param end_date The ending date for the plot, formatted as 'YYYY-MM-DD'.
#' @param plot_title Title of the plot.
#' @param x_axis_breaks Breaks for the x-axis, usually years.
#' @param var_1_type Type of transformation for the first variable.
#'                  Acceptable values: 'yoy' for year-over-year, 'qoq' for quarter-over-quarter,
#'                  'mom' for month-over-month, or NULL for no transformation.
#' @param var_2_type Type of transformation for the second variable.
#'                  Acceptable values: 'yoy' for year-over-year, 'qoq' for quarter-over-quarter,
#'                  'mom' for month-over-month, or NULL for no transformation.
#' @return A ggplot object representing the two-axis line plot.
#' @export
#'
#' @examples
#' create_two_axis_line_plot(data = monthly_data, variables = c("fed_funds_rate", "ngdp_monthly"),
#'                           var_labels = c("Fed Funds Rate (%)", "NGDP YoY % Change"), start_date = "2000-01-01",
#'                           end_date = "2024-01-01", x_axis_breaks = 5, plot_title = "Fed Funds and NGDP",
#'                           var_1_type = NULL, var_2_type = "yoy")
create_two_axis_line_plot <- function(data, variables, var_labels, start_date, end_date, plot_title, x_axis_breaks, var_1_type = NULL, var_2_type = NULL) {

  # Check if the dataset 'data' exists
  if (!exists("data")) {
    stop("Dataset 'data' not found.")
  }

  # Check if 'data' is a data frame
  if (!is.data.frame(data)) {
    stop("The provided 'data' is not a data frame.")
  }

  # Check if each variable in 'var_name_list' is found in 'data'
  missing_vars <- variables[!variables %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(paste("The following variables are not found in the dataset:", paste(missing_vars, collapse = ", "), "."))
  }

  # Get recession data and merge with existing data
  data <- add_recession_data(data)

  # Check if the correct number of variables and variable labels are provided
  if (length(variables) != 2) {
    stop("Please provide exactly two variables.")
  }
  if (length(var_labels) != 2) {
    stop("Please provide exactly two variable labels.")
  }

  # Convert dates to Date class and ensure data is sorted
  data <- data %>%
    mutate(date = as.Date(date)) %>%
    arrange(date)

  # Validate types for both variables
  valid_types <- c("yoy", "qoq", "mom", NULL)
  if (!is.null(var_1_type) && !(var_1_type %in% valid_types)) {
    stop("var_1_type must be 'yoy', 'qoq', 'mom', or NULL.")
  }
  if (!is.null(var_2_type) && !(var_2_type %in% valid_types)) {
    stop("var_2_type must be 'yoy', 'qoq', 'mom', or NULL.")
  }

  determine_lag <- function(data, change_type) {
    # Determine the frequency of the data
    date_diffs <- diff(data$date)
    median_diff <- median(date_diffs)

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

  apply_change <- function(data, var, change_type) {
    lag_value <- determine_lag(data, change_type)
    if (lag_value > 0) {
      data[[var]] <- (data[[var]] / lag(data[[var]], n = lag_value) - 1) * 100
    }
    return(data)
  }

  # Convert data to change if requested
  if (!is.null(var_1_type)) {
    data <- apply_change(data, variables[1], var_1_type)
  }
  if (!is.null(var_2_type)) {
    data <- apply_change(data, variables[2], var_2_type)
  }

  # Filter data based on the provided date range
  data <- data %>%
    filter(date >= start_date, date <= end_date)

  # Get recession periods
  recession_periods <- get_recession_periods(data)

  # Get the range of the second variable to dynamically create the secondary axis
  range_var2 <- range(data[[variables[2]]], na.rm = TRUE)
  range_var1 <- range(data[[variables[1]]], na.rm = TRUE)

  # Create the plot with recession bars
  p <- ggplot(data, aes(x = date)) +
    geom_rect(data = recession_periods, inherit.aes = FALSE, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.5) +
    geom_line(aes(y = data[[variables[1]]]), color = "blue") +
    geom_line(aes(y = (data[[variables[2]]] - min(range_var2)) / diff(range_var2) * diff(range_var1) + min(range_var1)), color = "red") +
    scale_y_continuous(
      name = var_labels[1],
      labels = label_comma(),
      limits = range_var1,
      sec.axis = sec_axis(~ . * diff(range_var2) / diff(range_var1) + min(range_var2) - min(range_var1) * diff(range_var2) / diff(range_var1),
                          name = var_labels[2], labels = label_comma())
    ) +
    scale_x_date(breaks = year_breaks_lineplot_x_axis(data$date, x_axis_breaks), labels = date_format("%Y")) +
    labs(title = plot_title) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend
      plot.title = element_text(hjust = 0),  # Title to the left
      axis.title.x = element_blank(),  # Remove x-axis label
      panel.grid.major.y = element_blank(),  # Remove horizontal grey lines
      panel.grid.minor.y = element_blank(),  # Remove horizontal grey lines
      axis.title.y = element_text(margin = margin(r = 10), color = "blue"),  # Blue y-axis label
      axis.title.y.right = element_text(margin = margin(l = 10), color = "red")  # Red right y-axis label
    )

  return(p)
}
