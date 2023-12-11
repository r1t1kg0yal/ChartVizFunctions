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
#' @param var_1_type Type of transformation for the first variable ('level' or 'yoy' for year-over-year).
#' @param var_2_type Type of transformation for the second variable ('level' or 'yoy' for year-over-year).
#'
#' @return A ggplot object representing the two-axis line plot.
#' @export
#'
#' @examples
#' # Assuming 'data' is your dataset with required columns
#' two_axis_plot <- create_two_axis_line_plot(data, c("var1", "var2"), c("Variable 1", "Variable 2"),
#'                                            "2020-01-01", "2020-12-31", "Two-Axis Line Plot", 1, "level", "yoy")
#' print(two_axis_plot)
create_two_axis_line_plot <- function(data, variables, var_labels, start_date, end_date, plot_title, x_axis_breaks, var_1_type, var_2_type) {

  # Get recession data and merge with existing data
  data <- add_recession_data(data)

  # Check if the correct number of variables and variable labels are provided
  if (length(variables) != 2) {
    stop("Please provide exactly two variables.")
  }
  if (length(var_labels) != 2) {
    stop("Please provide exactly two variable labels.")
  }
  # Check the types for both variables
  if (!(var_1_type %in% c("level", "yoy")) || !(var_2_type %in% c("level", "yoy"))) {
    stop("var_1_type and var_2_type must be either 'level' or 'yoy'.")
  }

  # Convert dates to Date class and ensure data is sorted
  data <- data %>%
    mutate(date = as.Date(date)) %>%
    arrange(date)

  # Convert data to yoy if requested
  if (var_1_type == "yoy") {
    data[[variables[1]]] <- (data[[variables[1]]] / lag(data[[variables[1]]], 12) - 1) * 100
  }
  if (var_2_type == "yoy") {
    data[[variables[2]]] <- (data[[variables[2]]] / lag(data[[variables[2]]], 12) - 1) * 100
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
