#' Create a Yield Curve Plot
#'
#' This function generates a plot of yield curves for specified financial instruments across different maturities on selected dates. It includes options for customizing the plot appearance.
#'
#' @param data A data frame containing the yield data.
#' @param var_names A vector of variable names in 'data' representing different maturities.
#' @param maturities A vector of labels corresponding to 'var_names', representing the maturity periods.
#' @param plot_dates A vector of dates for which to plot the yield curves.
#' @param plot_title Title of the plot.
#' @param x_axis_breaks Integer indicating the frequency of breaks on the x-axis.
#'
#' @return A ggplot object representing the yield curve plot.
#' @export
#'
#' @examples
#' # Assuming 'weekdaily_data' is your dataset with required columns
#' yield_curve_plot <- create_yield_curve(data = weekdaily_data, plot_title = "Yield Curve",
#'                                        var_names = c("three_month_yield", "six_month_yield", "one_year_yield",
#'                                                      "two_year_yield", "three_year_yield", "five_year_yield",
#'                                                      "seven_year_yield", "ten_year_yield", "twenty_year_yield",
#'                                                      "thirty_year_yield"),
#'                                        maturities = c("3m", "6m", "1y",
#'                                                      "2y", "3y", "5y",
#'                                                      "7y", "10y", "20y",
#'                                                      "30y"),
#'                                        plot_dates = c("2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01",
#'                                                       "2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01"))
#' print(yield_curve_plot)
create_yield_curve <- function(data, var_names, maturities, plot_dates, plot_title, x_axis_breaks = 1) {
  # Check if the required columns are present in the data
  if (!all(var_names %in% names(data))) {
    stop("Not all variables are present in the data frame.")
  }

  custom_colors <- c("red3", "darkorange", "yellow3", "green4", "blue3", "deepskyblue3", "purple2", "orchid2", "darkgrey","sienna4")

  # Ensure the 'date' column is in Date format
  data$date <- as.Date(data$date)

  # Function to find the closest date before the provided date
  closest_date <- function(date, dates) {
    dates <- sort(dates[dates <= date])
    if (length(dates) == 0) {
      return(NA)
    } else {
      return(tail(dates, 1))
    }
  }

  # Convert plot_dates to Date format and find the closest dates
  original_plot_dates <- sort(as.Date(plot_dates))
  data_dates <- data$date
  adjusted_plot_dates <- sapply(original_plot_dates, function(x) closest_date(x, data_dates))

  # Convert adjusted_plot_dates back to Date format after sapply
  adjusted_plot_dates <- as.Date(adjusted_plot_dates, origin = "1970-01-01")

  # Create a mapping of adjusted dates to original dates for legend labels
  date_legend_mapping <- setNames(as.character(original_plot_dates), as.character(adjusted_plot_dates))

  # Convert all var_names columns to numeric if they are not already
  data[var_names] <- lapply(data[var_names], function(x) as.numeric(as.character(x)))

  # Drop attributes from var_names columns
  data[var_names] <- lapply(data[var_names], function(x) { attributes(x) <- NULL; return(x) })

  # Reshape the data to a long format using pivot_longer
  long_data <- data %>%
    select(date, all_of(var_names)) %>%
    pivot_longer(cols = all_of(var_names), names_to = "maturity", values_to = "yield") %>%
    filter(date %in% adjusted_plot_dates) %>%
    mutate(maturity = factor(maturity, levels = var_names, labels = maturities),
           date = as.factor(date_legend_mapping[as.character(date)]))

  # Define breaks for the x-axis based on x_axis_breaks parameter
  x_breaks <- maturities[seq(1, length(maturities), x_axis_breaks)]

  # Plotting the yield curves
  ggplot(long_data, aes(x = maturity, y = yield, group = date, color = date)) +
    geom_line() +
    theme_minimal() +
    labs(title = plot_title,
         x = "Maturity",
         y = "Yield (%)",
         color = "Date") +
    scale_x_discrete(breaks = x_breaks) +
    scale_color_manual(values = custom_colors)
}
