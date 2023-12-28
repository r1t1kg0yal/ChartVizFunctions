#' Calculate Cumulative Percentage Change for Variables Over Specified Intervals
#'
#' This function computes the cumulative percentage change for given variables
#' in a dataframe over specified time intervals.
#'
#' @param df A dataframe containing a date column and other numeric columns for calculation.
#' @param var_names A vector of strings specifying the column names in the dataframe for which
#'                  the cumulative percentage change is to be calculated.
#' @param interval_short A string specifying the type of interval for the calculation.
#'                       It can be "decades" or "years". If this parameter is used,
#'                       `interval_manual` must be NULL.
#' @param interval_manual A list of numeric vectors, each containing two elements
#'                        representing the start and end years of the intervals.
#'                        If this parameter is used, `interval_short` must be NULL.
#' @return A tibble with rows representing the intervals and columns showing the
#'         cumulative percentage change for the specified variables.
#' @examples
#' # Assuming df is a dataframe with a date column and numeric columns "var1" and "var2"
#' df <- data.frame(
#'   date = seq(as.Date("2000-01-01"), by = "year", length.out = 21),
#'   var1 = runif(21, 50, 100),
#'   var2 = runif(21, 20, 80)
#' )
#' results_decades <- get_cumulative_table(df, c("var1", "var2"), interval_short = "decades")
#' results_manual <- get_cumulative_table(df, c("var1", "var2"), interval_manual = list(c(2000, 2009), c(2010, 2019)))
#' @import dplyr
#' @import lubridate
#' @import tibble
#' @export
get_cumulative_table <- function(df, var_names, interval_short = NULL, interval_manual = NULL) {
  if (!is.null(interval_short) && !is.null(interval_manual)) {
    stop("Only one of 'interval_short' or 'interval_manual' should be provided.")
  }

  # Define intervals based on decades or years
  if (!is.null(interval_short)) {
    start_year <- floor(min(year(df$date)) / 10) * 10
    end_year <- ceiling(max(year(df$date)) / 10) * 10
    if (interval_short == "decades") {
      interval_manual <- lapply(seq(start_year, end_year - 10, by = 10), function(x) c(x, x+9))
    } else if (interval_short == "years") {
      interval_manual <- lapply(seq(start_year, end_year), function(x) c(x, x))
    } else {
      stop("interval_short must be 'decades' or 'years'.")
    }
  }

  # Initialize a tibble with the correct column names
  results <- tibble(Interval = character())
  for (var_name in var_names) {
    results[[var_name]] <- numeric()
  }

  # Calculate cumulative percentage change for a given interval and column
  calc_percentage_change <- function(data, start, end, column) {
    subset_data <- data %>% filter(date >= as.Date(paste0(start, "-01-01")) & date <= as.Date(paste0(end, "-12-31")))
    if (nrow(subset_data) < 2) return(NA_real_) # Return NA if less than 2 data points
    start_value <- subset_data[[column]][1]
    end_value <- subset_data[[column]][nrow(subset_data)]
    return((end_value - start_value) / start_value * 100)
  }

  # Loop through each interval and variable to calculate cumulative percentage changes
  for (interval in interval_manual) {
    changes <- sapply(var_names, function(var_name) calc_percentage_change(df, interval[1], interval[2], var_name))
    interval_label <- paste(interval[1], "-", interval[2])
    results <- results %>% add_row(Interval = interval_label, !!!setNames(as.list(changes), var_names))
  }

  return(results)
}
