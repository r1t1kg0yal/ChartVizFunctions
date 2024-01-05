#' Create Returns Table for Use in Returns Plot
#'
#' This function calculates returns for given financial variables over a specified time range, handling individual variables, combinations of variables ("combo bets"), and an optional risk parity calculation.
#'
#' @param price_data A data frame with a column named 'date' and other columns representing price data for various financial instruments.
#' @param start_date The start date for calculating returns, formatted as 'YYYY-MM-DD'.
#' @param end_date The end date for calculating returns, formatted as 'YYYY-MM-DD'. Default is the current system date.
#' @param variables A character vector of column names in 'price_data' for which returns are to be calculated.
#' @param combo_bets An optional list of lists, each containing information about a combination bet, including variables and their respective sizes.
#' @param add_risk_parity A logical flag indicating whether to add a column calculating risk parity based returns. Default is FALSE.
#'
#' @return A data frame where each row corresponds to a date and each column to the returns of a specified variable, combination bet, or risk parity calculation.
#' @export
#'
#' @examples
#' # Assuming 'my_price_data' is your dataset with required columns
#' start_date <- "2020-01-01"
#' end_date <- "2020-12-31"
#' variables <- c("AAPL", "MSFT")
#' combo_bets <- list(
#'   list(var_1 = "AAPL", var_2 = "MSFT", size_1 = 0.5, size_2 = 0.5)
#' )
#' returns_table <- create_returns_table(my_price_data, start_date, end_date, variables, combo_bets)
create_returns_table <- function(price_data, start_date, end_date = Sys.Date(), variables, combo_bets = NULL, add_risk_parity = FALSE) {

  # Convert start_date and end_date to Date objects
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Check if price_data has any dates
  if (nrow(price_data) == 0 || is.null(price_data$date)) {
    stop("price_data is empty or does not contain a 'date' column.")
  }

  # Find the closest available date before the start_date if it's not in the data
  if (!start_date %in% price_data$date) {
    before_start_dates <- price_data$date[price_data$date < start_date]
    if (length(before_start_dates) == 0) {
      stop("No available dates before the specified start_date.")
    }
    start_date <- max(before_start_dates, na.rm = TRUE)
  }

  # Handle end_date if explicitly provided and different from Sys.Date()
  if (!is.null(end_date) && end_date != Sys.Date() && !end_date %in% price_data$date) {
    after_end_dates <- price_data$date[price_data$date > end_date]
    if (length(after_end_dates) == 0) {
      stop("No available dates after the specified end_date.")
    }
    end_date <- min(after_end_dates, na.rm = TRUE)
  }

  # Filter the price data for the selected date range
  price_data <- price_data[price_data$date >= start_date & price_data$date <= end_date, ]

  # Initialize an empty data frame to store returns
  returns_table <- data.frame(date = price_data$date)

  # Calculate returns for individual tickers
  for (variable in variables) {
    base_price <- price_data[price_data$date == start_date, variable]
    returns_table[[variable]] <- (price_data[[variable]] / base_price - 1) * 100
  }

  # Calculate returns for combo bets
  for (bet in combo_bets) {
    combo_return <- numeric(nrow(price_data))
    combo_name_parts <- c()

    for (bet_component in bet) {
      var <- bet_component$var
      size <- bet_component$size

      # Append variable name and size to the combo name
      combo_name_parts <- c(combo_name_parts, paste0((size * 100), "% ", var))

      # Calculate and accumulate weighted return for each variable in the combo
      var_return <- (price_data[[var]] / price_data[price_data$date == start_date, var] - 1) * 100
      combo_return <- combo_return + var_return * size
    }

    # Combine the parts of the combo name
    combo_name <- paste(combo_name_parts, collapse = " / ")

    # Add combo return to the returns table
    returns_table[[combo_name]] <- combo_return
  }

  # Add Risk Parity column if requested
  if(add_risk_parity) {
    risk_parity_return <- .25 * ((price_data[["SPY"]] / price_data[price_data$date == start_date, "SPY"] - 1) * 100) +
      .35 * ((price_data[["TLT"]] / price_data[price_data$date == start_date, "TLT"] - 1) * 100) +
      .35 * ((price_data[["LTPZ"]] / price_data[price_data$date == start_date, "LTPZ"] - 1) * 100) +
      .10 * ((price_data[["GLD"]] / price_data[price_data$date == start_date, "GLD"] - 1) * 100) +
      .15 * ((price_data[["HAP"]] / price_data[price_data$date == start_date, "HAP"] - 1) * 100)
    returns_table[["Risk Parity"]] <- risk_parity_return
  }

  # Return the returns table
  return(returns_table)
}
