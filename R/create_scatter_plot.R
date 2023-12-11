create_scatter_plot <- function(data, x_var, y_var, x_label, y_label, title = NULL,
                                highlight_dates = NULL, highlight_latest = FALSE,
                                include_lof = NULL, log_x = FALSE, log_y = FALSE,
                                start_date = NULL, end_date = NULL,
                                x_yoy = FALSE, y_yoy = FALSE, date_ranges = NULL) {

  library(ggplot2)
  library(dplyr)

  # Ensure data is ordered by date if x_var or y_var is a date
  if (is.Date(data[[x_var]]) || is.Date(data[[y_var]])) {
    data <- data %>% arrange(.data[[if_else(is.Date(data[[x_var]]), x_var, y_var)]])
  }

  # Validate include_lof parameter
  if (!is.null(include_lof) && !(include_lof %in% c("lm", "loess"))) {
    stop("Invalid method for include_lof: must be 'lm', 'loess', or NULL")
  }

  if(is.Date(data[[x_var]]) & log_x == TRUE) stop("log_x must be FALSE if x_var is a date")

  # Ensure data is ordered by date
  data <- data %>% arrange(date)

  # Store the original highlight dates for legend labels
  original_highlight_dates <- highlight_dates

  # Check highlight_dates and replace with closest dates if not found
  if (!is.null(highlight_dates)) {
    highlight_dates <- sapply(highlight_dates, function(date) {
      date <- as.Date(date)
      if (!date %in% data$date) {
        # Find the index of the closest date
        closest_idx <- which.min(abs(difftime(date, data$date)))
        # Replace with the closest date
        return(data$date[closest_idx])
      } else {
        return(date)
      }
    })
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

  # Filter the data based on start_date and end_date if provided
  if (!is.null(start_date)) {
    data <- data %>% filter(date >= start_date)
  }

  if (!is.null(end_date)) {
    data <- data %>% filter(date <= end_date)
  }

  # Apply log transformation if requested and if the variable is numeric
  if (log_x && !is.Date(data[[x_var]])) {
    data[[x_var]] <- log(data[[x_var]], base = exp(1))
    x_label <- paste("Log of", x_label)
  }
  if (log_y && !is.Date(data[[y_var]])) {
    data[[y_var]] <- log(data[[y_var]], base = exp(1))
    y_label <- paste("Log of", y_label)
  }

  # Calculate YoY change if requested
  if(x_yoy) {
    data[[x_var]] <- (data[[x_var]] / lag(data[[x_var]], n = lag_days) - 1) * 100
    x_label <- paste("YoY % Change in", x_label)
  }
  if(y_yoy) {
    data[[y_var]] <- (data[[y_var]] / lag(data[[y_var]], n = lag_days) - 1) * 100
    y_label <- paste("YoY % Change in", y_label)
  }

  # Filter out rows with NA in x_var or y_var
  data <- data %>% filter(!is.na(data[[x_var]]) & !is.na(data[[y_var]]))

  # Determine the latest non-missing data point for the specified variables
  if(highlight_latest) {
    latest_date <- max(data$date[!is.na(data[[x_var]]) & !is.na(data[[y_var]])], na.rm = TRUE)
    highlight_dates <- c(highlight_dates, latest_date)
  }

  # Create a color column for highlighted dates
  highlight_dates <- na.omit(as.Date(highlight_dates))
  data$highlight_color <- ifelse(data$date %in% highlight_dates, as.character(data$date), NA)

  # Label the latest point as "Latest" if highlight_latest is TRUE
  if(highlight_latest && !is.na(latest_date)) {
    data$highlight_color[data$date == latest_date] <- "Latest"
  }

  # Process date_ranges and assign colors, highlight_dates will override date_ranges
  date_range_colors <- c()
  if (!is.null(date_ranges)) {
    for (i in seq_along(date_ranges)) {
      range <- date_ranges[[i]]
      start_range <- as.Date(range[1])
      end_range <- as.Date(range[2])
      in_range <- data$date >= start_range & data$date <= end_range
      # Check if start and end dates are the same
      date_range_label <- if(start_range == end_range) {
        format(start_range, "%Y-%m-%d")
      } else {
        paste(format(start_range, "%Y-%m-%d"), "to", format(end_range, "%Y-%m-%d"))
      }
      # Only color as date range if not already colored as highlight date
      data$highlight_color[in_range & !data$date %in% highlight_dates] <- date_range_label
      date_range_colors[date_range_label] <- rainbow(length(date_ranges))[i]
    }
  }


  # Set highlight colors, with dates in highlight_dates taking precedence over date_ranges
  highlight_colors <- c(rainbow(length(unique(na.omit(highlight_dates)))), date_range_colors)
  names(highlight_colors) <- c(as.character(unique(na.omit(highlight_dates))), names(date_range_colors))

  if("Latest" %in% data$highlight_color) {
    highlight_colors["Latest"] <- "black"
  }

  # Base plot with non-highlighted points in grey and no legend for date ranges
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(aes(color = highlight_color), size = 0.1, show.legend = FALSE) +
    geom_point(data = data[!is.na(data$highlight_color), ], aes(color = highlight_color), size = 2, shape = 16) +
    scale_color_manual(values = highlight_colors, name = "Highlighted Dates") +
    theme(legend.position = "right")

  # Add line of best fit if requested
  if (!is.null(include_lof)) {
    p <- p + geom_smooth(method = include_lof, se = FALSE, color = "black", size = 0.5, span = 0.1)
  }

  # Create the subtitle based on the provided constraints
  subtitle <- if (!is.null(start_date) && !is.null(end_date)) {
    paste(format(start_date, format = "%Y-%m-%d"), "to", format(end_date, format = "%Y-%m-%d"))
  } else if (!is.null(start_date)) {
    paste("Since", format(start_date, format = "%Y-%m-%d"))
  } else if (!is.null(end_date)) {
    paste("Until", format(end_date, format = "%Y-%m-%d"))
  } else {
    NULL  # No subtitle if no date constraints are provided
  }

  # Auto-generate title if not provided
  if (is.null(title)) {
    title_parts <- c()
    if (x_yoy || log_x) {
      title_parts <- c(title_parts, x_label)
    }
    if (y_yoy || log_y) {
      title_parts <- c(title_parts, y_label)
    }
    title <- paste(title_parts, collapse = " and ")
  }

  # Add additional plot features
  p <- p + ylab(y_label) +
    xlab(x_label) +
    ggtitle(title) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major = element_line(color = "#d3d3d3", linewidth = 0.2), # Light gray color for major gridlines
      panel.grid.minor = element_line(color = "#d3d3d3", linewidth = 0.1)  # Light gray color for minor gridlines
    )

  # Set the x and y scales based on whether they are dates or not
  if (is.Date(data[[x_var]])) {
    p <- p + scale_x_date(date_labels = "%Y")
  } else {
    p <- p + scale_x_continuous(labels = number_format(big.mark = ","))
  }
  if (is.Date(data[[y_var]])) {
    p <- p + scale_y_date(date_labels = "%Y")
  } else {
    p <- p + scale_y_continuous(labels = number_format(big.mark = ","))
  }

  # Add the subtitle to the plot
  if (!is.null(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }

  # Return the plot
  return(p)
}
