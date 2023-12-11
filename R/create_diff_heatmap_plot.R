#' Create a Differential Heatmap Plot
#'
#' This function generates a heatmap plot that visualizes the difference between two variables over time. It integrates recession data and offers various customization options.
#'
#' @param data A data frame containing the dataset for plotting. Must include a 'date' column.
#' @param var_1_name Name of the first variable in 'data' to be used in the differential calculation.
#' @param var_2_name Name of the second variable in 'data' to be used in the differential calculation.
#' @param start_year The starting year for the plot.
#' @param end_year The ending year for the plot.
#' @param x_axis_breaks Breaks for the x-axis, usually years.
#' @param title Title of the heatmap.
#' @param frequency The frequency of data points ('weekly' or 'monthly').
#' @param change_space Determines the type of data transformation ('level' or 'yoy' for year-over-year).
#' @param flip_colors A boolean indicating whether to flip the default color scheme.
#' @param override_subtitle An optional subtitle for the heatmap, overriding the default.
#' @param include_cell_numbers A boolean indicating whether to include cell numbers in the heatmap.
#' @param scaling_power The power to which the cell numbers are scaled (relevant only if include_cell_numbers is TRUE).
#' @param num_decimals The number of decimal places to include in cell number labels (if applicable).
#'
#' @return A ggplot object representing the differential heatmap.
#' @export
#'
#' @examples
#' # Assuming 'data' is your dataset with required columns
#' diff_heatmap_plot <- create_diff_heatmap_plot(data, "var1", "var2", 2010, 2020, 1, "Differential Heatmap Title",
#'                                               "monthly", "level", FALSE, NULL, FALSE, NULL, 2)
#' print(diff_heatmap_plot)
create_diff_heatmap_plot <- function(data, var_1_name, var_2_name, start_year, end_year, x_axis_breaks, title,
                                     frequency, change_space, flip_colors = FALSE, override_subtitle = NULL,
                                     include_cell_numbers = FALSE, scaling_power = NULL, num_decimals = NULL) {

  # Get recession data and merge with existing data
  data <- add_recession_data(data)

  # Validate inputs
  if (!frequency %in% c('weekly', 'monthly')) {
    stop("Invalid frequency: must be 'weekly' or 'monthly'")
  }
  if (!change_space %in% c('level', 'yoy')) {
    stop("Invalid change_space: must be 'level' or 'yoy'")
  }
  if (include_cell_numbers && change_space == 'level' && is.null(scaling_power)) {
    stop("scaling_power must be provided for level heatmaps when include_cell_numbers is TRUE.")
  }
  if (include_cell_numbers && change_space == 'yoy' && !is.null(scaling_power)) {
    stop("scaling_power must be NULL for YoY heatmaps.")
  }

  # Determine lag period based on frequency
  lag_period <- ifelse(frequency == 'monthly', 12, 52)  # Monthly = 12, Weekly = 52

  # Prepare the data based on frequency
  period_label <- if (frequency == 'monthly') {
    factor(lubridate::month(data$date, label = TRUE, abbr = TRUE), levels = month.abb)
  } else { # weekly
    factor(data$week)
  }

  heatmap_data <- data %>%
    mutate(
      var_1 = get(var_1_name, .),
      var_2 = get(var_2_name, .),
      level_variable = var_1 - var_2,
      yoy_change_var_1 = (var_1 / lag(var_1, lag_period) - 1) * 100,
      yoy_change_var_2 = (var_2 / lag(var_2, lag_period) - 1) * 100,
      yoy_diff = yoy_change_var_1 - yoy_change_var_2,
      period = period_label,
      year = year(date)
    ) %>%
    filter(year >= start_year & year <= end_year)

  # Process decimals
  label_format <- paste0("%.", num_decimals, "f")

  if(change_space == "level") {
    # Reshape data
    heatmap_data_reshaped <- heatmap_data %>%
      select(year, period, level_variable) %>%
      spread(key = year, value = level_variable)

    # Convert to long format
    heatmap_data_long <- heatmap_data_reshaped %>%
      gather(key = "year", value = "level_variable", -period) %>%
      mutate(year = as.numeric(year))

    # Cap level_variable values
    median_value <- median(heatmap_data_long$level_variable, na.rm = TRUE)
    first_quartile <- quantile(heatmap_data_long$level_variable, 0.25, na.rm = TRUE)
    third_quartile <- quantile(heatmap_data_long$level_variable, 0.75, na.rm = TRUE)
    iqr_value <- IQR(heatmap_data_long$level_variable, na.rm = TRUE)
    non_outlier_max <- third_quartile + (1.5 * iqr_value)
    non_outlier_min <- first_quartile - (1.5 * iqr_value)
    heatmap_data_long$capped_level_variable <- pmax(pmin(heatmap_data_long$level_variable, non_outlier_max), non_outlier_min)

    # Determine scale factor and apply it if include_cell_numbers is TRUE
    scale_factor <- ifelse(include_cell_numbers, 10^scaling_power, 1)
    heatmap_data_long$scaled_level_variable <- heatmap_data_long$level_variable / scale_factor
    label_data <- heatmap_data_long$scaled_level_variable

    # Color scheme
    low_color <- if(flip_colors) "blue3" else "darkred"
    high_color <- if(flip_colors) "darkred" else "blue3"

    # Create heatmap plot
    heatmap_plot <- ggplot(heatmap_data_long, aes(x = year, y = period, fill = capped_level_variable)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = low_color, high = high_color, mid = "white",
                           midpoint = median_value, name = "Capped Level", na.value = "white",
                           labels = scales::number_format(big.mark = ",")) +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5), axis.title.x = element_blank(),
            axis.title.y = element_blank(), legend.position = "right",
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", colour = "white")) +
      scale_x_continuous(breaks = heatmap_breaks_x_axis(heatmap_data_long$year, x_axis_breaks),
                         labels = two_digit_year) +
      labs(fill = "Level Variable", title = title)

    # Adjust y-axis breaks for weekly data
    if (frequency == 'weekly') {
      heatmap_plot <- heatmap_plot + scale_y_discrete(breaks = seq(2, 52, by = 2))
    }

    # Add cell numbers if requested
    if (include_cell_numbers) {
      subtitle_text <- ifelse(!is.null(override_subtitle), override_subtitle,
                              paste("Cells labeled as 10^", scaling_power, " units", sep = ""))

      heatmap_plot <- heatmap_plot +
        geom_text(aes(label = ifelse(is.na(label_data), "",
                                     sprintf(label_format, label_data))),
                  size = 2, vjust = 0.5, hjust = 0.5) +
        labs(subtitle = subtitle_text)
    }

    return(heatmap_plot)
  }

  else {
    # Reshape data
    heatmap_data_reshaped <- heatmap_data %>%
      select(year, period, yoy_diff) %>%
      spread(key = year, value = yoy_diff)

    # Convert to long format
    heatmap_data_long <- heatmap_data_reshaped %>%
      gather(key = "year", value = "yoy_diff", -period) %>%
      mutate(year = as.numeric(year))

    # Cap level_variable values
    median_value <- median(heatmap_data_long$yoy_diff, na.rm = TRUE)
    third_quartile <- quantile(heatmap_data_long$yoy_diff, 0.75, na.rm = TRUE)
    iqr_value <- IQR(heatmap_data_long$yoy_diff, na.rm = TRUE)
    non_outlier_max <- third_quartile + (1.5 * iqr_value)
    heatmap_data_long$capped_level_variable <- pmin(heatmap_data_long$yoy_diff, non_outlier_max)

    # Scale factor is 1 since this is yoy
    scale_factor <- 1
    heatmap_data_long$scaled_level_variable <- heatmap_data_long$yoy_diff / scale_factor
    label_data <- heatmap_data_long$scaled_level_variable

    # Color scheme
    low_color <- if(flip_colors) "blue3" else "darkred"
    high_color <- if(flip_colors) "darkred" else "blue3"

    # Create heatmap plot
    heatmap_plot <- ggplot(heatmap_data_long, aes(x = year, y = period, fill = capped_level_variable)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = low_color, high = high_color, mid = "white",
                           midpoint = median_value, name = "Capped \nYoY % Change", na.value = "white",
                           labels = scales::number_format(big.mark = ",")) +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5), axis.title.x = element_blank(),
            axis.title.y = element_blank(), legend.position = "right",
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", colour = "white")) +
      scale_x_continuous(breaks = heatmap_breaks_x_axis(heatmap_data_long$year, x_axis_breaks),
                         labels = two_digit_year) +
      labs(fill = "Level Variable", title = title)

    # Adjust y-axis breaks for weekly data
    if (frequency == 'weekly') {
      heatmap_plot <- heatmap_plot + scale_y_discrete(breaks = seq(2, 52, by = 2))
    }

    # Add cell numbers if requested
    if (include_cell_numbers) {
      heatmap_plot <- heatmap_plot +
        geom_text(aes(label = ifelse(is.na(label_data), "",
                                     sprintf(label_format, label_data))),
                  size = 2, vjust = 0.5, hjust = 0.5) +
        labs(subtitle = ifelse(!is.null(override_subtitle), override_subtitle, "YoY % Change"))
    }

    return(heatmap_plot)
  }
}
