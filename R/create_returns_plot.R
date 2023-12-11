# Asset returns plot
create_returns_plot <- function(price_data, start_date, end_date = Sys.Date(), variables, combo_bets = NULL, add_risk_parity = FALSE) {

  # First, call create_returns_table to get the returns data
  returns_data <- create_returns_table(price_data, start_date, end_date, variables, combo_bets, add_risk_parity)

  # Define custom color palette
  custom_colors <- c("blue3", "red3", "green4", "darkgrey", "darkorange2", "yellow3", "purple2", "sienna4", "deepskyblue3", "orchid2")

  # Prepare the data for plotting
  long_returns_data <- reshape2::melt(returns_data, id.vars = "date", variable.name = "Variable", value.name = "Return")

  # Determine the plot title based on the presence of an end_date
  if (is.null(end_date) || end_date == Sys.Date()) {
    plot_title <- sprintf("Returns since %s", format(as.Date(start_date), "%b %d, %Y"))
  } else {
    plot_title <- sprintf("Returns from %s to %s", format(as.Date(start_date), "%b %d, %Y"), format(as.Date(end_date), "%b %d, %Y"))
  }

  # Find the last data point for each variable
  last_points <- long_returns_data %>%
    group_by(Variable) %>%
    filter(date == max(date, na.rm = TRUE)) %>%
    ungroup()

  # Create the plot
  p <- ggplot(long_returns_data, aes(x = date, y = Return, group = Variable, color = Variable)) +
    geom_line() +
    scale_color_manual(values = custom_colors) +
    scale_x_date(limits = c(min(long_returns_data$date), as.Date(max(long_returns_data$date)))) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs(title = plot_title, x = "", y = "Return") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.key = element_blank(),  # Remove the key symbols from the legend
          panel.grid.major = element_line(color = "#d3d3d3", linewidth = 0.2), # Light gray color for major gridlines
          panel.grid.minor = element_line(color = "#d3d3d3", linewidth = 0.1)) + # Light gray color for minor gridlines
    guides(color = guide_legend(override.aes = list(size = 1))) +
    geom_hline(yintercept = 0, size = .25, color = "lightgrey")

  return(p)
}
