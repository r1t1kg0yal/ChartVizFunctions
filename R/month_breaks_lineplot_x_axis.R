# Function to generate month interval breaks
month_breaks_lineplot_x_axis <- function(start_date, end_date, month_interval) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  seq(start_date, end_date, by = paste0(month_interval, " months"))
}
