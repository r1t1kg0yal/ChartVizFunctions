# Function to generate year interval breaks
year_breaks_lineplot_x_axis <- function(date_range, n){
  start_year <- floor(lubridate::year(min(date_range)) / n) * n
  end_year <- ceiling(lubridate::year(max(date_range)) / n) * n
  seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(end_year, "-01-01")), by = paste0(n, " years"))
}
