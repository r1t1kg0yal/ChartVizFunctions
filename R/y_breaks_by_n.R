# Function to create Y-axis breaks at N% intervals including 0%
y_breaks_by_n <- function(y_range, n){
  min_y <- floor(min(y_range, na.rm = TRUE) / n) * n
  max_y <- ceiling(max(y_range, na.rm = TRUE) / n) * n
  seq(min_y, max_y, by = n)
}
