# Function to create X-axis breaks for heatmaps at N% intervals
heatmap_breaks_x_axis <- function(range, n) {
  seq(from = floor(min(range) / n) * n, to = ceiling(max(range) / n) * n, by = n)
}
