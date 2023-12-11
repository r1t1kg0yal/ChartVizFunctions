# Define a custom interpolation function that doesn't interpolate past the last known value
interpolate_no_extrap <- function(x) {
  # Find the index of the last non-NA value
  last_na <- max(which(!is.na(x)))
  # Perform linear interpolation up to the last non-NA value
  x[1:last_na] <- na.approx(x[1:last_na], na.rm = FALSE)
  # Ensure all values past the last known value are set to NA
  if (last_na < length(x)) {
    x[(last_na+1):length(x)] <- NA
  }
  return(x)
}
