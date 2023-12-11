add_recession_data <- function(data) {
  # Check if recession data already exists in the data frame
  if ("recession" %in% names(data)) {
    return(data)
  }

  # Fetch recession data from FRED
  recession_data <- fredr(series_id = "USRECDM", frequency = "d") %>%
    as.data.frame() %>%
    dplyr::select(date, value) %>%
    dplyr::rename("recession" = "value")

  # Merge the recession data with the existing data frame
  data_with_recession <- merge(data, recession_data, by = "date", all.x = TRUE)

  return(data_with_recession)
}
