# Custom labels function to show only the last two digits of the year
four_digit_year <- function(years) {
  substr(years, 1, 4)
}
