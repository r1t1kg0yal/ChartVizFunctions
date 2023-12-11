# Custom labels function to show only the last two digits of the year
two_digit_year <- function(years) {
  substr(years, 3, 4)
}
