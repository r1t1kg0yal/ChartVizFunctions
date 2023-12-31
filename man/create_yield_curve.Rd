% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_yield_curve.R
\name{create_yield_curve}
\alias{create_yield_curve}
\title{Create a Yield Curve Plot}
\usage{
create_yield_curve(
  data,
  var_names,
  maturities,
  plot_dates,
  plot_title,
  x_axis_breaks = 1
)
}
\arguments{
\item{data}{A data frame containing the yield data.}

\item{var_names}{A vector of variable names in 'data' representing different maturities.}

\item{maturities}{A vector of labels corresponding to 'var_names', representing the maturity periods.}

\item{plot_dates}{A vector of dates for which to plot the yield curves.}

\item{plot_title}{Title of the plot.}

\item{x_axis_breaks}{Integer indicating the frequency of breaks on the x-axis.}
}
\value{
A ggplot object representing the yield curve plot.
}
\description{
This function generates a plot of yield curves for specified financial instruments across different maturities on selected dates. It includes options for customizing the plot appearance.
}
\examples{
# Assuming 'weekdaily_data' is your dataset with required columns
create_yield_curve(data = weekdaily_data, plot_title = "Yield Curve",
                   var_names = c("three_month_yield", "six_month_yield", "one_year_yield", "two_year_yield", "three_year_yield", "five_year_yield",
                                  "seven_year_yield", "ten_year_yield", "twenty_year_yield", "thirty_year_yield"),
                   maturities = c("3m", "6m", "1y", "2y", "3y", "5y", "7y", "10y", "20y", "30y"),
                   plot_dates = c("2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01",
                                  "2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01"))
}
