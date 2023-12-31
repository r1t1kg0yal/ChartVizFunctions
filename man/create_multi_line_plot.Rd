% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_multi_line_plot.R
\name{create_multi_line_plot}
\alias{create_multi_line_plot}
\title{Create a Multi-Line Plot}
\usage{
create_multi_line_plot(
  data,
  var_name_list,
  var_label_list,
  start_year,
  end_year,
  x_axis_breaks,
  x_axis_title,
  y_axis_title,
  title,
  var_changes = NULL,
  y_axis_breaks = NULL,
  include_smooth = FALSE,
  y_axis_lower_bound = NULL,
  y_axis_upper_bound = NULL
)
}
\arguments{
\item{data}{A data frame containing the dataset for plotting. Must include a 'date' column.}

\item{var_name_list}{A vector of variable names in 'data' to be plotted.}

\item{var_label_list}{A vector of labels corresponding to the variables for plot legend.}

\item{start_year}{The starting year for the plot.}

\item{end_year}{The ending year for the plot.}

\item{x_axis_breaks}{Breaks for the x-axis, usually years.}

\item{x_axis_title}{Title for the x-axis.}

\item{y_axis_title}{Title for the y-axis.}

\item{title}{Title of the plot.}

\item{var_changes}{A list of transformations to apply to each variable. Acceptable values for each element are 'mom' for month-over-month, 'qoq' for quarter-over-quarter, 'yoy' for year-over-year, or NULL for no change. The list must be the same length as 'var_name_list'.}

\item{y_axis_breaks}{Break points for the y-axis (optional).}

\item{include_smooth}{A boolean indicating whether to include a Loess smoothed line. Applicable only when no transformation is applied (i.e., var_changes is NULL or contains all NULLs).}

\item{y_axis_lower_bound}{Lower bound for the y-axis (optional).}

\item{y_axis_upper_bound}{Upper bound for the y-axis (optional).}
}
\value{
A ggplot object representing the multi-line plot.
}
\description{
This function generates a multi-line plot for a list of variables over time, with options for different types of data transformations and additional smoothing.
}
\examples{
create_multi_line_plot(data = monthly_data, var_name_list = c("bank_assets_sa", "bank_loans", "bank_securities_sa"),
                       start_year = 1980, end_year = 2023, var_label_list = c("Bank Assets", "Bank Loans", "Bank Securities"),
                       x_axis_breaks = 5, x_axis_title = "", y_axis_title = "Billions of Dollars", title = "Commercial Banks",
                       var_changes = NULL, include_smooth = FALSE)
create_multi_line_plot(data = monthly_data, var_name_list = c("bank_assets_sa", "bank_loans", "bank_securities_sa"),
                       start_year = 1980, end_year = 2023, var_label_list = c("Bank Assets", "Bank Loans", "Bank Securities"),
                       x_axis_breaks = 5, x_axis_title = "", y_axis_title = "YoY \% Change", title = "Commercial Banks",
                       var_changes = c("yoy", "yoy", "yoy"), y_axis_breaks = 10, include_smooth = TRUE)
}
