% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract}
\alias{extract}
\title{Extract and combine estimates and goodness-of-fit statistics from several
statistical models.}
\usage{
extract(models, statistic = "std.error", conf_level = 0.95,
  coef_map = NULL, coef_omit = NULL, gof_map = NULL,
  gof_omit = NULL, add_rows = NULL, stars = NULL, fmt = "\%.3f")
}
\arguments{
\item{models}{a single model object or a (potentially named) list of models
to summarize}

\item{statistic}{string name of the statistic to include in parentheses
below estimates. Must be either "conf.int", or one of the column names
produced by the `broom::tidy` function. Typical values include: "std.error",
"conf.int", "statistic", "p.value".}

\item{conf_level}{confidence level to use for confidence intervals}

\item{coef_map}{named character vector. Names refer to the original variable
names. Values refer to the variable names that will appear in the table.
Coefficients which are omitted from this vector will be omitted from the
table. The table will be ordered in the same order as this vector.}

\item{coef_omit}{string regular expression. Omits all matching coefficients
from the table (using `stringr::str_detect`).}

\item{gof_map}{data.frame in the same format as `gtsummary::gof_map`}

\item{gof_omit}{string regular expression. Omits all matching gof statistics from
the table (using `stringr::str_detect`).}

\item{add_rows}{list of character vectors, each of length equal to the number
of models + 1.}

\item{stars}{NULL for no significance stars. TRUE for default significance
stars (*=.1, **=.05, ***=.01). Named numeric vector for custom significance
stars: c('*' = .1, '+' = .05).}

\item{fmt}{string which specifies how numeric values will be rounded. This
string is passed to the `sprintf` function. '\%.3f' will keep 3 digits after
the decimal point with trailing zero. '\%.5f' will keep 5 digits. '\%.3e' will
use exponential notation. See `?sprintf` for more options.}
}
\description{
Extract and combine estimates and goodness-of-fit statistics from several
statistical models.
}
