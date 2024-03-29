% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_stats.R
\name{maxdrawdown}
\alias{maxdrawdown}
\title{Maximum Drawdown of a time series of prices}
\usage{
maxdrawdown(backtest)
}
\arguments{
\item{backtest}{A \code{data frame} or \code{matrix} containing one time
series of prices. If there are several prices provided, only the first one
will be used.}
}
\value{
A \code{numeric} output giving the maximum drawdown, as decimal
number
}
\description{
The function evaluates the ex post maximum drawdown, which is
defined as the most severe decrease from the previous maximum.
}
\examples{
maxdrawdown(backtest = as.matrix(EuStockMarkets[, 1]))
maxdrawdown(backtest = as.data.frame(EuStockMarkets[, 2]))
}
