#' @title Average (log) return of a time series of prices
#' @description This function generates a time series of log-returns and
#' while requiring as an input a time series of prices. In the end, the return
#' scaled to an annual return by multplying it by 252.
#' @param backtest A \code{data frame} or a \code{matrix} containing time series
#' of prices. If there are several prices provided, only the first one will
#' be used.
#' @return A \code{numeric} output giving the average annualized return as a
#' decimal number.
#' @export
#' @examples
#' averagereturn(backtest = as.matrix(EuStockMarkets[, 1]))
#' averagereturn(backtest = as.data.frame(EuStockMarkets[, 2]))
averagereturn <- function(backtest) {
    portfoliologreturns <- data.frame()

    for (r in 1:(nrow(backtest) - 1)) {
        portfoliologreturns[r, 1] <- log(backtest[r + 1, 1]/backtest[r, 1])
    }

    averagereturn <- mean(portfoliologreturns[, 1]) * 252
    return(averagereturn)

}

#' @title Maximum Drawdown of a time series of prices
#' @description The function evaluates the ex post maximum drawdown, which is
#' defined as the most severe decrease from the previous maximum.
#' @param backtest A \code{data frame} or \code{matrix} containing one time
#' series of prices. If there are several prices provided, only the first one
#' will be used.
#' @return A \code{numeric} output giving the maximum drawdown, as decimal
#' number
#' @export
#' @examples
#' maxdrawdown(backtest = as.matrix(EuStockMarkets[, 1]))
#' maxdrawdown(backtest = as.data.frame(EuStockMarkets[, 2]))
maxdrawdown <- function(backtest){
    trailingmaxdrawdown <- data.frame()

    for (r in 1:(nrow(backtest) - 1)){
        trailingmaxdrawdown[r, 1] <- min(tail(backtest[, 1], -r)) /
            backtest[r, ] - 1
    }

    maxdrawdown <- min(trailingmaxdrawdown[, 1])
    return(maxdrawdown)
}

#' @title Yearly standard deviation of a time series of prices.
#' @description This function estimates the annualized standard deviation of a
#' time series of returns. It needs daily data as an input, as it scales the
#' daily standard deviation by the use of the square root of time rule to an
#' annual proxy.
#' @param backtest A \code{data frame} or \code{matrix} containing one time
#' series of prices. If there are several prices provided, only the first one
#' will be used.
#' @return A \code{numeric} output giving the yearly standard deviation
#' as decimalnumber
#' @export
#' @examples
#' yearlystd(backtest = as.matrix(EuStockMarkets[, 1]))
#' yearlystd(backtest = as.data.frame(EuStockMarkets[, 2]))
yearlystd <- function(backtest){
    portfoliologreturns <- data.frame()

    for (r in 1:(nrow(backtest) - 1)){
        portfoliologreturns[r, 1] <- log(backtest[r + 1, 1] /
                                             backtest[r, 1])
    }

    yearlystd <- sd(portfoliologreturns[, 1]) * sqrt(252)
    return(yearlystd)
}

#' @title Sharpe ratio calculation of a portfolio.
#' @description The function gives the Sharpe ratio of a PF, given its
#' constituents' expected returns, its covariances and weights.
#' @param expectedreturn A \code{matrix} of expected returns.
#' @param covmatrix A \code{matrix} containing the covarinaces of the PF's
#' constituents, with their variance on the main diagonale
#' @param par A \code{matrix} containing the weights of the PF constituents.
#' All weights, except for one, have to be provided, as the last one is choosen,
#' such that the weights sum up to 100 percent.
#' @return \code{Numeric vector} conraining the Sharpe ratio.
#' @export
#' @examples
#' sharpe(expectedreturn = matrix(c(1, 2)),
#' covmatrix = matrix(c(0.004, 0.7, 0.7, 0.002), nrow = 2),
#' par = c(0.2))
sharpe <- function(expectedreturn, covmatrix, par){

######### optim, used, later needs the parameters as vector, but we need it as #
######### matrix here - therefore we have to change it #########################

    par <- as.matrix(par)
    par <- rbind(par, 1 - sum(par[, 1]))

######### Calculate Portfolio return ###########################################

    pfreturn <- t(expectedreturn) %*% par

######### Calculate Portfolio standard deviation ###############################

    pfvar <- t(par) %*% covmatrix %*% par

######### Calculate Sharpe Ratio ###############################################

    sharperatio <- pfreturn / (pfvar ^ 0.5)

    return(sharperatio)
}
