#' @title Calculate Sharpe ratio optimized PF weights
#' @description The function can optimize a time series of prices in order to
#' get portfolio weights that deliver the highest possible Sharpe ratio. As this
#' function optimizes the Sharpe Ratio, which is calculated with a seperate
#' function in this package, the \code{sharpe function} needs to be loaded as
#' well.
#' @param data A \code{data frame} or \code{matrix} that contains the time
#' series of prices.
#' @return A \code{data frame} with containing
#' time series of the portfolio performance.
#' @export
#' @examples
#' optimpf(EuStockMarkets[, 1:3])
######## Sharperatio optimized -  Pure Equity Portfolio ########################
######## Takes a dataframe with all equity indices as input ####################

optimpf <- function(data) {

    #get past returns
    returns <- data.frame()

    for (c in 1:ncol(data)) {
        for (r in 1:nrow(data) - 1) {
            returns[r, c] <- (data[r + 1, c] - data[r, c]) / data[r, c]
        }
    }

# make sure, that the sum of the weights of the PF components adds up to 1, ####

    startingweights <- as.matrix(rep(1 / ncol(data),
                                     length.out = (ncol(data) - 1)))

    expectedreturn <- as.matrix(rep(0, length.out = ncol(data)))

######### pac the returns and covariances into  matrices #######################

    returnmatrix <- as.matrix(returns[, ])
    covmatrix <- cov(returnmatrix)

######### compute the expected returns for every security ######################

    for (c in 1:ncol(returns)) {
        expectedreturn[c, 1] <- mean(returns[,c])
    }

######### Find Optimal PF-weights given the lower bound of 0 and the ###########
######### Sharpe - Ratio function defined above. ###############################

    optweights <- optim(par = as.vector(startingweights),
                        fn = sharpe,
                        expectedreturn = expectedreturn,
                        covmatrix = covmatrix,
                        control = list(fnscale = -1),
                        lower = 0,
                        method = "L-BFGS-B")

    optweights <- as.matrix(optweights$par)
    optweights <- rbind(optweights, 1 - sum(optweights))


######### Function to standardize PF time series to a starting value of 100 ####

    indexportfolio <- data.frame()

    for (c in 1:ncol(data)) {
        indexportfolio[1, c] <- 100
    }

    for (c in 1:ncol(data)) {
        for (r in 1:nrow(returns)) {
            indexportfolio[r + 1, c] <- indexportfolio[r, c] *
                (1 + returns[r, c])
        }
    }

    colnames(indexportfolio) <- NULL
    rownames(indexportfolio) <- NULL

######### Form Portfolio with the Sharpe ratio optimal weights #################

    portfolio <- data.frame()
    for (r in 1:nrow(indexportfolio)) {
        portfolio[r, 1] <- as.numeric(as.matrix(indexportfolio[r, ]) %*%
                                          optweights)
    }


######### The output of this function is a PF time series indexed to 100 #######

    return(portfolio)

######### end of optimpf function ##############################################
################################################################################
}


#' @title Generate a weighted portfolio of two assets
#' @description The \code{function} takes two time series of asset prices as
#' input and estimates the portfolio evaluation based on the weight provided by
#' the user.
#' @param equity A \code{data frame} or \code{matrix} with a time series of
#' equity prices.
#' @param debt A \code{data frame} or \code{matrix} with a time series of
#' debt prices.
#' @param equityaspercent A \code{numeric input} indicating which proportion of
#' the PF should be invested in the first asset.
#' @return A \code{data frame} containing the time series of the portfolio
#' performance.
#' @export
######### Equity + Debt Portfolio ##############################################

equityanddebtpf <- function(equity, debt, equityaspercent) {

    bondindex <- data.frame(matrix(NA,
                                   ncol = 2,
                                   nrow = nrow(equity)))
    bondindex[, 1] <- debt
    bondindex[, 2] <- debt

    debt <- optimpf(bondindex)
    portfolio <- data.frame()

    for (r in 1:nrow(equity)){
        portfolio[r, 1] <- equity[r, 1] * equityaspercent +
            debt[r, 1] * (1 - equityaspercent)
    }

    return(portfolio)
}

#' @title Scale a time series of prices to a starting value of 100.
#' @description The \code{function} takes a \code{data frame} containing
#' security prices and scales this series to a starting value of 100.
#' @param data A \code{data frame} or \code{matrix} with a time series of
#' security prices
#' @return A \code{data frame} with the evolution of the indexed PF
#' @export
#' @examples
#' indexpf(EuStockMarkets[, 2:3])
indexpf <- function(data) {

    returns <- data.frame()

    for (c in 1:ncol(data)) {
        for (r in 1:nrow(data) - 1) {
            returns[r, c] <- (data[r + 1, c] - data[r, c]) / data[r, c]
        }
    }

    indexportfolio <- data.frame()

    for (c in 1:ncol(data)) {
        indexportfolio[1, c] <- 100
    }

    for (c in 1:ncol(data)){
        for (r in 1:nrow(returns)){
            indexportfolio[r + 1,c] <- indexportfolio[r, c] *
                (1 + returns[r , c])
        }
    }
    return(indexportfolio)

}

#' @title \code{Function} to get a PF, that follows the risk parity approach.
#' @description The goal of risk parity PFs is to find PF weights, such that the
#' risk contribution of each constutuent is the same for all PF constituents.
#' @param equity A \code{data frame} or \code{matrix} with a time series of
#' security prices
#' @param debt A \code{data frame} or \code{matrix} with a time series of
#' security prices
#' @param commodity A \code{data frame} or \code{matrix} with a time series of
#' security prices
#' @return A \code{list} with the evolution of the portfolio as \code{data frame}
#' and the amount of equity as \code{numeric}.
#' @export
#' @examples riskparitypf(equity = EuStockMarkets[, 1], debt = EuStockMarkets[, 2],
#' commodity = EuStockMarkets[, 3])
riskparitypf <- function(equity, debt, commodity) {

    data <- as.matrix(cbind(equity, debt, commodity))

    startingweights <- as.matrix(rep(1 / ncol(data),
                                     length.out = (ncol(data) - 1)))

    returns <- data.frame()
    for (c in 1:ncol(data)) {
        for (r in 1:nrow(data) - 1) {
            returns[r, c] <- (data[r + 1, c] - data[r, c]) / data[r, c]
        }
    }

    returnmatrix <- as.matrix(returns[, ])
    covmatrix <- cov(returnmatrix)

    optweights <- optim(par = as.vector(startingweights),
                        fn = riskparity,
                        covmatrix = covmatrix,
                        lower = 0,
                        method = "L-BFGS-B")

    optweights <- as.matrix(optweights$par)
    optweights <- rbind(optweights, 1 - sum(optweights))
    riskparityequity <<- as.numeric(optweights[1, 1])
    portfolio <- data.frame(100)

    for (r in 1:nrow(returns)) {
        portfolio[(1 + r), 1] <- portfolio[r, 1] *
            (1 + (returns[r, 1] * optweights[1, 1] +
                      returns[r, 2] * optweights[2, 1] +
                      returns[r, 3] * optweights[3, 1]))
    }
    liste <- list(portfolio,as.numeric(optweights[1, 1]))
    return(liste)
##### end of Risk parity function ##############################################
}


#' @title Function to get a PF, that applies minium variance calculations
#' @description The goal of MVPFs is to find PF weights, that deliver the
#' smallest variance possible.
#' @param data A \code{data frame} or \code{matrix} with a time series of
#' security prices
#' @return A \code{data frame} with the evolution of the portfolio.
#' @export
#' @examples minvarpf(EuStockMarkets[, 1:3])
minvarpf <- function(data) {

    returns <- data.frame()
    for (c in 1:ncol(data)) {
        for (r in 1:nrow(data) - 1) {
            returns[r, c] <- (data[(r + 1), c] - data[r, c]) / data[r, c]
        }
    }

    returnmatrix <- as.matrix(returns[, ])
    covmatrix <- as.matrix(cov(returnmatrix))

    startingweights <- as.matrix(rep(1 / ncol(data),
                                     length.out = (ncol(data) - 1)))

    optweights <- optim(par = as.vector(startingweights),
                        fn = minvar,
                        covmatrix = covmatrix,
                        lower = 0,
                        method = "L-BFGS-B")

    optweights <- as.matrix(optweights$par)
    optweights <- rbind(optweights, 1 - sum(optweights))

    portfolio <- data.frame(100)
    for (r in 1:nrow(returns)) {
        portfolio[(1 + r), 1] <- portfolio[r, 1] *
            (1 + (as.matrix(returns[r, ]) %*% optweights))
    }

    return(portfolio)
### End of Minimum Variance Portfolio function #################################
}



##################### Supporing functions ######################################
## these functions are only used by the above listed ones and not by the user ##



####### Calculate sharpe Ratio for maximization ################################

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



riskparity <- function(covmatrix, par) {

    par <- as.matrix(par)
    par <- rbind(par, 1 - sum(par[, 1]))
    startingweights <- par

    marginalrisk <- covmatrix %*% startingweights
    contribution <- as.matrix(as.numeric((startingweights * marginalrisk))) /
        sqrt(as.numeric((t(startingweights) %*% covmatrix %*% startingweights)))

    portfoliorisk <- sqrt(as.numeric((t(startingweights) %*% covmatrix %*%
                                          startingweights)))


    contributionpercent <- matrix(ncol = 1, nrow = 3)
    for (r in 1:nrow(contribution)) {
        contributionpercent[r, 1] <- contribution[r, 1]/sum(contribution)
    }


###### Workaround to make the optimizer find a stable minimum ##################

    if (par[1, 1] + par[2, 1] > 1){
        portfoliorisk <- 3
    }

    portfoliorisk <- abs((contributionpercent[1, 1] -
                              contributionpercent[2, 1])) +
        abs((contributionpercent[1, 1] -
                 contributionpercent[3, 1]))

    return(portfoliorisk)
}

minvar <- function(covmatrix, par) {

    par <- as.matrix(par)
    par <- rbind(par, 1 - sum(par[, 1]))

    std <- t(par) %*% covmatrix %*% par

    if (par[1, 1] + par[2, 1] > 1){
        std <- 3
    }

    return(std)
}
