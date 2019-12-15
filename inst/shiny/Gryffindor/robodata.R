library(reticulate)
library(lubridate)
library(tidyverse)
library(rlist)
library(jsonlite)
library(rtsdata)
library(gryffindorrobo)


use_python(Sys.which("python"))
invest <- import("investpy")
ind_stocks_eu <- list('iShares STOXX Europe 600 Banks UCITS',
                      'iShares STOXX Europe 600 Basic Resources UCITS',
                      'iShares STOXX Europe 600 Chemicals UCITS',
                      'iShares STOXX Europe 600 Construction & Materials',
                      'iShares STOXX Europe 600 Financial Services UCITS',
                      'iShares STOXX Europe 600 Food & Beverage UCITS',
                      'iShares STOXX Europe 600 Health Care UCITS',
                      'iShares STOXX Europe 600 Industrial Goods & Servic',
                      'iShares STOXX Europe 600 Insurance UCITS',
                      'iShares STOXX Europe 600 Oil & Gas UCITS',
                      'iShares STOXX Europe 600 Personal & Household Good',
                      'iShares STOXX Europe 600 Technology UCITS',
                      'iShares STOXX Europe 600 Telecommunications UCITS',
                      'iShares STOXX Europe 600 Utilities UCITS')

ind_stocks_us <- list('STOXX North America 600 Banks USD Price',
                      'STOXX North America 600 Basic Resources USD Price',
                      'STOXX North America 600 Chemicals USD Price',
                      'STOXX North America 600 Construction & Materials U',
                      'STOXX North America 600 Financial Services USD Pri',
                      'STOXX North America 600 Food & Beverage USD Price',
                      'STOXX North America 600 Health Care USD Price',
                      'STOXX North America 600 Industrial Goods & Service',
                      'STOXX North America 600 Insurance USD Price',
                      'STOXX North America 600 Oil & Gas USD Price',
                      'STOXX North America 600 Personal & Household Goods',
                      'STOXX North America 600 Technology USD Price',
                      'STOXX North America 600 Telecommunications USD Pri',
                      'STOXX North America 600 Utilities USD Price')

ind_stocks_asia <- list('STOXX Asia/Pacific 600 Banks USD Price',
                        'STOXX Asia/Pacific 600 Basic Resources USD Price',
                        'STOXX Asia/Pacific 600 Chemicals USD Price',
                        'STOXX Asia/Pacific 600 Construction & Materials US',
                        'STOXX Asia/Pacific 600 Financial Services USD Pric',
                        'STOXX Asia/Pacific 600 Food & Beverage USD Price',
                        'STOXX Asia/Pacific 600 Health Care USD Price',
                        'STOXX Asia/Pacific 600 Industrial Goods & Services',
                        'STOXX Asia/Pacific 600 Insurance USD Price',
                        'STOXX Asia/Pacific 600 Oil & Gas USD Price',
                        'STOXX Asia/Pacific 600 Personal & Household Goods',
                        'STOXX Asia/Pacific 600 Technology USD Price',
                        'STOXX Asia/Pacific 600 Telecommunications USD Pric',
                        'STOXX Asia/Pacific 600 Utilities USD Price')

inds_eu <- list("EU_banks", "EU_resources", "EU_chemicals",
                "EU_construction","EU_financials", "EU_food","EU_health",
                "EU_industrial","EU_insurance", "EU_energy",
                "EU_personal", "EU_tech", "EU_telecom",
                "EU_utilities")
inds_us <- list("US_banks", "US_resources", "US_chemicals",
                "US_construction","US_financials", "US_food","US_health", 
                "US_industrial","US_insurance", "US_energy",
                "US_personal", "US_tech", "US_telecom",
                "US_utilities")
inds_asia <- list("AS_banks", "AS_resources", "AS_chemicals",
                  "AS_construction","AS_financials", "AS_food","AS_health", 
                  "AS_industrial","AS_insurance", "AS_energy",
                  "AS_personal", "AS_tech", "AS_telecom",
                  "AS_utilities")

# Main scraping function


dfs <- list()

data_scrap <- function(stocks,industries,country, yrs, scraping_function){
  for (item in Map(list, stocks,industries)){
    
    df <- scraping_function(item[[1]],
                            country = country,
                            from_date= format((Sys.Date() - years(yrs)), '%d/%m/%Y'),
                            to_date= format(Sys.Date(), '%d/%m/%Y'), as_json = TRUE)
    #df$industry <- rep(item[[2]], nrow(df))
    
    
    df <- jsonlite::fromJSON(df) %>% as.data.frame() %>% subset(select = c('historical.date','historical.close'))
    
    names(df)[names(df) == 'historical.close'] <- item[[2]]
    names(df)[names(df) == 'historical.date'] <- 'Date'
    
    dfs <- list.append(dfs, df)
    
  }
  return(dfs)
}

# function for limited data since Dec 2014 (prior data is not on investing)
data_scrap_ltd <- function(stocks,industries,country, scraping_function){
  for (item in Map(list, stocks,industries)){
    
    df <- scraping_function(item[[1]],
                            country = country,
                            from_date= '11/12/2014',
                            to_date= format(Sys.Date(), '%d/%m/%Y'), as_json = TRUE)
    #df$industry <- rep(item[[2]], nrow(df))
    
    
    df <- jsonlite::fromJSON(df) %>% as.data.frame() %>% subset(select = c('historical.date','historical.close'))
    
    names(df)[names(df) == 'historical.close'] <- item[[2]]
    names(df)[names(df) == 'historical.date'] <- 'Date'
    
    dfs <- list.append(dfs, df)
    
  }
  return(dfs)
}


EU <- data_scrap(ind_stocks_eu, inds_eu, 'Germany', 15,
                 invest$get_etf_historical_data) %>% reduce(inner_join, by = "Date")  
US <- data_scrap_ltd(ind_stocks_us, inds_us, 'world',
                     invest$get_index_historical_data) %>% reduce(inner_join, by = "Date")  
AS <- data_scrap_ltd(ind_stocks_asia, inds_asia, 'world',
                 invest$get_index_historical_data) %>% reduce(inner_join, by = "Date")

# historical data load for US and Asia exceeding the limit of investing.com
#datastatic <- read.csv("staticdata/histstock.csv", sep=";")


#Data for portfolioconstruction
Africa <- as.data.frame(ds.getSymbol.yahoo("EZA", from = "2006-12-08", to = Sys.Date())[,6])
Australia <- as.data.frame(ds.getSymbol.yahoo("EWA", from = "2006-12-08", to = Sys.Date())[,6])
Latinamerica <- as.data.frame(ds.getSymbol.yahoo("ILF", from = "2006-12-08", to = Sys.Date())[,6])
Commodities <- as.data.frame(ds.getSymbol.yahoo("IAU", from = "2006-12-08", to = Sys.Date())[,6])
LongBond <- as.data.frame(ds.getSymbol.yahoo("TLT", from = "2006-12-08", to = Sys.Date())[,6])
ShortBond <- as.data.frame(ds.getSymbol.yahoo("TIP", from = "2006-12-08", to = Sys.Date())[,6])
# Data for Benchmark
sp500 <- as.data.frame(ds.getSymbol.yahoo("^GSPC", from = "2006-12-08", to = Sys.Date())[,6])
stoxx <- as.data.frame(ds.getSymbol.yahoo("FEZ", from = "2006-12-08", to = Sys.Date())[,6])
asia <- as.data.frame(ds.getSymbol.yahoo("VPL", from = "2006-12-08", to = Sys.Date())[,6])

benchmarklist <- list(sp500, stoxx, asia)


for (c in (1:(length(benchmarklist)-1))) {
  
  if(c == 1){
    databenchmark <- benchmarklist[1]
    databenchmark <- merge.data.frame(databenchmark, benchmarklist[c+1], by = "row.names")
    rownames(databenchmark) <- databenchmark[,1]
    databenchmark <- databenchmark[,-1]
  }
  else{
    databenchmark <- merge.data.frame(databenchmark, benchmarklist[c+1], by = "row.names")
    rownames(databenchmark) <- databenchmark[,1]
    databenchmark <- databenchmark[,-1]
  }
}

for (c in 1:ncol(databenchmark)) {
  databenchmark[,c] <- indexpf(as.data.frame(databenchmark[,c]))
}

benchmark <- as.data.frame(matrix(data = NA, ncol = 1, nrow=nrow(databenchmark)))
weights <- c(0.4,0.3,0.3)

for (r in 1:nrow(databenchmark)) {
  benchmark[r,1] <- as.numeric(as.matrix(databenchmark[r,]) %*% as.matrix(weights))
}

rownames(benchmark) <- rownames(sp500)

indices = c("Africa", "Australia", "Latinamerica", "Commodities", "LongBond", "ShortBond", "Benchmark")
dflist <- list(Africa, Australia, Latinamerica, Commodities, LongBond, ShortBond, benchmark)


for (c in (1:(length(dflist)-1))) {
  
  if(c == 1){
    datayahoo <- dflist[1]
    datayahoo <- merge.data.frame(datayahoo, dflist[c+1], by = "row.names")
    rownames(datayahoo) <- datayahoo[,1]
    datayahoo <- datayahoo[,-1]
  }
  else{
  datayahoo <- merge.data.frame(datayahoo, dflist[c+1], by = "row.names")
  rownames(datayahoo) <- datayahoo[,1]
  datayahoo <- datayahoo[,-1]
  }
}

colnames(datayahoo) <- indices
datayahoo$Date <- row.names(datayahoo)
datayahoo$Date <- format(as.Date(datayahoo$Date), "%d/%m/%Y")



#Read in data downloaded from Datastream as not everything was available on investing.com
#Drop three industries as they cause too many missing values in our data
as_static <- read.csv("staticdata/as.csv", sep = ';')
as_static[ ,c('AS_media', 'AS_retail', 'AS_travel')] <- list(NULL)

us_static <- read.csv("staticdata/us.csv", sep = ";")
us_static[ ,c('US_media', 'US_retail', 'US_travel')] <- list(NULL)

us_static[-c(1)] <- lapply( us_static[-c(1)], function(x) as.numeric(x))
US_final <- bind_rows(US, us_static)

as_static[-c(1)] <- lapply( as_static[-c(1)], function(x) as.numeric(x))
AS_final <- bind_rows(AS, as_static)

# Creating final dataframe
ovr <- dplyr::inner_join(EU,US_final, by = "Date") %>%
  inner_join(.,AS_final, by = "Date") %>% inner_join(.,datayahoo, by = "Date")











