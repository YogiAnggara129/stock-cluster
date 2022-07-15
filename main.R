# set working directory
setwd("E:\\My Project\\Course\\stock-cluster")

# import data
## source: https://www.idx.co.id/data-pasar/data-saham/indeks-saham/
jii <- read.csv("data/jii_jul-aug_2022.csv")
jii$Kode

stocks_code <- jii$Kode
stocks_code <- sapply(stocks_code, function(x) {
  x = strsplit(x, "'[^']*'(*SKIP)(*F)|\\s+", perl=TRUE)
  paste0(x, ".JK")}
)

dateStart <- "01-01-2021"        ## Format: %d-%m-%Y
dateFinish <- "15-07-2022"       ## Format: %d-%m-%Y
freqValue <- "1mo"               ## 1mo: montly, 1wk: weekly, 1d: daily

# Get stocks close return
get_stock_price <- function(stocks, dateStart, dateFinish, freqValue) {
  isFirst = TRUE
  for (stock in stocks) {
    api = paste0("https://query1.finance.yahoo.com/v7/finance/download/",
                 stock,
                 "?period1=",
                 as.numeric(as.POSIXct(dateStart, format="%d-%m-%Y")),
                 "&period2=",
                 as.numeric(as.POSIXct(dateFinish, format="%d-%m-%Y")),
                 "&interval=",
                 freqValue)
    dfStock = read.csv(api)
    
    if(isFirst){
      dfStocks = data.frame(Date = as.Date(dfStock[, 1]))
      isFirst = FALSE
    }
    
    dfStocks[stock] = dfStock$Close
  }
  Date = dfStocks[-1, 1]
  nRows = length(Date) + 1
  retValue = (dfStocks[-1, -1] - dfStocks[-nRows, -1]) / dfStocks[-nRows, -1]
  dfStocksReturn = cbind(Date, retValue)
  rownames(dfStocksReturn) = NULL
  return(dfStocksReturn)
}

jii_ret <- get_stock_price(stocks_code, dateStart, dateFinish, freqValue)
View(jii_ret)
write.csv(jii_ret, "data/jii_jul-aug_2022_ret.csv", row.names = FALSE)

# get expected return and risk
jii_ret <- read.csv("data/jii_jul-aug_2022_ret.csv")
jii_ret <- jii_ret[,-1]  # drop date column
exp_ret <- colMeans(jii_ret)
risk <- diag(var(jii_ret))
jii_ret_risk <- data.frame(expected_return = exp_ret, risk = risk)
View(jii_ret_risk)
write.csv(jii_ret_risk, "data/jii_jul-aug_2022_ret_risk.csv", row.names = FALSE)

# K-Means model
library(ggplot2)
library(factoextra)
jii_ret_risk <- read.csv("data/jii_jul-aug_2022_ret_risk.csv")
fviz_nbclust(jii_ret_risk, FUNcluster = kmeans, method="wss")
fviz_nbclust(jii_ret_risk, FUNcluster = kmeans, method="silhouette")

clust <- kmeans(jii_ret_risk, centers = 3)
clust
fviz_cluster(clust, jii_ret_risk)
