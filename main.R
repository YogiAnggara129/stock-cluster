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

# plot stocks return
library(MASS)
library(reshape2)
library(reshape)
library(ggplot2)
jii_ret <- read.csv("data/jii_jul-aug_2022_ret.csv")
molten_jii_ret <- melt(jii_ret, id="Date")
molten_jii_ret$Date <- as.Date(molten_jii_ret$Date)
ggplot(molten_jii_ret[molten_jii_ret$Date > as.Date("2022-01-01"),]) +
  geom_line(aes(x=Date, y=value, colour=variable)) +
  scale_colour_discrete(name="Kode Saham") +
  ylab("Return") +
  xlab("Tanggal")

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

clust <- kmeans(jii_ret_risk, centers = 2)
clust
fviz_cluster(clust, jii_ret_risk)

# mean and covariance
jii_ret <- read.csv("data/jii_jul-aug_2022_ret.csv")
stocks_choosed <- list(
  clust_1 = c("ITMG.JK", "ADRO.JK", "PTBA.JK", "MDKA.JK"),
  clust_2 = c("INDF.JK", "TLKM.JK", "KLBF.JK", "UNTR.JK")
) 

## MLE
mean_stocks_choosed <- list(
  clust_1 = colMeans(jii_ret[,stocks_choosed$clust_1]),
  clust_2 = colMeans(jii_ret[,stocks_choosed$clust_2])
)
cov_stocks_choosed <- list(
  clust_1 = cov(jii_ret[,stocks_choosed$clust_1]),
  clust_2 = cov(jii_ret[,stocks_choosed$clust_2])
)
mean_stocks_choosed
cov_stocks_choosed

## S-estimation
library(rrcov)
cov_s_stocks_choosed <- list(
  clust_1 = CovSest(jii_ret[,stocks_choosed$clust_1]),
  clust_2 = CovSest(jii_ret[,stocks_choosed$clust_2])
)
cov_s_stocks_choosed