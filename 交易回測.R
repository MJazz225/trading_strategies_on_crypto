###########################################################################
#
#
#    交易回測
#
#
#
############################################################################

rm(list=ls())
setwd("C:\\Users\\User\\Documents\\TXF_5minK")  ##資料在自己電腦
if(!require(data.table))install.packages("data.table")
if(!require(dplyr))install.packages("dpylr")
library(xts)
library(zoo)
library(stargazer)
if(!require(crypto2))install.packages("crypto2")
if(!require(quantmod))install.packages("quantmod")


getSymbols("AAPL")
chartSeries(AAPL["2012-01::2012-06"], theme="white")

ma_20 <- runMean(AAPL[,4],n=20)
ma_60 <- runMean(AAPL[,4],n=60)
addTA(ma_20, on=1, col = "blue")
addTA(ma_60, on=1, col = "red")

position <- Lag(ifelse(ma_20>ma_60,1,0)) #部位, 1做多0做空
return <- ROC(Cl(AAPL))*position #計算損益
return1 <- return["2007-03-30/2013-12-31"]
cumreturn <- exp(cumsum(return1))
plot(cumreturn)

coin_list <- crypto_list()
MATIC <- crypto_history(Polygon)

setwd("D:\\Users\\marshaw.tan\\Downloads")
ETH <- read.csv("Binance_ETHUSDT_minute.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE) #ETHEREUM

colnames(ETH) <- ETH[2,]
ETH <- ETH[3:nrow(ETH),]
ETH <- ETH[c(nrow(ETH):1),]
ETH <- as.data.frame(ETH)
ETH_date <- ETH$date
ETH_date <- as.POSIXct.default(ETH_date) #把日期轉換成時間序列
ETH <- ETH[, c(4,5,6,7,9)]
ETH <- as.xts(ETH, ETH_date) #轉換時間序列
ETH <- as.data.frame(ETH)

ETH1 <- ETH[576625:nrow(ETH),] #從2021年開始
head(ETH1)
eth_5 <- runMean(ETH1[,4],n=5)
eth_10 <- runMean(ETH1[,4],n=10)
eth_20 <- runMean(ETH1[,4],n=20)
eth_60 <- runMean(ETH1[,4],n=60)
money = 100000
#buy <- money
#sell <- 
position <- Lag(ifelse(ETH1[,4] > eth_10 & eth_5>eth_10 & eth_5>eth_60,1,0)) #部位, 1做多0做空

return <- ROC(as.numeric(ETH1[,4]))*position #計算損益
return <- na.omit(return)
cumreturn <- exp(cumsum(return))
plot(cumreturn)


ETH2 <- cbind(ETH1, eth_5, eth_10, eth_20, eth_20)
for (i in nrow(ETH1)) {

  buyposition <- Lag(ifelse(ETH2[,4] > eth_10*1.001 & eth_5*1.001 > eth_10*1.001 & eth_5*1.001>eth_60*1.001,1,0)) #部位, 1做多
  sellposition <- Lag(ifelse(ETH2[,4] < eth_10*0.999 & eth_5*0.999 < eth_10*0.999 & eth_5*0.999 < eth_60*0.999,0,-1)) #部位, 1做空
  
  ShortPosition <- Lag(ifelse(ETH2[,4] > eth_10*1.001 & eth_5*1.001 > eth_10*1.001 & eth_5*1.001>eth_60*1.001,-1,0)) #漲的時候做空
  LongPosition <- Lag(ifelse(ETH2[,4] < eth_10*0.999 & eth_5*0.999 < eth_10*0.999 & eth_5*0.999 < eth_60*0.999,0,1)) #跌的時候做多
}

par(mfrow=c(2,2))


buyreturn <- ROC(as.numeric(ETH1[,4]))*buyposition #計算做多損益
buyreturn <- na.omit(buyreturn)
cumbuyreturn <- exp(cumsum(buyreturn))
plot(cumbuyreturn)

sellreturn <- ROC(as.numeric(ETH1[,4]))*sellposition #計算做空損益
sellreturn <- na.omit(sellreturn)
cumsellreturn <- exp(cumsum(sellreturn))
plot(cumsellreturn)

shortreturn <- ROC(as.numeric(ETH1[,4]))*ShortPosition #漲做空
shortreturn <- na.omit(shortreturn)
cumshortreturn <- exp(cumsum(shortreturn))
plot(cumshortreturn)

longreturn <- ROC(as.numeric(ETH1[,4]))*LongPosition #跌做多
longreturn <- na.omit(longreturn)
cumlongreturn <- exp(cumsum(longreturn))
plot(cumlongreturn)


buysellposition <- buyposition+sellposition
return <- ROC(as.numeric(ETH1[,4]))*buysellposition #計算做空損益
return <- na.omit(return)
cumreturn <- exp(cumsum(return))
plot(cumreturn)




