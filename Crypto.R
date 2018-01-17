# This program reads the crypto data from Bitfinex

install.packages("Quandl")
install.packages("dygraphs")
install.packages("TTR")


library("xts")  
library("Quandl")
library("TTR")
library("dygraphs")
# Quandl.api_key('***************')


BTC <- Quandl("BITFINEX/BTCUSD",type="xts")
ETH <- Quandl("BINANCE/ETHUSD")
IOTA <- Quandl("BITFINEX/IOTUSD")
XRP <- Quandl("BITFINEX/XRPUSD")
#XVG <- Quandl("BITFINEX/XVGUSD")
macd  <- MACD(BTC[,2], 12, 26, 9, maType="EMA" )

EMA.BTC12 <- EMA(BTC[,3], 12)
EMA.BTC26 <- EMA(BTC[,3], 20)
EMA.BTC <- cbind(EMA.BTC12,EMA.BTC26)
colnames(EMA.BTC)<- c("EMA12","EMA26")
BTC<-cbind.xts(EMA.BTC,BTC) # name missing 


# Plot

dygraph(BTC[,c(1,2,5)]) %>% 
  dyRangeSelector()

BTC <- BTC["2017-6/2018-6"]
macd <- macd["2017-6/2018-6"] 
windows()
par(mfrow=c(2,1))
plot(macd)
plot(BTC[,c(1,2,5)])