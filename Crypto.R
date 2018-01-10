# This program reads the crypto data from Bitfinex

install.packages("Quandl")
install.packages("dygraphs")

library(xts)  
library("Quandl")
library("dygraphs")
# Quandl.api_key('***************')
BTC <- Quandl("BITFINEX/BTCUSD",type="xts")
ETH <- Quandl("BINANCE/ETHUSD")
IOTA <- Quandl("BITFINEX/IOTUSD")
XRP <- Quandl("BITFINEX/XRPUSD")
#XVG <- Quandl("BITFINEX/XVGUSD")

test <- as.matrix(ETH[2])
BTC.ma7 <- filter(BTC[,3], 1/13,sides = 1, method = "recursive") # smoothing 1 week
BTC.ma11 <- filter(BTC[,3], 2/27,sides = 1, method = "recursive" ) # smoothing half month
BTC<-cbind.xts(BTC.ma7,BTC) # name missing 
BTC<-cbind.xts(BTC.ma11,BTC) # name missing
# Plot
dygraph(BTC[,1:3]) %>% 
  dyRangeSelector()


