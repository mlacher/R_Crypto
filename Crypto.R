# This program reads the crypto data from Bitfinex

install.packages("Quandl")
install.packages("dygraphs")
library(xts)  
library("Quandl")
library("dygraphs")
# Quandl.api_key('***************')
BTC <- Quandl("BITFINEX/BTCUSD",type="xts")
ETH <- Quandl("BITFINEX/ETHUSD")
IOTA <- Quandl("BITFINEX/IOTUSD")
test <- as.matrix(ETH[2])

BTC.ma11 <- xts(x= filter(BTC[,3], rep(1/11, 11)),order.by = BTC)
BTC<-cbind.xts(BTC.ma11,BTC) # name missing, order wrong

# Plot
dygraph(BTC[,1:2]) %>% 
  dyRangeSelector()


