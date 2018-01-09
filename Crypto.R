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



# Plot
dygraph(BTC[,1:3]) %>% 
  dyRangeSelector()
