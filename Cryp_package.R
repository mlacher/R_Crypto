
install.packages("Quandl")
install.packages("dygraphs")
install.packages("TTR")

library("xts")  
library("Quandl")
library("TTR")
library("dygraphs")

Crypt_DB<- function(Crypt_Name){
  Crypt_Add = paste("BITFINEX/",Crypt_Name,"USD", sep ="")
  BTC <- Quandl(FullName,type="xts")
  return (BTC)
}

