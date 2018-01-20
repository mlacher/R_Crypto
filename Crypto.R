

#-----------------------Required Packages----------------------------#
install.packages("xts")
install.packages("Quandl")
install.packages("dygraphs")
install.packages("TTR")

library("xts")  
library("Quandl")
library("TTR")
library("dygraphs")

#------------------------------Functions-----------------------------#

#1.1 Read Database

Read_Crypt_DB<- function(Crypt_Name){
Crypt_Add = paste("BITFINEX/",Crypt_Name,"USD", sep ="")
BTC <- Quandl(Crypt_Add,type="xts")
return (BTC)
}


#1.2 Calulate Stuff
Calc_Crypt <- function (Crypt_DB){
macd  <- MACD(Crypt_DB[,2], 12, 26, 9, maType="EMA" )
EMA12 <- EMA(BTC[,3], 12)
EMA26 <- EMA(BTC[,3], 20)
EMA <- cbind(EMA12,EMA26)
colnames(EMA.BTC)<- c("EMA12","EMA26")
Crypt_DB<-cbind.xts(EMA,Crypt_DB) 
}
#------------------------------Main----------------------------------#

BTC <- Read_Crypt_DB("BTC")
Calc_Crypt(BTC)
# Plot

dygraph(BTC[,c(1,2,5)]) %>% 
  dyRangeSelector()

BTC <- BTC["2017-6/2018-6"]
macd <- macd["2017-6/2018-6"] 
windows()
par(mfrow=c(2,1))
plot(macd)
plot(BTC[,c(1,2,5)])