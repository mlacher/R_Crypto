

#-----------------------Required Packages----------------------------#
install.packages("xts")
install.packages("Quandl")
install.packages("dygraphs")
install.packages("digest")
install.packages("TTR")

library("xts")  
library("Quandl")
library("TTR")
library("dygraphs")

#------------------------------Functions-----------------------------#

#1.1 Read Database
Read_Crypt_DB<- function(Crypt_Name){
Crypt_Add = paste("BITFINEX/",Crypt_Name,"USD", sep ="")
Crypt_DB <- Quandl(Crypt_Add,type="xts")
return (Crypt_DB)
}

#1.2 Calulate Stuff
Calc_Crypt <- function (Crypt_DB){
macd  <- MACD(Crypt_DB[,2], 12, 26, 9, maType="EMA" )
EMA12 <- EMA(Crypt_DB[,3], 12)
EMA26 <- EMA(Crypt_DB[,3], 20)
EMA <- cbind(EMA12,EMA26)
colnames(EMA)<- c("EMA12","EMA26")
cCrypt_DB<-cbind.xts(EMA,Crypt_DB) 
BB20<- BBands(Crypt_DB[,3], sd=2.0)
cCrypt_DB<-cbind.xts(BB20,cCrypt_DB) 
return (cCrypt_DB)
}

#1.3 Plot Range and Save
Plot_Crypt <- function (cCrypt_DB, begDate, endDate){
Date_Add = paste(begDate,"/",endDate, sep = "")
plot_cCrypt_DB <- cCrypt_DB[Date_Add]
windows()
savePlot(pdf,'myplot.pdf')
par(mfrow=c(2,1))
plot(plot_cCrypt_DB[,3])
plot(plot_cCrypt_DB[,c(1,2)])
dev.off()
}

macd <- macd["2017-6/2018-6"] 


#------------------------------Main----------------------------------#
library("StockCalc")
BTC  <- Read_Crypt_DB("BTC")
cBTC <- Calc_Crypt(BTC)
Plot_Crypt(cBTC,"2017-6","2018-1")
# Plot

dygraph(cBTC[,c(1,3,9)]) %>% 
  dyRangeSelector()

