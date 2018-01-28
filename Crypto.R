

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

#-----------------------1.1 Read Database----------------------------#
Read_Crypt_DB<- function(Crypt_Name){
Crypt_Add = paste("BITFINEX/",Crypt_Name,"USD", sep ="")
Crypt_DB <- Quandl(Crypt_Add,type="xts")
return (Crypt_DB)
}

#----------------------1.2 Calulate Stuff----------------------------#
Calc_Crypt <- function (Crypt_DB){

#-----------------------1.2.1 MACD / EMA-----------------------------#
# MACD 
# Long Buy Signal : EMA12>EMA26 cross from below
# Sell Signal     : EMA12<EMA26 cross from top
macd  <- MACD(Crypt_DB[,2], 12, 26, 9, maType="EMA" )
EMA12 <- EMA(Crypt_DB[,3], 12)
EMA26 <- EMA(Crypt_DB[,3], 20)
EMA <- cbind(EMA12,EMA26)
colnames(EMA)<- c("EMA12","EMA26")
cCrypt_DB<-cbind.xts(EMA,Crypt_DB) 

#---------------------1.2.2 Boilinger Band---------------------------#
# Trend up    : Chart crosses Upper band
# Trend low   : Chart crosses Lower band
# Trend Change short: Chart close to one band -> leads to other direction 
# Trend Change(up or down) : bandwidth small
BB20<- BBands(Crypt_DB[,3], sd=2.0)
cCrypt_DB<-cbind.xts(BB20,cCrypt_DB) 

#--------------------------1.2.3 RSI---------------------------------#
# Undervalued   : RSI< 30
# Overvalued    : RSI> 70
Rsi<-RSI(Crypt_DB[,3])
cCrypt_DB<-cbind.xts(Rsi,cCrypt_DB)

#--------------1.2.X Return XTS Object-------------------------------#
# Return Object with following Structure:
# EMA|BBdown|BBAVG|BBup|pctB|EMA12|EMA26|HIGH|LOW|MID|LAST|BID|ASK|Vol
return (cCrypt_DB)
}

#--------------------1.3 BuySignal-----------------------------------#
# Algorithm to calculate WinSuccess 
# Include BB(Bandwith small)+ RSI(70-30)+MACD(positiv)+BB(more uptrend)

#----------------1.4 Plot Range and Save-----------------------------#
Plot_Crypt <- function (cCrypt_DB, begDate, endDate){
Date_Add = paste(begDate,"/",endDate, sep = "")
plot_cCrypt_DB <- cCrypt_DB[Date_Add]


# par(mfrow=c(2,1))
# a<-plot(plot_cCrypt_DB[,3])
# b<-plot(plot_cCrypt_DB[,c(1,2)])
return(plot_cCrypt_DB)
}

macd <- macd["2017-6/2018-6"] 


#------------------------------Main----------------------------------#
library("StockCalc")
ETH  <- Read_Crypt_DB("ETH")
cETH <- Calc_Crypt(ETH)

pcETH <- Plot_Crypt(cETH,"2017-6","2018-1")
# Plot
par(mfrow=c(2,1))
plot(pcETH[,c(2,4,6,7,10)])
plot(pcETH[,c(1)])
abline(h=70)



dygraph(cETH[,c(1,3,9)]) %>% 
  dyRangeSelector()

