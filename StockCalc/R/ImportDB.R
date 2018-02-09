# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library("xts")
library("Quandl")
library("TTR")
library("dygraphs")

#------------------------------Functions-----------------------------#

#-----------------------1.1 Read Database----------------------------#
Read_Share_DB<- function(Share_Type,Share_Name,begDate, endDate){
  Share_Add = paste(Share_Type,Share_Name, sep ="")
  Share_DB <- Quandl(Share_Add,type="xts",
                     start_date=begDate, end_date=endDate)
  return (Share_DB)
}

#----------------------1.2 Calulate Stuff----------------------------#
Calc_Share_DB <- function (Share_DB){

  #-----------------------1.2.1 MACD / EMA-----------------------------#
  # MACD
  # Long Buy Signal : EMA12>EMA26 cross from below
  # Sell Signal     : EMA12<EMA26 cross from top
  macd  <- MACD(Share_DB[,2], 12, 26, 9, maType="EMA" )
  EMA12 <- EMA(Share_DB[,3], 12)
  EMA26 <- EMA(Share_DB[,3], 20)
  EMA <- cbind(EMA12,EMA26)
  colnames(EMA)<- c("EMA12","EMA26")
  cShare_DB<-cbind.xts(EMA,Share_DB)

  #---------------------1.2.2 Boilinger Band---------------------------#
  # Trend up    : Chart crosses Upper band
  # Trend low   : Chart crosses Lower band
  # Trend Change short: Chart close to one band -> leads to other direction
  # Trend Change(up or down) : bandwidth small
  BB20<- BBands(Share_DB[,3], sd=2.0)
  cShare_DB<-cbind.xts(BB20,cShare_DB)

  #--------------------------1.2.3 RSI---------------------------------#
  # Undervalued   : RSI< 30
  # Overvalued    : RSI> 70
  Rsi<-RSI(Share_DB[,3])
  cShare_DB<-cbind.xts(Rsi,cShare_DB)

  #--------------1.2.X Return XTS Object-------------------------------#
  # Return Object with following Structure:
  # RSI|BBdown|BBAVG|BBup|pctB|EMA12|EMA26|HIGH|LOW|MID|LAST|BID|ASK|Vol
  cShare_DB<- na.omit(cShare_DB) # remove mising values
  return (cShare_DB)
}



