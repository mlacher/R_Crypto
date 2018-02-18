

#-----------------------Required Packages----------------------------#
install.packages("xts")
install.packages("Quandl")
install.packages("dygraphs")
install.packages("digest")
install.packages("TTR")
install.packages("qpcR")

library("xts")
library("Quandl")
library("TTR")
library("dygraphs")
library("ggplot2")
library("qpcR") 

#------------------------------Main----------------------------------#
library("StockCalc")
Quandl.api_key("aFFTC-nfXbcUNY5xbuVt")
#user mxlchr, PW krass123

sh_names <- c("AES","ADI","DOW","EW",
              "FISV",
              "HD",
              "FDX",
              "ICE",
              "FLS",
              "IR",
              "IP",
              "FMC",
              "FFIV",
              "FOXA",
              "FIS",
              "FITB",
              "MMM",
              "NI",
              "FTI",
              "F",
              "AAPL",
              "FB",
              "AMZN",
              "MUSA",
              "MVC",
              "MYE",
              "MYRG",
              "MYGN",
              "NBTB",
              "NC",
              "NANO",
              "NSTG",
              "NNVC",
              "NATH"


)

array_size <- length(sh_names)
#array_size <-3 # debug function
Starttime <- Sys.time()
buy <- data.frame(matrix(vector(), 0, 5,
                         dimnames=list(c(), c("name","BDate","Buy","SDate","Sell"))),
                  stringsAsFactors=T)


allShare <- xts(x="", order.by=Sys.Date())

for (b in 1:array_size){

ETH  <- Read_Share_DB("WIKI/",sh_names[b],"2016-06-30","2017-06-30")
allShare<- cbind(allShare,ETH[,3])
cETH <- Calc_Share_DB(ETH)
buy<-Eval_Share(cETH,sh_names[b],1.06,buy )


}
#delte first dummy value
# allShare <- allShare[,-1]
# colnames(allShare)<- sh_names

Stoptime <- Sys.time()
print(Stoptime-Starttime)



##BBand variation
BBTH_1.06<- coredata(as.xts(buy[5]/buy[3]))



BBTH_1.025<- test[,1]
#names(test1) <- "1.025"
#test1 <- coredata(as.xts(test1))




# create value labels

BBTH<-qpcR:::cbind.na(BBTH_1.02, BBTH_1.025,BBTH_1.03,BBTH_1.035,
                BBTH_1.04, BBTH_1.045,BBTH_1.05,BBTH_1.055,
                BBTH_1.06)
boxplot(BBTH, xaxt = "n", ylim= c(0.9,1.3), main = 
          "Succes rate differnt BBTH 06/17-02/18")

axis (1, at=1:9,labels= c(1.02,
                          1.025
                          ,1.03
                          ,1.035
                          ,1.04
                          ,1.045
                          ,1.05
                          ,1.055
                          ,1.06))


layout(mat=matrix(c(1,2,3,4),nrow=4,ncol=1,byrow=T))
plot.xts(cETH[,c(2,4,6,7,10)])
plot(nETH[,c(6)])
plot(nETH[,c(7)])
plot(nETH[,c(1:3)])

plot.xts(allShare[,c(1:3)])


dygraph(cETH[,c(2,4,6,7,10)]) %>%
  # dyRangeSelector()
  dyAnnotation(rownames(buy)[1], text = "B", tooltip = "Buy")%>%
  dyAnnotation(rownames(sell)[1], text = "S", tooltip = "Sell")

#BBand based on the mean_norm value


