

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
library("ggplot2")


#------------------------------Main----------------------------------#
library("StockCalc")
Quandl.api_key("aFFTC-nfXbcUNY5xbuVt")
#user mxlchr, PW krass123
Starttime <- Sys.time()
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

buy <- data.frame(matrix(vector(), 0, 4,
                         dimnames=list(c(), c("BDate","Buy","SDate","Sell"))),
                  stringsAsFactors=T)


allShare <- xts(x="", order.by=Sys.Date())

for (b in 1:array_size){

ETH  <- Read_Share_DB("WIKI/",sh_names[b],"2017-06-30","2018-01-31")
allShare<- cbind(allShare,ETH[,3])
cETH <- Calc_Share_DB(ETH)
buy<-Eval_Share(cETH,1.035,buy )


}
#delte first dummy value
# allShare <- allShare[,-1]
# colnames(allShare)<- sh_names

Stoptime <- Sys.time()
print(Stoptime-Starttime)



##BBand variation
BBTH_1.04<- coredata(as.xts(buy[4]/buy[2]))
BBTH_1.025<- test[,1]
#names(test1) <- "1.025"
#test1 <- coredata(as.xts(test1))




# create value labels



plot(density(BBTH_1.02), col = "red",xlim = c(0.9,1.2),
     main = "BBTH from 1.02 - 1.04, ds: 0.05, r/blu/g/o/blk")
abline (v = mean(BBTH_1.02), col = "red")
lines(density(BBTH_1.025), col = "blue")
abline (v = mean(BBTH_1.025), col = "blue")
lines(density(BBTH_1.03), col = "green")
abline (v = mean(BBTH_1.03), col = "green")
lines(density(BBTH_1.035), col = "orange")
abline (v = mean(BBTH_1.035), col = "orange")
lines(density(BBTH_1.04), col = "black")
abline (v = mean(BBTH_1.04), col = "black")



lines (c(mean(BBTH_1.02),
        mean(BBTH_1.025),
        mean(BBTH_1.03),
        mean(BBTH_1.035)
        ,mean(BBTH_1.04)), xaxt = "n", xlab = "BBTH", ylab = "% win")
axis (1, at=1:5,labels= c(1.02,1.025,1.03,1.035,1.04))


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


