

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
<<<<<<< HEAD
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

#array_size <- length(sh_names)
array_size <-3 # debug function

buy <- data.frame(matrix(vector(), 0, 2,
                         dimnames=list(c(), c("Buy","Name"))),
                  stringsAsFactors=F)


allShare <- xts(x="", order.by=Sys.Date())
=======
# sh_names <- c("AES","ADI","CCE","DOW","EW",
#               "FISV",
#               "HD",
#               "FDX",
#               "ICE",
#               "FLS",
#               "IR",
#               "IP",
#               "FMC",
#               "FFIV",
#               "FOXA",
#               "FIS",
#               "FITB",
#               "MMM",
#               "NI",
#               "FTI",
#               "F",
#               "AAPL",
#               "FB",
#               "AMZN"
# )
sh_names <- c("AMZN","MMM",
                             "NI",
                            "FTI",
                            "F")
array_size <- length(sh_names)

buy <- data.frame(matrix(vector(), 0, 1,
                       dimnames=list(c(), c("Buy"))),
                stringsAsFactors=F)
sell <- data.frame(matrix(vector(), 0, 1,
                          dimnames=list(c(), c("Sell"))),
                   stringsAsFactors=F)
for (b in 1:array_size){
>>>>>>> parent of 83bb3ce... minor cleaning

for (b in 1:array_size){

ETH  <- Read_Share_DB("WIKI/",sh_names[b],"2017-06-30","2018-01-31")
allShare<- cbind(allShare,ETH[,3])
cETH <- Calc_Share_DB(ETH)
buy<-Eval_Share(cETH,buy )



}
#delte first dummy value
allShare <- allShare[,-1]
colnames(allShare)<- sh_names

Stoptime <- Sys.time()
print(Stoptime-Starttime)




plot(sell/buy)


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


