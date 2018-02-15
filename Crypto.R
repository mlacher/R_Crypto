

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

array_size <- length(sh_names)
#array_size <-3 # debug function
buy <- data.frame(matrix(vector(), 0, 2,
                       dimnames=list(c(), c("Buy","Name"))),
                stringsAsFactors=F)
sell <- data.frame(matrix(vector(), 0, 1,
                          dimnames=list(c(), c("Sell"))),
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
sh_names <- c("AMZN")
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

<<<<<<< HEAD
ETH  <- Read_Share_DB("WIKI/",sh_names[b],"2017-06-30","2018-01-31")

allShare<- cbind(allShare,ETH[,3])
cETH <- Calc_Share_DB(ETH)
<<<<<<< HEAD
buy<-Eval_Share(cETH,buy )
=======
ETH  <- Read_Share_DB("WIKI/",sh_names[1],"2017-06-30","2018-01-31")
cETH <- Calc_Share(ETH)
nETC<-Eval_Share(cETH )


#1 or 0 if below x days
goto = 0;
for(i in 1:(array_size-1)){
  if(nETC [(i+1),4]>0){  #bigger 1.035
    nETC[(i+1),5]<- as.numeric(nETC [i,5])+as.numeric(nETC [(i+1),4]);
    if (nETC[(i+1),5]>10){ # 15Days value below TH
      nETC[(i+1),6]<- 1;
      #nETC[(i+1),5]<-0; # RESET
    } # First Buy Sign @ 15D
    else {nETC[(i+1),6]<- 0;}

  }
  else{
    nETC[(i+1),5] <- 0;
  }
  if (cETH[(i+1),10]>cETH[(i+1),4]){ #Value hits UpperBB (buy signal)
    nETC[(i+1),7]<- 1;
  }
  else if (cETH[(i+1),10]<cETH[(i+1),2]){ #Value hits LowerBB (sell signal)
    nETC[(i+1),7]<- -1;
  }
  else {
    nETC[(i+1),7]<- 0;
  }

  if ((nETC[(i+1),7]==1) && (nETC[(i+1),6]==1)&& (goto ==0)){ # first test buy in
    buy<- rbind.data.frame(cETH[(i+1),10],buy) ;
    goto = 1;
    sell <- rbind.data.frame(cETH[(array_size),10],sell);
  }
>>>>>>> parent of 2c14de5... code cleaning

=======
nETH<-Eval_Share(cETH )

array_size <- length(nETH[,3])
#1 or 0 if below x days
goto = 0;
for(i in 1:(array_size-1)){
  if(nETH [(i+1),4]>0){  #bigger 1.035
    nETH[(i+1),5]<- as.numeric(nETH [i,5])+as.numeric(nETH [(i+1),4]);
    if (nETH[(i+1),5]>15){ # 15Days value below TH
      nETH[(i+1),6]<- 1;
      #nETH[(i+1),5]<-0; # RESET
    } # First Buy Sign @ 15D
    else {nETH[(i+1),6]<- 0;}

  }
  else{
    nETH[(i+1),5] <- 0;
  }
  if (cETH[(i+1),10]>cETH[(i+1),4]){ #Value hits UpperBB (buy signal)
    nETH[(i+1),7]<- 1;
  }
  else if (cETH[(i+1),10]<cETH[(i+1),2]){ #Value hits LowerBB (sell signal)
    nETH[(i+1),7]<- -1;
  }
  else {
    nETH[(i+1),7]<- 0;
  }

  if ((nETH[(i+1),7]==1) && (nETH[(i+1),6]==1)&& (goto ==0)){ # first test buy in
    buy<- rbind.data.frame(cETH[(i+5),10],buy) ; # +5, often price drop after crossing UpperBB
    goto = 1;
    sell <- rbind.data.frame(cETH[(array_size),10],sell);
  }
}
>>>>>>> parent of 84834cf... 1st major update

if (goto == 0){
  sell <- rbind.data.frame(cETH[(array_size),10],sell)
  buy <- rbind.data.frame(cETH[(array_size),10],buy)
  }

}
Stoptime <- Sys.time()
print(Stoptime-Starttime)

#delte first dummy value
allShare <- allShare[,-1]
colnames(allShare)<- sh_names


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


