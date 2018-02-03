

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



#------------------------------Main----------------------------------#
library("StockCalc")
Quandl.api_key("aFFTC-nfXbcUNY5xbuVt")
#user mxlchr, PW krass123
Starttime <- Sys.time()
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
 
 
ETH  <- Read_Share_DB("WIKI/",sh_names[b],"2017-06-30","2018-01-31")
cETH <- Calc_Share(ETH)



#BB band calculations
nETC<- cbind.xts(cETH[,3]/cETH[,3],cETH[,2]/cETH[,3],cETH[,4]/cETH[,3],0,0,0,0)
array_size <- length(nETC [,3])
#1 or 0 if below threshold
for(i in 1:array_size){
  if(nETC [i,3]< 1.04){                
    nETC[i,4]<- 1;
  }
  else{
    nETC[i,4] <- 0;
  }                    
}

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

  
}


}
Stoptime <- Sys.time()
print(Stoptime-Starttime)

plot(sell/buy)


par(mfrow=c(4,1))
plot.xts(cETH[,c(2,4,6,7,10)])
plot(nETC[,c(6)])
plot(nETC[,c(7)])
plot(nETC[,c(1:3)])

dygraph(cETH[,c(2,4,6,7,10)]) %>% 
  # dyRangeSelector()
  dyAnnotation(rownames(buy)[1], text = "B", tooltip = "Buy")

#BBand based on the mean_norm value
plot(nETC)


