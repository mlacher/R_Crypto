

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

test <- cbind(1,2)

#------------------------------Main----------------------------------#
library("StockCalc")
Quandl.api_key("aFFTC-nfXbcUNY5xbuVt")
#user mxlchr, PW krass123
Starttime <- Sys.time()
sh_names <- c("AES","ADI","CCE","DOW","EW",
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
              "AAPL"
)

sell <- c(0)
array_size <- length(sh_names)

for (b in 1:array_size){
 
 
ETH  <- Read_Share_DB("WIKI/",sh_names[b],"2017-06-30","2018-01-31")
cETH <- Calc_Share(ETH)



#BB band calculations
nETC<- cbind.xts(cETH[,3]/cETH[,3],cETH[,2]/cETH[,3],cETH[,4]/cETH[,3],0,0,0,0)
array_size <- length(nETC [,3])
#1 or 0 if below threshold
for(i in 1:array_size){
  if(nETC [i,3]< 1.035){                
    nETC[i,4]<- 1;
  }
  else{
    nETC[i,4] <- 0;
  }                    
}
#1 or 0 if below x days
goto = 0;
for(i in 1:(array_size-1)){
  if(nETC [(i+1),4]>0){                
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
    buy<- rbind.xts(buy,cETH[(i+1),10]) ;
    goto = 1;
    #sell[b] <- cETH[(array_size),10];
  }
  else if ((nETC[(i+1),7]==-1) && (nETC[(i+1),6]==1)&& (goto ==1)){ # first test sell
    goto = 1;
    sell <- rbind.xts(cETH[(i+1),10]);
  }
  #else {sell[b] <-cETH[(array_size),10];}
}


}
Stoptime <- Sys.time()
print(Stoptime-Starttime)
# Plot
buy
plot(sell/buy)


plot.xts(cETH[,c(2,4,6,7,10)])
abline(v=.index(buy[35]), col='blue')

abline(v=as.Date(index(buy[35])),col = 'red')
par(mfrow=c(3,1))


plot(as.matrix (cETH[,10]) , type = "l")
abline(h=11.140,col = 'red')
plot(nETC[,c(6)])
plot(nETC[,c(7)])
plot(cETH[,c(1)])
#BBand based on the mean_norm value

plot(nETC)

dygraph(cETH[,c(10)]) %>% 
 # dyRangeSelector()
dyAnnotation(index(buy[35]), text = "B", tooltip = "Vietnam")
