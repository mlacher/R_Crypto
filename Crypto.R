

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
hallo <- c("AAPL","AES","ADI","CCE")
buy <- c(0,0,0)
sell <- c(0,0,0)
for (b in 1:4){
 
  
ETH  <- Read_Share_DB("WIKI/",hallo[b])
cETH <- Calc_Share(ETH)
pcETH <- Plot_Share(cETH,"2017-6","2018-2")

#BB band calculations
nETC<- cbind.xts(pcETH[,3]/pcETH[,3],pcETH[,2]/pcETH[,3],pcETH[,4]/pcETH[,3],0,0,0,0)
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
  if (pcETH[(i+1),10]>pcETH[(i+1),4]){ #Value hits UpperBB
    nETC[(i+1),7]<- 1;
  }
  else {
    nETC[(i+1),7]<- 0;
  }
  
  if ((nETC[(i+1),7]==1) && (nETC[(i+1),6]==1)&& (goto ==0)){ # first test buy in
    buy[b] <- pcETH[(i+1),10];
    i = (array_size-1);# does not work?
    goto = 1;
    sell[b] <- pcETH[(array_size),10];
  }

}


}

# Plot
par(mfrow=c(3,1))
plot(pcETH[,c(2,4,6,7,10)])
plot(nETC[,c(1:3)])
plot(nETC[,c(6)])
plot(nETC[,c(7)])
plot(pcETH[,c(1)])
#BBand based on the mean_norm value

plot(nETC)

dygraph(cETH[,c(1,3,9)]) %>% 
  dyRangeSelector()

