

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
ETH  <- Read_Share_DB("WIKI/","BA")
cETH <- Calc_Share(ETH)

pcETH <- Plot_Share(cETH,"2017-6","2018-2")
nETC<- cbind.xts(pcETH[,3]/pcETH[,3],pcETH[,2]/pcETH[,3],pcETH[,4]/pcETH[,3],0)
array_size <- length(nETC [,3])

for(i in 1:array_size){
  if(nETC [i,3]< 1.035){                
    nETC[i,4]<- 1;
  }
  else{
    nETC[i,4] <- 0;
  }                    
}

# Plot
par(mfrow=c(3,1))
plot(pcETH[,c(2,4,6,7,10)])
plot(pcETH[,c(1)])
plot(nETC)

#BBand based on the mean_norm value

plot(nETC)

dygraph(cETH[,c(1,3,9)]) %>% 
  dyRangeSelector()

