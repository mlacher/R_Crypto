
#Boilinger Band evaluation
Eval_Share <- function (cShare_DB,normbb,rbuy){

  cbuy <- data.frame(matrix(vector(), 0, 4,
                           dimnames=list(c(), c("BDate","Buy","SDate","Sell"))),
                    stringsAsFactors=T)


#Normalise BBand to mean value
cCalc_Share_DB<- cbind.xts(cShare_DB[,3]/cShare_DB[,3],
                 cShare_DB[,2]/cShare_DB[,3],
                 cShare_DB[,4]/cShare_DB[,3],
                 0,0,0,0)
array_size <- length(cCalc_Share_DB[,3]) #doubled could be handed to funtion
#1 or 0 if below threshold
for(i in 1:array_size){
  if(cCalc_Share_DB [i,3]< normbb){
    cCalc_Share_DB[i,4]<- 1;
  }
  else{
    cCalc_Share_DB[i,4] <- 0;
  }
}


array_size <- length(cCalc_Share_DB[,3])
#1 or 0 if below x days
goto = 0;
for(i in 1:(array_size-1)){
  if(cCalc_Share_DB [(i+1),4]>0){  #bigger 1.035
    cCalc_Share_DB[(i+1),5]<- as.numeric(cCalc_Share_DB [i,5])+as.numeric(cCalc_Share_DB [(i+1),4]);
    if (cCalc_Share_DB[(i+1),5]>15){ # 15Days value below TH
      cCalc_Share_DB[(i+1),6]<- 1;
      #cCalc_Share_DB[(i+1),5]<-0; # RESET
    } # First cbuy Sign @ 15D
    else {cCalc_Share_DB[(i+1),6]<- 0;}

  }
  else{
    cCalc_Share_DB[(i+1),5] <- 0;
  }
  if (cShare_DB[(i+1),10]>cShare_DB[(i+1),4]){ #Value hits UpperBB (cbuy signal)
    cCalc_Share_DB[(i+1),7]<- 1;
  }
  else if (cShare_DB[(i+1),10]<cShare_DB[(i+1),2]){ #Value hits LowerBB (sell signal)
    cCalc_Share_DB[(i+1),7]<- -1;
  }
  else {
    cCalc_Share_DB[(i+1),7]<- 0;
  }

  if ((cCalc_Share_DB[(i+1),7]==1) && 
      (cCalc_Share_DB[(i+1),6]==1) && 
      (goto ==0) &&
      (i< (array_size-7))){ # first test cbuy in
    cbuy<- cbind.data.frame(index(cShare_DB)[i+5],cShare_DB[(i+5),10],
                            index(cShare_DB[(array_size)]),cShare_DB[(array_size),10])
    rbuy<- rbind.data.frame(cbuy,rbuy) ; # +5, often price drop after crossing UpperBB
    goto = 1;
  }
}
return(rbuy)
}
