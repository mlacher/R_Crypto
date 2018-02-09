
#Boilinger Band evaluation
Eval_Share <- function (cShare_DB){

#Normalise BBand to mean value
cCalc_Share_DB<- cbind.xts(cShare_DB[,3]/cShare_DB[,3],
                 cShare_DB[,2]/cShare_DB[,3],
                 cShare_DB[,4]/cShare_DB[,3],
                 0,0,0,0)
array_size <- length(cCalc_Share_DB[,3]) #doubled could be handed to funtion
#1 or 0 if below threshold
for(i in 1:array_size){
  if(cCalc_Share_DB [i,3]< 1.035){
    cCalc_Share_DB[i,4]<- 1;
  }
  else{
    cCalc_Share_DB[i,4] <- 0;
  }
}
return(cCalc_Share_DB)
}
