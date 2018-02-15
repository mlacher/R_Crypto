
<<<<<<< HEAD:StockCalc/R/CalcShare.R
Eval_Share <- function (cShare_DB,cbuy){

  sell <- data.frame(matrix(vector(), 0, 1,
                            dimnames=list(c(), c("Sell"))),
                     stringsAsFactors=F)

=======
#Boilinger Band evaluation
Eval_Share <- function (cShare_DB){
>>>>>>> parent of 84834cf... 1st major update:StockCalc/R/AlgShare_V1.R

#Normalise BBand to mean value
cCalc_Share_DB<- cbind.xts(cShare_DB[,3]/cShare_DB[,3],
                 cShare_DB[,2]/cShare_DB[,3],
                 cShare_DB[,4]/cShare_DB[,3],
                 0,0,0,0)
array_size <- length(cCalc_Share_DB [,3])
#1 or 0 if below threshold
for(i in 1:array_size){
  if(cCalc_Share_DB [i,3]< 1.04){
    cCalc_Share_DB[i,4]<- 1;
  }
  else{
    cCalc_Share_DB[i,4] <- 0;
  }
}
return(cCalc_Share_DB)
}
