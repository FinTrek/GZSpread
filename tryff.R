master<-c()
for(i in 0:168){
  print(i)
  hi<-read.csv.ffdf(file="~/Desktop/Princeton/Spring2014/FIN591/Paper/data/trace",header=TRUE,first.rows=1000,skip=(500000*i),colClasses=rep("factor",9),nrows=500000,VERBOSE=TRUE)
  x<-subset(as.data.frame(hi))
  x<-x[-which(duplicated(data.frame(x$CUSIP_ID,x$TRD_EXCTN_DT))==FALSE), ]
  master<-rbind(master,x)
}

for(cusip in unique(fisd$ISSUER_CUSIP)){
  if(length(which(substr(master$CUSIP_ID,1,6)==cusip))>0){
    print("yay")
  }
}