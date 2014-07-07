#####import data#####
fisd <- read.csv("~/Desktop/Princeton/Spring2014/FIN591/Paper/data/fisd.csv", stringsAsFactors=FALSE)
fisd$MATURITY<-as.Date(as.character(fisd$MATURITY),"%Y%m%d")
fisd$OFFERING_DATE<-as.Date(as.character(fisd$OFFERING_DATE),"%Y%m%d")
fisd$FIRST_INTEREST_DATE<-as.Date(as.character(fisd$FIRST_INTEREST_DATE),"%Y%m%d")
fisd$LAST_INTEREST_DATE<-as.Date(as.character(fisd$LAST_INTEREST_DATE),"%Y%m%d")

treas <- read.csv("~/Desktop/Princeton/Spring2014/FIN591/Paper/data/treas.csv", stringsAsFactors=FALSE)
treas$Date<-as.Date(treas$Date,"%m/%d/%y")
#####eliminate senior unstructured, non-fixed-coupon#####
fisd<-fisd[-which(fisd$COUPON_TYPE!="F"), ]
fisd<-fisd[-which(is.na(fisd$INTEREST_FREQUENCY)), ]
#####find s#####
#####eliminate spreads below 5bp, 3500bp#####
fisd<-fisd[-which(is.na(fisd$TREASURY_SPREAD)), ]
fisd<-fisd[-which(fisd$TREASURY_SPREAD<5 | fisd$TREASURY_SPREAD>3500), ]
#####eliminate par value < 1m #####
fisd<-fisd[-which(fisd$OFFERING_AMT<1000000), ]
#####eliminate term to maturity of < 1 year, > 30 years #####
fisd<-fisd[-which(difftime(fisd$MATURITY,fisd$OFFERING_DATE,units="weeks")>1560), ]
fisd<-fisd[-which(difftime(fisd$MATURITY,fisd$OFFERING_DATE,units="weeks")<53), ]
#####make fake prices#####
maturitylength<-difftime(fisd$MATURITY[1],fisd$FIRST_INTEREST_DATE[1],units="weeks")/4
frequency<-fisd$INTEREST_FREQUENCY[1]
coupon<-fisd$COUPON[1]*fisd$PRINCIPAL_AMT*frequency
dates<-c()
firstdate<-fisd$FIRST_INTEREST_DATE[1]
dates<-append(dates,firstdate)
while(difftime(fisd$MATURITY[1],firstdate,"weeks")>0){
  month(firstdate)<-month(firstdate)+12/frequency
  dates<-append(dates,firstdate)
}
#####construct GZ#####
y<-c()
m<-c()
gz<-c()
for(year in 1989:2014){
  for(month in 1:12){
    y<-append(y,year)
    m<-append(m,month)
    block<-fisd[which(year(fisd$MATURITY)>=year & month(fisd$MATURITY)>=month 
                & year(fisd$OFFERING_DATE)<=year & month(fisd$OFFERING_DATE)<=month), ]
    if(dim(block)[1]>=1){
      gz<-append(gz,mean(block$TREASURY_SPREAD))
    } else{
      gz<-append(gz,NA)
    }
  }
}
plot((y+m/12),gz,type="l")
#####match with compustat#####
#####match with crsp#####
#####create monthly average spreads#####