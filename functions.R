importdata<-function(){
  print("loading crsp/comp")
  comp <- read.csv("~/Desktop/Princeton/Spring2014/FIN561/Paper/OriginalData/comp.csv",stringsAsFactors=FALSE)
  date<-as.Date(as.character(comp$datadate),"%m/%d/%Y")
  comp$datadate<-date
  CUSIP<-comp$cusip
  comp<-cbind(comp,CUSIP)
  comp$cusip <- substr(comp$cusip,1,6)
  comp<-cbind(seq(from=1,to=dim(comp)[1],by=1),comp)
  colnames(comp)[1]<-"hash"
  #load("~/Desktop/Princeton/Spring2014/FIN591/Paper/data/crsp.RData")
  crsp <- read.csv("~/Desktop/Princeton/Spring2014/FIN591/Paper/data/crspdaily2.csv",stringsAsFactors=FALSE)
  date<-as.Date(as.character(crsp$date),"%Y%m%d")
  crsp$date<-date
  crsp$CUSIP <- substr(crsp$CUSIP,1,6)
  crsp<-cbind(seq(from=1,to=dim(crsp)[1],by=1),crsp)
  colnames(crsp)[1]<-"hash"
  
  print("loading gzmonthly")
  GZ_monthly <- read.csv("~/Desktop/Princeton/Spring2014/FIN591/Paper/data/Data_AER_2010_0787/GZ_monthly.csv")
  
  print("loading fisd")
  fisd <- read.csv("~/Desktop/Princeton/Spring2014/FIN591/Paper/data/fisd.csv", stringsAsFactors=FALSE)
  fisd$MATURITY<-as.Date(as.character(fisd$MATURITY),"%Y%m%d")
  fisd$OFFERING_DATE<-as.Date(as.character(fisd$OFFERING_DATE),"%Y%m%d")
  fisd$FIRST_INTEREST_DATE<-as.Date(as.character(fisd$FIRST_INTEREST_DATE),"%Y%m%d")
  fisd$LAST_INTEREST_DATE<-as.Date(as.character(fisd$LAST_INTEREST_DATE),"%Y%m%d")
  
  print("loading treas")
  treas <- read.csv("~/Desktop/Princeton/Spring2014/FIN591/Paper/data/treas.csv", stringsAsFactors=FALSE)
  treas$Date<-as.Date(treas$Date,"%m/%d/%Y")
  colnames(treas)<-c("Date","y1mo","y3mo","y6mo","y1yr","y2yr","y3yr","y5yr","y7yr","y10yr","y20yr","y30yr")
  
  print("loading dailyconstantmat")
  dailyconstantmat <- read.csv("~/Desktop/Princeton/Spring2014/FIN591/Paper/data/dailyconstantmat.csv", stringsAsFactors=FALSE)
  dailyconstantmat$observation_date<-as.Date(dailyconstantmat$observation_date,"%Y-%m-%d")
  
  print("loading real gdp")
  realgdp <- read.csv("~/Desktop/Princeton/Spring2014/FIN591/Paper/data/realgdp.csv",stringsAsFactors=FALSE)
  industry <- read.csv("~/Desktop/Princeton/Spring2014/FIN591/Paper/data/industry.csv",stringsAsFactors=FALSE)
  ratings <- read.csv("~/Desktop/Princeton/Spring2014/FIN591/Paper/data/ratings.csv",stringsAsFactors=FALSE)
  ind<-rep(NA,dim(fisd)[1])
  grade<-rep(NA,dim(fisd)[1])
  for(i in 1:dim(fisd)[1]){
    print(i)
    ind[i]<-industry$INDUSTRY_GROUP[which(industry$OFFERING_DATE==fisd$OFFERING_DATE[i] & industry$ISSUER_CUSIP==fisd$ISSUER_CUSIP[i])[1]]
    grade[i]<-ratings$RATING[which(ratings$OFFERING_DATE==fisd$OFFERING_DATE[i] & ratings$ISSUER_CUSIP==fisd$ISSUER_CUSIP[i])[1]]
  }
  
  #print("loading crsp/comp")
  crsp<-cbind(rep(NA,dim(crsp)[1]),crsp)
  colnames(crsp)[1]<-"constr"
  for(i in unique(crsp$date)){
    print(i)
    crsp$constr[which(crsp$date==i)]<-as.numeric(as.character(dailyconstantmat$DGS1[which(dailyconstantmat$observation_date==i)]))
  }
}
eliminate<-function(){
  #eliminate senior unstructured, non-fixed-coupon
  fisd<-fisd[-which(fisd$COUPON_TYPE!="F"), ]
  fisd<-fisd[-which(is.na(fisd$INTEREST_FREQUENCY)), ]
  fisd<-fisd[-which(is.na(fisd$MATURITY)), ]
  fisd<-fisd[-which(is.na(fisd$FIRST_INTEREST_DATE)), ]
  fisd<-fisd[-which(fisd$MATURITY==fisd$FIRST_INTEREST_DATE), ]
  #fisd<-fisd[-which(difftime(fisd$MATURITY,as.Date("2014-04-21"),units="days")>0), ]
  #eliminate spreads below 5bp, 3500bp
  fisd<-fisd[-which(is.na(fisd$TREASURY_SPREAD)), ]
  fisd<-fisd[-which(fisd$TREASURY_SPREAD<5 | fisd$TREASURY_SPREAD>3500), ]
  #eliminate par value < 1m
  fisd<-fisd[-which(fisd$OFFERING_AMT<1000000), ]
  #eliminate term to maturity of < 1 year, > 30 years
  fisd<-fisd[-which(difftime(fisd$MATURITY,fisd$OFFERING_DATE,units="weeks")>1560), ]
  fisd<-fisd[-which(difftime(fisd$MATURITY,fisd$OFFERING_DATE,units="weeks")<53), ]
}
ytm<-function(price,coupon){
  r<-0
  guess<-0
  while(abs(guess-price)>1 & r<.1){
    rates<-rep(1/(1+r),length(coupon))
    rates<-cumprod(rates)
    guess<-sum(rates*coupon)
    r<-r+.0001
    #print(paste(r," ",length(guess)," ",guess," ",price))
  }
  return(r)
}
getspreads<-function(){
  spread<-rep(NA,dim(fisd)[1])
  for(k in 1:dim(fisd)[1]){
    if(difftime(fisd$MATURITY[k],as.Date("2014-04-21"),units="days")>0){
      spread[k]<-fisd$TREASURY_SPREAD[k]/10000
      print(paste(k," ",spread[k]," whack"))
    } else{
      maturitylength<-difftime(fisd$MATURITY[k],fisd$FIRST_INTEREST_DATE[k],units="weeks")/4
      frequency<-fisd$INTEREST_FREQUENCY[k]
      coupon<-fisd$COUPON[k]*fisd$PRINCIPAL_AMT[k]/frequency/100
      firstdate<-fisd$FIRST_INTEREST_DATE[k]
      dates<-c(); rates<-c(); t<-c(); dates<-append(dates,firstdate)
      while(difftime(fisd$MATURITY[k],firstdate,units="weeks")>0){
        month(firstdate)<-month(firstdate)+12/frequency
        dates<-append(dates,firstdate)
      }
      coupon<-rep(coupon,length(dates)); coupon[length(dates)]<-coupon[length(dates)]+fisd$PRINCIPAL_AMT[k]
      t<-rep(frequency/12,length(dates)); t<-cumsum(t)
      if(frequency==2){
        for(i in 1:length(dates)){
          #print(i)
          rates<-append(rates,treas$y6mo[which(year(treas$Date)==year(dates[i]) & month(treas$Date)==month(dates[i]))[1]])
        }
      }
      if(frequency==4){
        for(i in 1:length(dates)){
          #print(i)
          rates<-append(rates,treas$y3mo[which(year(treas$Date)==year(dates[i]) & month(treas$Date)==month(dates[i]))[1]])
        }
      }
      if(frequency==1){
        for(i in 1:length(dates)){
          #print(i)
            rates<-append(rates,treas$y1yr[which(year(treas$Date)==year(dates[i]) & month(treas$Date)==month(dates[i]))[1]])
        }
      }
      rates<-as.numeric(rates)/100; 
      #disc<-cumprod(1/(rates+1)); 
      disc<-exp(-(rates*t))
      price<-disc*coupon; price<-sum(price)
      spread[k]<-fisd$OFFERING_YIELD[k]/100-ytm(price,coupon)
      print(paste(k," ",spread[k]))
    }
  }
  #fisd<-cbind(fisd,spread)
  #fisd<-fisd[-which(is.na(fisd$spread)), ]
  return(spread)
}
plotgz<-function(){
  y<-c()
  m<-c()
  gz<-c()
  for(year in 2007:2010){
    for(month in 1:12){
      y<-append(y,year)
      m<-append(m,month)
      #block<-fisd[which(year(fisd$MATURITY)>=year & month(fisd$MATURITY)>=month 
      #                  & year(fisd$OFFERING_DATE)<=year & month(fisd$OFFERING_DATE)<=month), ]
      block<-fisd[which(year(fisd$OFFERING_DATE)==year & month(fisd$OFFERING_DATE)==month), ]
      if(dim(block)[1]>=1){
        gz<-append(gz,mean(block$spread[which(is.na(block$spread)==FALSE)]*100))
        #gz<-append(gz,mean(block$TREASURY_SPREAD)/100)
      } else{
        gz<-append(gz,NA)
      }
    }
  }
  gz<-gz[-c(216,215,214)]
  y<-y[-c(216,215,214)]
  m<-m[-c(216,215,214)]
  lines((y+m/12),gz,type="l",ylim=c(-1,8),col="blue")
  plot((y+m/12),GZ_monthly$gz_spr[265:477],ylim=c(-1,8),col="red",type="l",xlab="Year",ylab="Percentage Points")
}
matchcrspcomp<-function(){
  lct<-rep(NA,dim(fisd)[1])
  dlt<-rep(NA,dim(fisd)[1])
  hash<-c()
  returns<-list()
  for(i in 2019:dim(fisd)[1]){
    block_crsp<-crsp[which(crsp$CUSIP==fisd$ISSUER_CUSIP[i] & difftime(fisd$OFFERING_DATE[i],crsp$date,units="weeks")<=52 & difftime(fisd$OFFERING_DATE[i],crsp$date,units="weeks")>=0), ]
    block_comp<-comp[which(comp$cusip==fisd$ISSUER_CUSIP[i] & abs(difftime(comp$datadate,fisd$OFFERING_DATE[i],units="weeks"))<120), ]
    if(dim(block_crsp)[1]>1 & dim(block_comp)[1]>1){
      print(paste(i))
      hash<-append(hash,i)
      lct[i]<-block_comp$lct[which(is.na(block_comp$lct)==FALSE)[1]]
      dlt[i]<-block_comp$dltt[which(is.na(block_comp$dltt)==FALSE)[1]]
      returns[[i]]<-block_crsp
    }
  }
}
getdd<-function(){
  hash<-hash[which(hash %in% which(is.na(lct)==FALSE & is.na(dlt)==FALSE))]
  blacklist<-c()
  for(i in 1:length(hash)){
    prc<-returns[[hash[i]]]$PRC; shrout<-returns[[hash[i]]]$SHROUT; constr<-returns[[hash[i]]]$constr
    filter<-which(is.na(prc)==FALSE & is.na(shrout)==FALSE & is.na(constr)==FALSE)
    prc<-prc[filter]; shrout<-shrout[filter]; constr<-constr[filter]
    if(length(prc)<=2){
      blacklist<-append(blacklist,i)
    }
  }
  hash<-hash[-blacklist]
  dd<-rep(NA,length(hash))
  for(i in 6:length(hash)){
    D<-lct[hash[i]]+.5*dlt[hash[i]]
    prc<-returns2[[i]]$PRC; shrout<-returns2[[i]]$SHROUT; constr<-returns2[[i]]$constr
    filter<-which(is.na(prc)==FALSE & is.na(shrout)==FALSE & is.na(constr)==FALSE)
    prc<-prc[filter]; shrout<-shrout[filter]; constr<-constr[filter]
    E<-prc*shrout/1000
    ret<-diff(E); ret<-ret/E[1:length(ret)]
    sigmae<-sd(ret)
    sigmav<-sigmae*(D/(D+E))
    #for(m in 1:2){
      V<-rep(0,length(E))
      print(paste(i," ",length(E)," ",mean(E)," ",D," ",returns2[[i]]$TICKER[1]," ",fisd[hash[i], ]$grade))
      for(j in 1:length(E)){
        cat(j)
        V[j]<-impv(1,0,sigmav[j],D,constr[j],E[j],1,i)
      }
      ret<-diff(V); ret<-ret/V[1:length(ret)]
      sigmav<-rep(sd(ret),length(E))
      muv<-mean(ret)
      print(V[1:10])
      print(sigmav[1:10])
      #print(i)
    #}
    dd[i]<-(log(mean(V)/D)+(muv-.5*mean(sigmav)^2))/(mean(sigmav))
    print(paste("dd: ",dd[i]))
  }
}
impv<-function(T,t,sigma,K,r,C,alpha,i){
  Ch<-0;S<-K
  d1<-(log(S/K)+(r+sigma^2/2)*(T-t))/(sigma*sqrt(T-t))
  d2<-d1-sigma*sqrt(T-t)
  Ch<-pnorm(d1)*S-pnorm(d2)*K*exp(-r*(T-t))
  if(Ch>C & abs(Ch-C)>alpha){
    while(abs(Ch-C)>alpha){
      S<-S-alpha
      d1<-(log(S/K)+(r+sigma^2/2)*(T-t))/(sigma*sqrt(T-t))
      d2<-d1-sigma*sqrt(T-t)
      Ch<-pnorm(d1)*S-pnorm(d2)*K*exp(-r*(T-t))
      #print(paste(S," ",Ch," ",C," ",i))
    }
  } else{
    while(abs(Ch-C)>alpha){
      S<-S+alpha
      d1<-(log(S/K)+(r+sigma^2/2)*(T-t))/(sigma*sqrt(T-t))
      d2<-d1-sigma*sqrt(T-t)
      Ch<-pnorm(d1)*S-pnorm(d2)*K*exp(-r*(T-t))
      #print(paste(S," ",Ch," ",C," ",i))
    }
  }
  return(S)
}
plotdd<-function(){
  y<-c()
  m<-c()
  gz<-c()
  dd<-c()
  cp<-c()
  ba<-c()
  ts<-c()
  ebp1<-c()
  ebp2<-c()
  ebp3<-c()
  for(year in 2000:2014){
    for(month in 1:12){
      y<-append(y,year)
      m<-append(m,month)
      block<-fisd[which(year(fisd$OFFERING_DATE)==year & month(fisd$OFFERING_DATE)==month), ]
      block2<-cprate[which(year(cprate$observation_date)==year & month(cprate$observation_date)==month), ]
      block3<-baa_aaa[which(year(baa_aaa$observation_date)==year & month(baa_aaa$observation_date)==month), ]
      block4<-dailyconstantmat[which(year(dailyconstantmat$observation_date)==year & month(dailyconstantmat$observation_date)==month), ]
      block5<-EBP[which(EBP$y==year & EBP$m==month), ]
      if(dim(block)[1]>=1){
        if(year>2006){
          gz<-append(gz,mean(block$spread*100))
        } else{
          gz<-append(gz,mean(block$TREASURY_SPREAD)/100)
        }
      } else{
        gz<-append(gz,NA)
      }
      if(dim(block)[1]>=1){
        dd<-append(dd,mean(block$ddrat))
      } else{
        dd<-append(dd,NA)
      }
      cp<-append(cp,mean(block2$CP3M,na.rm=TRUE))
      ba<-append(ba,mean(block3$baa_aaa,na.rm=TRUE))
      ts<-append(ts,mean(block4$DGS3MO,na.rm=TRUE))
      ebp1<-append(ebp1,mean(block5$ebp1,na.rm=TRUE))
      ebp2<-append(ebp2,mean(block5$ebp2,na.rm=TRUE))
      ebp3<-append(ebp3,mean(block5$ebp4,na.rm=TRUE))
    }
  }
  cp<-cp-ts
  #gz<-gz[-c(130,131,132)]
  #y<-y[-c(130,131,132)]
  #m<-m[-c(130,131,132)]
  #dd<-dd[-c(130,131,132)]
  dd<-data.frame(dd)
  gz<-data.frame(gz)
  ebp1<-data.frame(ebp3)
  miss <- !is.na(gz$gz)
  plot(y[which(miss)]+m[which(miss)]/12,gz[gz&miss],type="l",col="blue",xlab="Year",ylab="Percentage Points")  
  lines((y+m/12),GZ_monthly$gz_spr[349:477],col="red",type="l",xlab="Year",ylab="Percentage Points")
  plot((y+m/12),gztrend2,ylim=c(0,6),col="red",type="l",xlab="Year",ylab="Percentage Points")
  lines(y[which(miss)]+m[which(miss)]/12,gztrend,type="l",col="blue")  
  lines((y+m/12),cp,col="red",type="l",xlab="Year",ylab="Percentage Points")
  lines((y+m/12),ba,col="green",type="l",xlab="Year",ylab="Percentage Points")
  
  lines((y+m/12),GZ_monthly$dd_wp50[349:477],ylim=c(-10,10),col="black",type="l")
  #lines((hi$y[1:176]+hi$m[1:176]/12),hi$dd[1:176]/10+3,type="l",ylim=c(-1,10),col="black") 
  #lines((y+m/12),na.omit(dd)/10+3,type="l",ylim=c(-1,10),col="black",lwd=2)  
  
  miss2 <- !is.na(ebp1$ebp1)
  lines(y[which((ebp1&miss2)==TRUE)]+m[which((ebp1&miss2)==TRUE)]/12,ebp1[ebp1&miss2],lty=2,type="l",xlab="Year",ylab="Percentage Points")
  miss4 <- !is.na(dd$dd)
  lines(y[which(dd&miss4==TRUE)]+m[which(dd&miss4==TRUE)]/12, dd[dd&miss4]/20, col="black", type="l")
  gzhat<-data.frame(gz$gz-ebp1$ebp1)
  colnames(gzhat)[1]<-"gzhat"
  miss3 <- !is.na(gzhat$gzhat)
  plot(y[which((gzhat&miss3)==TRUE)]+m[which((gzhat&miss3)==TRUE)]/12,gzhat[gzhat&miss3],col="red",type="l",xlab="Year",ylab="Percentage Points")
  
}
getddmv<-function(){
  dd<-rep(NA,length(hash))
  for(i in 1:length(hash)){
    print(i)
    D<-lct[hash[i]]+.5*dlt[hash[i]]
    prc<-returns[[hash[i]]]$PRC; shrout<-returns[[hash[i]]]$SHROUT
    filter<-which(is.na(prc)==FALSE & is.na(shrout)==FALSE)
    prc<-prc[filter]; shrout<-shrout[filter]
    E<-prc*shrout/1000
    V<-E+D
    ret<-diff(V); ret<-ret/V[1:length(ret)]
    sigmav<-sd(ret)
    muv<-mean(ret)
    dd[i]<-(log(mean(V)/D)+(muv-.5*sigmav^2))/sigmav
  }
}
getddmle<-function(){
  dd<-rep(NA,length(hash))
  for(i in 1:length(hash)){
    print(i)
    D<-lct[hash[i]]+.5*dlt[hash[i]]
    prc<-returns[[hash[i]]]$PRC; shrout<-returns[[hash[i]]]$SHROUT; constr<-returns[[hash[i]]]$constr
    filter<-which(is.na(prc)==FALSE & is.na(shrout)==FALSE & is.na(constr)==FALSE)
    prc<-prc[filter]; shrout<-shrout[filter]; constr<-constr[filter]
    E<-prc*shrout/1000
    n<-length(E)
    h<-1/12
    V<-rep(0,length(E))
    L<-c()
    sigopt<-c()
    muopt<-c()
    sigma<-.05
    for(j in 1:length(E)){
      V[j]<-impv(1,0,sigma,D,constr[j],E[j],1,i)
    }
    for(sigma in seq(0.001,.5,by=.01)){
      for(mu in seq(-.5,.5,by=.01)){
        W<-diff(log(V))-(mu-.5*sigma^2)*h
        L<-append(L,(-sum(W)/(2*sigma^2*h) - sum(log(V)) 
          - sum(log(pnorm((log(V/D)+(constr+sigma^2/2))/(sigma))))))
        print((-sum(W)/(2*sigma^2*h) - sum(log(V)) 
               - sum(log(pnorm((log(V/D)+(constr+sigma^2/2))/(sigma))))))
        sigopt<-append(sigopt,sigma)
        muopt<-append(muopt,mu)
      }
    }
    ret<-diff(V); ret<-ret/V[1:length(ret)]
    sigmav<-sigopt
    muv<-muopt
    dd[i]<-(log(mean(V)/D)+(muv-.5*sigmav^2))/sigmav
  }
}
regress<-function(){
  y<-c()
  m<-c()
  gz<-c()
  gzp1<-c()
  gzp2<-c()
  gzp3<-c()
  gzp4<-c()
  gzp5<-c()
  hash<-0
  dd1<-list()
  dd2<-list()
  dd3<-list()
  for(year in 1993:2014){
    for(month in 1:12){
      hash<-hash+1
      y<-append(y,year)
      m<-append(m,month)
      block<-fisd[which(year(fisd$OFFERING_DATE)==year & month(fisd$OFFERING_DATE)==month), ]
      block<-block[which(block$ddrat!=Inf), ]
      if(dim(block)[1]>=1){
        print(dim(block))
        gz<-append(gz,mean(block$spread[which(is.na(block$spread)==FALSE)]))
        gzp1<-append(gzp1,mean(predict(lm(block$spread ~ block$dd + block$PRINCIPAL_AMT + block$COUPON))))
        gzp2<-append(gzp2,mean(predict(lm(block$spread ~ block$ddmv + block$PRINCIPAL_AMT + block$COUPON))))
        #gzp3<-append(gzp3,mean(predict(lm(block$spread ~ block$ddmle + block$PRINCIPAL_AMT + block$COUPON))))
        gzp4<-append(gzp4,mean(predict(lm(block$spread ~ block$ddrat + block$PRINCIPAL_AMT + block$COUPON))))
        #gzp5<-append(gzp5,mean(predict(lm(block$spread ~ block$ddrat2 + block$PRINCIPAL_AMT + block$COUPON))))
        #tryCatch({print(paste(year," ",month,summary(lm(block$spread ~ block$dd + block$PRINCIPAL_AMT + block$COUPON
         #                    + block$age + block$grade + block$ind))[[4]]))}, error=function(e){})
        fuck<-tryCatch({summary(lm(block$spread ~ block$dd + block$PRINCIPAL_AMT + block$COUPON
                                   + block$age + block$grade + block$ind))[[4]]}, error=function(e){})
        if(length(dim(fuck)[1]>0)){
          dd1[[hash]]<-fuck
          #print(dd1[[hash]])
          print(month)
        } else{
          dd1[[hash]]<-NA
        }
        dd2[[hash]]<-tryCatch({summary(lm(block$spread ~ block$ddmv + block$PRINCIPAL_AMT + block$COUPON
                                   + block$age + block$grade + block$ind))[[4]]}, error=function(e){})
        dd3[[hash]]<-tryCatch({summary(lm(block$spread ~ block$ddrat + block$PRINCIPAL_AMT + block$COUPON
                                   + block$age + block$grade + block$ind))[[4]]}, error=function(e){})
      }
       else{
         dd1[[hash]]<-NA
         dd2[[hash]]<-NA
         dd3[[hash]]<-NA
        gz<-append(gz,NA)
        gzp1<-append(gzp1,NA)
        gzp2<-append(gzp2,NA)
        gzp3<-append(gzp3,NA)
        gzp4<-append(gzp4,NA)
        gzp5<-append(gzp5,NA)
      }
    }
  }
  GZ<-data.frame(y)
  GZ<-cbind(GZ,m,gz,gzp1,gzp2,gzp3,gzp4,gzp5)
}
predictgdp1quarter<-function(){
  c<-400
  p<-1
  h<-1
  ts<-rep(NA,length(realgdp$observation_date))
  rf<-rep(NA,length(realgdp$observation_date))
  cs<-rep(NA,length(realgdp$observation_date))
  ba<-rep(NA,length(realgdp$observation_date))
  cp<-rep(NA,length(realgdp$observation_date))
  cs2<-rep(NA,length(realgdp$observation_date))
  for(j in 1:length(unique(realgdp$observation_date))){
    i<-unique(realgdp$observation_date)[j]
    print(i)
    if(length(which(abs(difftime(dailyconstantmat$observation_date,i,units="weeks"))<1))>0){
      ts[which(realgdp$observation_date==i)]<-dailyconstantmat$DGS30[which(abs(difftime(dailyconstantmat$observation_date,i,units="weeks"))<1)[1]]-dailyconstantmat$DGS3MO[which(abs(difftime(dailyconstantmat$observation_date,i,units="weeks"))<1)[1]]
    }
    if(length(which(abs(difftime(rff$observation_date,i,units="weeks"))<1))>0){
      rf[which(realgdp$observation_date==i)]<-rff$FEDFUNDS[which(abs(difftime(rff$observation_date,i,units="weeks"))<1)[1]]
    }
    if(length(which(year(fisd$OFFERING_DATE)==year(i) & month(fisd$OFFERING_DATE)==month(i)))>0){
      cs[which(realgdp$observation_date==i)]<-fisd$spread[which(year(fisd$OFFERING_DATE)==year(i) & month(fisd$OFFERING_DATE)==month(i))[1]]
    }
    if(length(which(year(fisd$OFFERING_DATE)==year(i) & month(fisd$OFFERING_DATE)==month(i)))>0){
      cs2[which(realgdp$observation_date==i)]<-fisd$TREASURY_SPREAD[which(year(fisd$OFFERING_DATE)==year(i) & month(fisd$OFFERING_DATE)==month(i))[1]]
    }
    if(length(which(year(baa_aaa$observation_date)==year(i) & month(baa_aaa$observation_date)==month(i)))>0){
      ba[which(realgdp$observation_date==i)]<-baa_aaa$baa_aaa[which(year(baa_aaa$observation_date)==year(i) & month(baa_aaa$observation_date)==month(i))[1]]
    }
    if(length(which(year(cprate$observation_date)==year(i) & month(cprate$observation_date)==month(i)))>0){
      cp[which(realgdp$observation_date==i)]<-cprate$CP3M[which(year(cprate$observation_date)==year(i) & month(cprate$observation_date)==month(i))[1]]
    }
  }
  realgdp<-cbind(realgdp,cs,ba,cp,cs2)
  y<-realgdp3$GDPC1[1:(length(realgdp3$GDPC1)-1)]
  grad_y<-(c/(h+1))*log(realgdp3$GDPC1[2:length(realgdp3$GDPC1)]/y)
  ts<-realgdp3$ts[1:(length(realgdp3$GDPC1)-1)]*100/100
  rf<-realgdp3$rf[1:(length(realgdp3$GDPC1)-1)]*100/100
  cs<-realgdp3$cs[1:(length(realgdp3$GDPC1)-1)]*100*100/100
  ba<-realgdp3$ba[1:(length(realgdp3$GDPC1)-1)]*100/100
  cp<-realgdp3$cp[1:(length(realgdp3$GDPC1)-1)]*100/100
  cs2<-realgdp3$cs2[1:(length(realgdp3$GDPC1)-1)]/100
  table1<-c()
  table1<-cbind(table1,c(summary(lm(grad_y ~ y + ts + rf))[[4]][3,1],summary(lm(grad_y ~ y + ts + rf))[[4]][3,3],
                         summary(lm(grad_y ~ y + ts + rf))[[4]][3,4],summary(lm(grad_y ~ y + ts + rf))[[4]][4,1],
                         summary(lm(grad_y ~ y + ts + rf))[[4]][4,3],summary(lm(grad_y ~ y + ts + rf))[[4]][4,4],rep("",9)))
  table1<-cbind(table1,c(summary(lm(grad_y ~ y + ts + rf + cp))[[4]][3,1],summary(lm(grad_y ~ y + ts + rf + cp))[[4]][3,3],
                         summary(lm(grad_y ~ y + ts + rf + cp))[[4]][3,4],summary(lm(grad_y ~ y + ts + rf + cp))[[4]][4,1],
                         summary(lm(grad_y ~ y + ts + rf + cp))[[4]][4,3],summary(lm(grad_y ~ y + ts + rf + cp))[[4]][4,4],
                         summary(lm(grad_y ~ y + ts + rf + cp))[[4]][5,1],summary(lm(grad_y ~ y + ts + rf + cp))[[4]][5,3],
                         summary(lm(grad_y ~ y + ts + rf + cp))[[4]][5,4],rep("",6)))
  table1<-cbind(table1,c(summary(lm(grad_y ~ y + ts + rf + ba))[[4]][3,1],summary(lm(grad_y ~ y + ts + rf + ba))[[4]][3,3],
                         summary(lm(grad_y ~ y + ts + rf + ba))[[4]][3,4],summary(lm(grad_y ~ y + ts + rf + ba))[[4]][4,1],
                         summary(lm(grad_y ~ y + ts + rf + ba))[[4]][4,3],summary(lm(grad_y ~ y + ts + rf + ba))[[4]][4,4],rep("",3),
                         summary(lm(grad_y ~ y + ts + rf + ba))[[4]][5,1],summary(lm(grad_y ~ y + ts + rf + ba))[[4]][5,3],
                         summary(lm(grad_y ~ y + ts + rf + ba))[[4]][5,4],rep("",3)))
  table1<-cbind(table1,c(summary(lm(grad_y ~ y + ts + rf + cs))[[4]][3,1],summary(lm(grad_y ~ y + ts + rf + cs))[[4]][3,3],
                         summary(lm(grad_y ~ y + ts + rf + cs))[[4]][3,4],summary(lm(grad_y ~ y + ts + rf + cs))[[4]][4,1],
                         summary(lm(grad_y ~ y + ts + rf + cs))[[4]][4,3],summary(lm(grad_y ~ y + ts + rf + cs))[[4]][4,4],rep("",6),
                         summary(lm(grad_y ~ y + ts + rf + cs))[[4]][5,1],summary(lm(grad_y ~ y + ts + rf + cs))[[4]][5,3],
                         summary(lm(grad_y ~ y + ts + rf + cs))[[4]][5,4]))
  table1<-cbind(table1,c(summary(lm(grad_y ~ y + ts + rf + cs2))[[4]][3,1],summary(lm(grad_y ~ y + ts + rf + cs2))[[4]][3,3],
                         summary(lm(grad_y ~ y + ts + rf + cs2))[[4]][3,4],summary(lm(grad_y ~ y + ts + rf + cs2))[[4]][4,1],
                         summary(lm(grad_y ~ y + ts + rf + cs2))[[4]][4,3],summary(lm(grad_y ~ y + ts + rf + cs2))[[4]][4,4],rep("",6),
                         summary(lm(grad_y ~ y + ts + rf + cs2))[[4]][5,1],summary(lm(grad_y ~ y + ts + rf + cs2))[[4]][5,3],
                         summary(lm(grad_y ~ y + ts + rf + cs2))[[4]][5,4]))
}
predictgdp1quarter2<-function(){
  c<-400
  p<-1
  h<-1
  ts<-rep(NA,length(realgdp$observation_date))
  rf<-rep(NA,length(realgdp$observation_date))
  cshat<-rep(NA,length(realgdp$observation_date))
  ebp<-rep(NA,length(realgdp$observation_date))
  cshat2<-rep(NA,length(realgdp$observation_date))
  ebp2<-rep(NA,length(realgdp$observation_date))
  cshat3<-rep(NA,length(realgdp$observation_date))
  ebp3<-rep(NA,length(realgdp$observation_date))
  cshat4<-rep(NA,length(realgdp$observation_date))
  ebp4<-rep(NA,length(realgdp$observation_date))
  for(j in 1:length(unique(realgdp$observation_date))){
    i<-unique(realgdp$observation_date)[j]
    print(i)
    if(length(which(year(dailyconstantmat$observation_date)==year(i) & month(dailyconstantmat$observation_date)==month(i) & is.na(dailyconstantmat$DGS30)==FALSE))>0){
      ts[which(realgdp$observation_date==i)]<-dailyconstantmat$DGS30[which(year(dailyconstantmat$observation_date)==year(i) & month(dailyconstantmat$observation_date)==month(i)& is.na(dailyconstantmat$DGS30)==FALSE)[1]]-dailyconstantmat$DGS3MO[which(year(dailyconstantmat$observation_date)==year(i) & month(dailyconstantmat$observation_date)==month(i)& is.na(dailyconstantmat$DGS3MO)==FALSE)[1]]
    }
    if(length(year(rff$observation_date)==year(i) & month(rff$observation_date)==month(i))>0){
      rf[which(realgdp$observation_date==i)]<-rff$FEDFUNDS[which(year(rff$observation_date)==year(i) & month(rff$observation_date)==month(i))[1]]
    }
    if(length(which(EBP$y==year(i) & EBP$m==month(i)))>0){
      ebp[which(realgdp$observation_date==i)]<-EBP$ebp1[which(EBP$y==year(i) & EBP$m==month(i))]
    }
    if(length(which(GZ$y==year(i) & GZ$m==month(i)))>0){
      cshat[which(realgdp$observation_date==i)]<-GZ$gzp1[which(GZ$y==year(i) & GZ$m==month(i))]
    }
    if(length(which(EBP$y==year(i) & EBP$m==month(i)))>0){
      ebp2[which(realgdp$observation_date==i)]<-EBP$ebp2[which(EBP$y==year(i) & EBP$m==month(i))]
    }
    if(length(which(GZ$y==year(i) & GZ$m==month(i)))>0){
      cshat2[which(realgdp$observation_date==i)]<-GZ$gzp2[which(GZ$y==year(i) & GZ$m==month(i))]
    }
    if(length(which(EBP$y==year(i) & EBP$m==month(i)))>0){
      ebp3[which(realgdp$observation_date==i)]<-EBP$ebp3[which(EBP$y==year(i) & EBP$m==month(i))]
    }
    if(length(which(GZ$y==year(i) & GZ$m==month(i)))>0){
      cshat3[which(realgdp$observation_date==i)]<-GZ$gzp3[which(GZ$y==year(i) & GZ$m==month(i))]
    }
    if(length(which(EBP$y==year(i) & EBP$m==month(i)))>0){
      ebp4[which(realgdp$observation_date==i)]<-EBP$ebp4[which(EBP$y==year(i) & EBP$m==month(i))]
    }
    if(length(which(GZ$y==year(i) & GZ$m==month(i)))>0){
      cshat4[which(realgdp$observation_date==i)]<-GZ$gzp4[which(GZ$y==year(i) & GZ$m==month(i))]
    }
  }
  realgdp<-cbind(realgdp,ts,rf,cshat,ebp,cshat2,ebp2,cshat3,ebp3,cshat4,ebp4)
  y<-realgdp3$GDPC1[1:(length(realgdp3$GDPC1)-1)]
  grad_y<-(c/(h+1))*log(realgdp3$GDPC1[2:length(realgdp3$GDPC1)]/y)
  ts<-realgdp3$ts[1:(length(realgdp3$GDPC1)-1)]*100/100
  rf<-realgdp3$rf[1:(length(realgdp3$GDPC1)-1)]*100/100
  cshat<-realgdp3$cshat[1:(length(realgdp3$GDPC1)-1)]*100*100/100
  ebp<-realgdp3$ebp[1:(length(realgdp3$GDPC1)-1)]*100
  cshat2<-realgdp3$cshat2[1:(length(realgdp3$GDPC1)-1)]*100*100/100
  ebp2<-realgdp3$ebp2[1:(length(realgdp3$GDPC1)-1)]*100
  cshat3<-realgdp3$cshat3[1:(length(realgdp3$GDPC1)-1)]*100*100/100
  ebp3<-realgdp3$ebp3[1:(length(realgdp3$GDPC1)-1)]*100
  cshat4<-realgdp3$cshat4[1:(length(realgdp3$GDPC1)-1)]*100*100/100
  ebp4<-realgdp3$ebp4[1:(length(realgdp3$GDPC1)-1)]*100
  table1<-c()
  table1<-cbind(table1,c(summary(lm(grad_y ~ y + ts + rf + cshat + ebp))[[4]][3,1],summary(lm(grad_y ~ y + ts + rf + cshat + ebp))[[4]][3,3],
                         summary(lm(grad_y ~ y + ts + rf + cshat + ebp))[[4]][3,4],summary(lm(grad_y ~ y + ts + rf + cshat + ebp))[[4]][4,1],
                         summary(lm(grad_y ~ y + ts + rf + cshat + ebp))[[4]][4,3],summary(lm(grad_y ~ y + ts + rf + cshat + ebp))[[4]][4,4],
                         summary(lm(grad_y ~ y + ts + rf + cshat + ebp))[[4]][5,1],summary(lm(grad_y ~ y + ts + rf + cshat + ebp))[[4]][5,3],
                         summary(lm(grad_y ~ y + ts + rf + cshat + ebp))[[4]][5,4],summary(lm(grad_y ~ y + ts + rf + cshat + ebp))[[4]][6,1],
                         summary(lm(grad_y ~ y + ts + rf + cshat + ebp))[[4]][6,3],summary(lm(grad_y ~ y + ts + rf + cshat + ebp))[[4]][6,4]))
  table1<-cbind(table1,c(summary(lm(grad_y ~ y + ts + rf + cshat2 + ebp2))[[4]][3,1],summary(lm(grad_y ~ y + ts + rf + cshat2 + ebp2))[[4]][3,3],
                         summary(lm(grad_y ~ y + ts + rf + cshat2 + ebp2))[[4]][3,4],summary(lm(grad_y ~ y + ts + rf + cshat2 + ebp2))[[4]][4,1],
                         summary(lm(grad_y ~ y + ts + rf + cshat2 + ebp2))[[4]][4,3],summary(lm(grad_y ~ y + ts + rf + cshat2 + ebp2))[[4]][4,4],
                         summary(lm(grad_y ~ y + ts + rf + cshat2 + ebp2))[[4]][5,1],summary(lm(grad_y ~ y + ts + rf + cshat2 + ebp2))[[4]][5,3],
                         summary(lm(grad_y ~ y + ts + rf + cshat2 + ebp2))[[4]][5,4],summary(lm(grad_y ~ y + ts + rf + cshat2 + ebp2))[[4]][6,1],
                         summary(lm(grad_y ~ y + ts + rf + cshat2 + ebp2))[[4]][6,3],summary(lm(grad_y ~ y + ts + rf + cshat2 + ebp2))[[4]][6,4]))
  table1<-cbind(table1,c(summary(lm(grad_y ~ y + ts + rf + cshat3 + ebp3))[[4]][3,1],summary(lm(grad_y ~ y + ts + rf + cshat3 + ebp3))[[4]][3,3],
                         summary(lm(grad_y ~ y + ts + rf + cshat3 + ebp3))[[4]][3,4],summary(lm(grad_y ~ y + ts + rf + cshat3 + ebp3))[[4]][4,1],
                         summary(lm(grad_y ~ y + ts + rf + cshat3 + ebp3))[[4]][4,3],summary(lm(grad_y ~ y + ts + rf + cshat3 + ebp3))[[4]][4,4],
                         summary(lm(grad_y ~ y + ts + rf + cshat3 + ebp3))[[4]][5,1],summary(lm(grad_y ~ y + ts + rf + cshat3 + ebp3))[[4]][5,3],
                         summary(lm(grad_y ~ y + ts + rf + cshat3 + ebp3))[[4]][5,4],summary(lm(grad_y ~ y + ts + rf + cshat3 + ebp3))[[4]][6,1],
                         summary(lm(grad_y ~ y + ts + rf + cshat3 + ebp3))[[4]][6,3],summary(lm(grad_y ~ y + ts + rf + cshat3 + ebp3))[[4]][6,4]))
  table1<-cbind(table1,c(summary(lm(grad_y ~ y + ts + rf + cshat4 + ebp4))[[4]][3,1],summary(lm(grad_y ~ y + ts + rf + cshat4 + ebp4))[[4]][3,3],
                         summary(lm(grad_y ~ y + ts + rf + cshat4 + ebp4))[[4]][3,4],summary(lm(grad_y ~ y + ts + rf + cshat4 + ebp4))[[4]][4,1],
                         summary(lm(grad_y ~ y + ts + rf + cshat4 + ebp4))[[4]][4,3],summary(lm(grad_y ~ y + ts + rf + cshat4 + ebp4))[[4]][4,4],
                         summary(lm(grad_y ~ y + ts + rf + cshat4 + ebp4))[[4]][5,1],summary(lm(grad_y ~ y + ts + rf + cshat4 + ebp4))[[4]][5,3],
                         summary(lm(grad_y ~ y + ts + rf + cshat4 + ebp4))[[4]][5,4],summary(lm(grad_y ~ y + ts + rf + cshat4 + ebp4))[[4]][6,1],
                         summary(lm(grad_y ~ y + ts + rf + cshat4 + ebp4))[[4]][6,3],summary(lm(grad_y ~ y + ts + rf + cshat4 + ebp4))[[4]][6,4]))
}
plot1<-function(){
  lines(fisd$OFFERING_DATE[-which(year(fisd$OFFERING_DATE)<2000)],100*fisd$spread[-which(year(fisd$OFFERING_DATE)<2000)],type="l")
  lines(fisd$OFFERING_DATE[-which(year(fisd$OFFERING_DATE)<2000)],(1/100)*fisd$TREASURY_SPREAD[-which(year(fisd$OFFERING_DATE)<2000)],type="l")
}

for(i in unique(year(fisd$OFFERING_DATE))){
  print(paste(i," ",length(which(year(fisd$OFFERING_DATE)==i))," ",
              mean(fisd$OFFERING_AMT[which(year(fisd$OFFERING_DATE)==i)],na.rm=TRUE)/1000000," ",
              (mean(year(fisd$MATURITY[which(year(fisd$OFFERING_DATE)==i)]),na.rm=TRUE)-mean(year(fisd$OFFERING_DATE[which(year(fisd$OFFERING_DATE)==i)]),na.rm=TRUE))," ",
              mean(which(fisd$CONVERTIBLE[which(year(fisd$OFFERING_DATE)==i)]!="N"),na.rm=TRUE)," ",
              mean(fisd$COUPON[which(year(fisd$OFFERING_DATE)==i)],na.rm=TRUE)," ",
              mean(fisd$OFFERING_YIELD[which(year(fisd$OFFERING_DATE)==i)],na.rm=TRUE)," ",
              mean(fisd$TREASURY_SPREAD[which(year(fisd$OFFERING_DATE)==i)],na.rm=TRUE)," ",
              mean(fisd$spread[which(year(fisd$OFFERING_DATE)==i)],na.rm=TRUE)
        ))
}

#returns2<-list()
#for(i in 1:length(hash)){
#  print(i)
#  permno<-returns[[hash[i]]]$PERMNO[1]
#  mindate<-min(returns[[hash[i]]]$date)
#  maxdate<-max(returns[[hash[i]]]$date)
#  returns2[[i]]<-crsp[which(crsp$PERMNO==permno & difftime(crsp$date,mindate,units="days")>0 & difftime(crsp$date,maxdate,units="days")<0), ]
#}
summary(lm(fisd$spread ~ fisd$dd + fisd$PRINCIPAL_AMT + fisd$COUPON
           + fisd$age + fisd$grade + fisd$ind))[[4]]
summary(lm(fisd$spread ~ fisd$ddmv + fisd$PRINCIPAL_AMT + fisd$COUPON
           + fisd$age + fisd$grade + fisd$ind))[[4]]
fisd<-fisd[-which(fisd$ddrat %in% c(NA,NaN,Inf)), ]
summary(lm(fisd$spread ~ fisd$ddrat + fisd$PRINCIPAL_AMT + fisd$COUPON
           + fisd$age + fisd$grade + fisd$ind))[[4]]