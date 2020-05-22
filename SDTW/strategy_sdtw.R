## R codes: SDTW Strategy
## Functions for trading;
# Open long position;
long <- function(cashInto,longPositionInto,longPriceInto,price,longShotInto,initialMarginRate,marginAccountInto){
  if (cashInto < longShotInto*price*initialMarginRate*(1+0.00023)){
    notice = "Failure: capital shortage!"
    cashOut = cashInto
    longPositionOut = longPositionInto
    marginAccountOut = marginAccountInto
    longPriceOut = longPriceInto
  }
  else{
    cashOut = cashInto - longShotInto * price * initialMarginRate - longShotInto * price * 0.00023
    marginAccountOut = marginAccountInto + longShotInto * price * initialMarginRate
    longPositionOut = c(longPositionInto,longShotInto)
    longPriceOut = c(longPriceInto,price)
    notice = paste("Long:",longShotInto/300,"shares;")
  }
  lst <- list(notice,cashOut,longPositionOut,marginAccountOut,longPriceOut)
  return(lst)
}
# Close long position;
close_long <- function(cashInto,marginAccountInto,longPositionInto,longPriceInto,price){
  marginAccountOut = marginAccountInto
  cashOut = cashInto + marginAccountInto
  marginAccountOut = 0
  longPositionOut = c()
  longPriceOut = c()
  
  notice = "Close long postion."
  lst <- list(notice,cashOut,marginAccountOut,longPositionOut,longPriceOut)
  return(lst)
}
# Open short position;
short <- function(cashInto,shortPositionInto,shortPriceInto,price,shortShotInto,initialMarginRate,marginAccountInto){
  if (cashInto < shortShotInto*price*initialMarginRate*(1+0.00023)){
    notice = "Failure: capital shortage!"
    cashOut = cashInto
    shortPositionOut = shortPositionInto
    marginAccountOut = marginAccountInto
    shortPriceOut = shortPriceInto
  }
  else{
    cashOut = cashInto - shortShotInto * price * initialMarginRate - shortShotInto * price * 0.00023
    marginAccountOut = marginAccountInto + shortShotInto * price * initialMarginRate
    shortPositionOut = c(shortPositionInto,shortShotInto)
    shortPriceOut = c(shortPriceInto,price)
    notice = paste("Short:",shortShotInto/300,"shares;")
  }
  lst <- list(notice,cashOut,shortPositionOut,marginAccountOut,shortPriceOut)
  return(lst)
}
# Close short position;
close_short <- function(cashInto,marginAccountInto,shortPositionInto,shortPriceInto,price){
  cashOut = cashInto + marginAccountInto
  marginAccountOut = 0
  shortPositionOut = c()
  shortPriceOut = c()
  
  notice = "Close short postion."
  lst <- list(notice,cashOut,marginAccountOut,shortPositionOut,shortPriceOut)
  return(lst)
}

## Record and Maintain Daily Position;
record <- function(cash,initialMarginAccount,longPosition,longPrice,shortPosition,shortPrice,price,priceOld){
  PFValue = cash
  if(!(is.null(longPosition))){
    for(i in 1:len(longPosition)) {
      marginAccount = marginAccount + longPosition[i] * (price - priceOld)
    }
  }else if(!(is.null(shortPosition))){
    for(i in 1:len(shortPosition)) {
      marginAccount = marginAccount + shortPosition[i] * (priceOld - price)
    }
    
  }else{
    # Do nothing : )
  }
  
  PFValue = PFValue + marginAccount
  lst <- list(PFValue,marginAccount)
  return(lst)
}
maintain <- function(cashInto,marginAccountInto,longPosition,longPrice,shortPosition,shortPrice,price,initialMarginRate,maintainenceRatio){
  tempTT = 0
  if(!(is.null(longPosition))){
    for(i in 1:len(longPosition)) {
      tempTT = tempTT + longPosition[i] * longPrice[i] * initialMarginRate
    }
    if(marginAccountInto < (tempTT * maintainenceRatio)){
      marginAccountOut = tempTT
      cashOut = cashInto - (tempTT - marginAccountInto)
      notice = paste("Margin call:",(tempTT - marginAccountInto),"???")
    }
    else{
      marginAccountOut = marginAccountInto
      cashOut = cashInto
      notice = " "
    }
  }else if(!(is.null(shortPosition))){
    for(i in 1:len(shortPosition)) {
      tempTT = tempTT + shortPosition[i] * shortPrice[i] * initialMarginRate
    }
    if(marginAccountInto < (tempTT * maintainenceRatio)){
      marginAccountOut = tempTT
      cashOut = cashInto - (tempTT - marginAccountInto)
      notice = paste("Margin call:",(tempTT - marginAccountInto),"???")
    }
    else{
      marginAccountOut = marginAccountInto
      cashOut = cashInto
      notice = " "
    }
  }
  else{
    marginAccountOut = marginAccountInto
    cashOut = cashInto
    notice = " "
  }
  
  lst <- list(cashOut,marginAccountOut,notice)
  return(lst)
}

# Evaluate strategy performance;
strategyEval <- function(PFValue,rateRiskFree){
  retSeries = diff(PFValue)/PFValue[1:(len(PFValue)-1)];
  annualRet = ((PFValue[len(PFValue)]-PFValue[1])/PFValue[1])*252/len(PFValue);
  annualVola = sd(retSeries)*sqrt(252);
  sharpeRatio = (annualRet - rateRiskFree)/annualVola;
  ddSeries = c()
  for(i in 1:len(PFValue)){
    tp = max(PFValue[1:i]);
    ddSeries[i] = max((tp-PFValue[i])/tp,0);
  }
  mdd = max(ddSeries)
  
  ary <- c(annualRet,annualVola,sharpeRatio,mdd)
}
############################################################################################
# Load packages from: https://github.com/systematicinvestor/SIT/raw/master/sit.gz;
############################################################################################
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)
load.packages('quantmod')
############################################################################################
# Read data into the work space;
############################################################################################ 
fnList = c("HS300.csv","ZZ500.csv","SZ50.csv")
tickersList = c("HS300","ZZ500","SZ50")
# Loop through each contract;
for (fni in 1:3){
  # Load the data file;
  tickers = tickersList[i]
  filename = fnList[i]
  data <- read.csv(filename)
  rownames(data) <- data$Date
  data <- data[,-1,drop=FALSE]
  data = as.xts(data)
  # Parameters setting;
  refLength = floor((2/3)*len(data[,1]))
  RW = 30
  pre_RW = 7
  bkt = 1
  # Initalize portfolio figures;
  initialCapital = 100000000
  longPosition = c()
  longPrice = c()
  shortPosition = c()
  shortPrice = c()
  oneShot = 3000
  PFrecorder = c()
  initialMarginRate = 0.15
  maintainenceRatio = 0.75
  marginAccount = 0
  cash = initialCapital
  PFrecorder[1] = cash + marginAccount
  
  while(bkt<=(len(data[,1])-refLength)-RW-pre_RW+1){
    referencePrice = coredata(Cl(data))[1:refLength] # Retrieve historical reference price sequences;
    n = len(referencePrice)
    queryDataPrice = data[(refLength+1):len(data[,1])][bkt:(bkt+RW-1),1] 
    queryPrice = coredata(Cl(data))[(refLength+1):len(data[,1])][bkt:(bkt+RW-1)] # Retrieve query price squence;
    
    n.queryPrice = len(queryPrice)
    n.referencePrice = len(referencePrice)
    
    #*****************************************************************
    # Compute Distances for prices;
    #*****************************************************************         
    distPrice = rep(NA, n.referencePrice)
    queryPrice.normalized = (queryPrice - mean(queryPrice)) / sd(queryPrice)
    
    for( i in n.queryPrice : n.referencePrice ) {
      window = referencePrice[ (i - n.queryPrice + 1) : i]
      window.normalized = (window - mean(window)) / sd(window)
      distPrice[i] = stats:::dist(rbind(queryPrice.normalized, window.normalized))
    }
    
    #******************************************************************
    # Find Matches;
    #******************************************************************             
    min.indexPrice = c()
    n.matchPrice = 100
    
    # Only look at the minimums;
    tempPrice = distPrice
    tempPrice[ tempPrice > mean(distPrice, na.rm=T) ] = NA
    
    # Remove n.query, points to the left/right of the minimums;
    for(i in 1:n.matchPrice) {
      if(any(!is.na(tempPrice))) {
        index = which.min(tempPrice)
        min.indexPrice[i] = index
        tempPrice[index] = NA
      }
    }
    
    #*****************************************************************
    # Plot Matches;
    #******************************************************************         
    datesPrice = index(data)[1:len(distPrice)]
    
    par(mar=c(2, 4, 2, 2))
    plot(datesPrice, distPrice, type='l',col='gray', main='Top Matches: Price', ylab='Euclidean Distance', xlab='')
    abline(h = mean(distPrice, na.rm=T), col='darkgray', lwd=2)
    points(datesPrice[min.indexPrice], distPrice[min.indexPrice], pch=22, col='red', bg='red')
    text(datesPrice[min.indexPrice], distPrice[min.indexPrice], 1:n.matchPrice, adj=c(1,1), col='black',xpd=TRUE)
    
    dataPrice = data[,1]
    plota(dataPrice, type='l', col='gray', main=tickers)
    plota.lines(queryDataPrice, col='blue')
    for(i in 1:n.matchPrice) {
      plota.lines(dataPrice[(min.indexPrice[i]-n.queryPrice + 1):min.indexPrice[i]], col='red')
    }
    text(index(data)[min.indexPrice - n.queryPrice/2], referencePrice[min.indexPrice - n.queryPrice/2], 1:n.matchPrice, 
         adj=c(1,-1), col='black',xpd=TRUE)
    plota.legend('Pattern,Match #','blue,red')
    
    #*****************************************************************
    # Overlay all Matches;
    #******************************************************************         
    matchesPrice = matrix(NA, nr=(n.matchPrice+1), nc=3*n.queryPrice)
    tempPrice = c(rep(NA, n.queryPrice), referencePrice, queryPrice)
    for(i in 1:n.matchPrice) {
      matchesPrice[i,] = tempPrice[ (min.indexPrice[i] - n.queryPrice + 1):(min.indexPrice[i] + 2*n.queryPrice) ]   
    }
    
    # Add the 'query' pattern;
    matchesPrice[(n.matchPrice+1),] = tempPrice[ (len(tempPrice) - 2*n.queryPrice + 1):(len(tempPrice) + n.queryPrice) ]       
    
    # Normalize;
    for(i in 1:(n.matchPrice+1)) {
      matchesPrice[i,] = matchesPrice[i,] / matchesPrice[i,n.queryPrice]
    }
    
    #*****************************************************************
    # Plot all Matches;
    #******************************************************************                 
    tempPrice = 100 * ( t(matchesPrice[,-c(1:n.queryPrice)]) - 1)
    
    par(mar=c(2, 4, 2, 2))
    matplot(tempPrice, type='l',col='gray',lwd=2, lty='dotted', xlim=c(1,2.5*n.queryPrice),
            main = paste('Pattern Prediction with', n.matchPrice, 'neighbours'),ylab='Normalized', xlab='')
    lines(tempPrice[,(n.matchPrice+1)], col='black',lwd=4)
    
    points(rep(2*n.queryPrice,n.matchPrice), tempPrice[2*n.queryPrice,1:n.matchPrice], pch=21, lwd=2, col='gray', bg='gray')
    
    bt.plot.dot.label <- function(x, dataPrice, xfun, col='red') {
      for(j in 1:len(xfun)) {
        y = match.fun(xfun[[j]])(dataPrice)
        points(x, y, pch=21, lwd=4, col=col, bg=col)
        text(x, y, paste(names(xfun)[j], ':', round(y,1),'%'),
             adj=c(-0.1,0), cex = 0.8, col=col,xpd=TRUE)         
      }
    }
    
    tempPrice[is.na(tempPrice)] = 0
    bt.plot.dot.label(2*n.queryPrice, tempPrice[2*n.queryPrice,1:n.matchPrice], 
                      list(Min=min,Max=max,Median=median,'Bot 25%'=function(x) quantile(x,0.25),'Top 75%'=function(x) quantile(x,0.75)))
    bt.plot.dot.label(n.queryPrice, tempPrice[n.queryPrice,(n.matchPrice+1)], list(Current=min))
    
    
    #*****************************************************************
    # Compute Distances for volume;
    #******************************************************************
    # Set up data:
    referenceVol = coredata(Vo(data))[1:refLength]
    n = len(referenceVol)
    queryDataVol = data[(refLength+1):len(data[,1])][bkt:(bkt+RW-1),2]
    queryVol = coredata(Vo(data))[(refLength+1):len(data[,1])][bkt:(bkt+RW-1)]
    
    n.queryVol = len(queryVol)
    n.referenceVol = len(referenceVol)
    
    distVol = rep(NA, n.matchPrice)
    queryVol.normalized = (queryVol - mean(queryVol)) / sd(queryVol)
    
    for( i in 1 : len(min.indexPrice) ) {
      window = referenceVol[min.indexPrice[i]: (min.indexPrice[i]+RW-1)]
      window.normalized = (window - mean(window)) / sd(window)
      distVol[i] = stats:::dist(rbind(queryVol.normalized, window.normalized))
    }
    
    #******************************************************************
    # Find Matches;
    #******************************************************************             
    min.indexVol = c()
    countVol = c()
    n.matchVol = 30
    
    # Only look at the minimums;
    tempVol = distVol
    tempVol[ tempVol > mean(distVol, na.rm=T) ] = NA
    
    # Remove n.query, points to the left/right of the minimums
    for(i in 1:n.matchVol) {
      if(any(!is.na(tempVol))) {
        index = which.min(tempVol)
        min.indexVol[i] = min.indexPrice[index]
        countVol[i] = index
        tempVol[index] = NA
      }
    }
    
    #*****************************************************************
    # Plot Matches;
    #******************************************************************         
    datesVol = index(data)[min.indexPrice]
    datesVol = sort(datesVol)
    distVol = distVol[order(datesVol)]
    
    par(mar=c(2, 4, 2, 2))
    plot(datesVol, distVol, type='l',col='gray', main='Top Matches: plus Volume', ylab='Euclidean Distance', xlab='')
    abline(h = mean(distVol, na.rm=T), col='darkgray', lwd=2)
    points(datesVol[countVol], distVol[countVol], pch=22, col='red', bg='red')
    text(datesVol[countVol], distVol[countVol], 1:n.matchVol, adj=c(1,1), col='black',xpd=TRUE)
    
    plota(data[,2], type='l', col='gray', main=tickers)
    plota.lines(queryDataVol, col='blue')
    for(i in 1:n.matchVol) {
      plota.lines(data[,2][(min.indexVol[i]-n.queryVol + 1):min.indexVol[i]], col='red')
    }
    text(index(data)[min.indexVol - n.queryVol/2], referenceVol[min.indexVol - n.queryVol/2], 1:n.matchVol, 
         adj=c(1,-1), col='black',xpd=TRUE)
    plota.legend('Pattern,Match #','blue,red')
    
    #*****************************************************************
    # Overlay all Matches;
    #******************************************************************         
    matchesVol = matrix(NA, nr=(n.matchVol+1), nc=3*n.queryVol)
    tempVol = c(rep(NA, n.queryVol), referenceVol, queryVol)
    for(i in 1:n.matchVol) {
      matchesVol[i,] = coredata(dataPrice)[ (min.indexVol[i] - n.queryVol + 1):(min.indexVol[i] + 2*n.queryVol) ]   
    }
    
    # Add the 'query' pattern;
    matchesVol[(n.matchVol+1),] = coredata(dataPrice)[ (len(tempVol) - 2*n.queryVol + 1):(len(tempVol) + n.queryVol) ]       
    
    # Normalize;
    for(i in 1:(n.matchVol+1)) {
      matchesVol[i,] = matchesVol[i,] / matchesVol[i,n.queryVol]
    }
    
    #*****************************************************************
    # Plot 30 Matches;
    #******************************************************************    
    dataPrice = data[,1]
    plota(dataPrice, type='l', col='gray', main=tickers)
    plota.lines(queryDataPrice, col='blue')
    for(i in 1:n.matchVol) {
      plota.lines(dataPrice[(min.indexVol[i]-n.queryVol + 1):min.indexVol[i]], col='red')
    }
    text(index(data)[min.indexVol - n.queryVol/2], referencePrice[min.indexVol - n.queryVol/2], 1:n.matchVol, 
         adj=c(1,-1), col='black',xpd=TRUE)
    plota.legend('Pattern,Match #','blue,red')
    
    #*****************************************************************
    # Plot all Matches;
    #******************************************************************                 
    tempVol = 100 * ( t(matchesVol[,-c(1:n.queryVol)]) - 1)
    
    par(mar=c(2, 4, 2, 2))
    matplot(tempVol, type='l',col='gray',lwd=2, lty='dotted', xlim=c(1,2.5*n.queryVol),
            main = paste('Pattern Prediction with', n.matchVol, 'neighbours'),ylab='Normalized', xlab='')
    lines(tempPrice[,(n.matchPrice+1)], col='black',lwd=4)
    
    points(rep(2*n.queryVol,n.matchVol), tempVol[2*n.queryVol,1:n.matchVol], pch=21, lwd=2, col='gray', bg='gray')
    
    data_vol = data[,1]
    bt.plot.dot.label <- function(x, data_vol, xfun, col='red') {
      for(j in 1:len(xfun)) {
        y = match.fun(xfun[[j]])(data_vol)
        points(x, y, pch=21, lwd=4, col=col, bg=col)
        text(x, y, paste(names(xfun)[j], ':', round(y,1),'%'),
             adj=c(-0.1,0), cex = 0.8, col=col,xpd=TRUE)         
      }
    }
    
    bt.plot.dot.label(2*n.queryVol, tempVol[2*n.queryVol,1:n.matchVol], 
                      list(Min=min,Max=max,Median=median,'Bot 25%'=function(x) quantile(x,0.25),'Top 75%'=function(x) quantile(x,0.75)))
    bt.plot.dot.label(n.queryVol, tempVol[n.queryVol,(n.matchVol+1)], list(Current=min))
    
    #*****************************************************************
    # Table with predictions;
    #******************************************************************         
    temp = matrix( double(), nr=(n.matchVol+3), 1)
    rownames(temp) = c(1:n.matchVol, spl('Min,Average,Max'))
    colnames(temp) = "Pre_Rets"
    
    # compute returns
    for(j in 1:n.matchVol){
      temp[j,'Pre_Rets'] = data.matrix(data[,1][min.indexVol[j]+pre_RW])[1]/data.matrix(data[,1][min.indexVol[j]])[1]
    }
    
    # Compute minimal, maximal and average returns;
    index = 'Pre_Rets'
    temp['Min',index] = min(temp[1:(n.matchVol),index])
    temp['Average',index] = mean(temp[1:(n.matchVol),index])
    temp['Max',index] = max(temp[1:(n.matchVol),index])
    
    # Format;
    temp = temp - 1
    # Find the significance level for the average of the susequent return series;
    pValLes <- t.test(temp[1:n.matchVol,1],mu=0,alternative = 'less')$p.value
    pValGtr <- t.test(temp[1:n.matchVol,1],mu=0,alternative = 'greater')$p.value
    
    # Close all short positions and open the long position;
    if (pValGtr >= 0.95){
      recordList = record(cash,marginAccount,longPosition,longPrice,shortPosition,shortPrice,queryPrice[len(queryPrice)],queryPrice[len(queryPrice)-1])
      marginAccount = recordList[[2]]
      listClosePosition = close_short(cash,marginAccount,shortPosition,shortPrice,queryPrice[len(queryPrice)])
      cash = listClosePosition[[2]]
      marginAccount = listClosePosition[[3]]
      shortPosition = listClosePosition[[4]]
      shortPrice = listClosePosition[[5]]
      print(listClosePosition[[1]])
      print(cash)
      print(marginAccount)
      
      listPosition = long(cash,longPosition,longPrice,queryPrice[len(queryPrice)],oneShot,initialMarginRate,marginAccount)
      cash = listPosition[[2]]
      longPosition = listPosition[[3]]
      marginAccount = listPosition[[4]]
      longPrice = listPosition[[5]]
      print(listPosition[[1]])
      print(cash)
      print(marginAccount)
    }else if(pValLes >=0.95){
      # Close all long positions and open the short position;
      recordList = record(cash,marginAccount,longPosition,longPrice,shortPosition,shortPrice,queryPrice[len(queryPrice)],queryPrice[len(queryPrice)-1])
      marginAccount = recordList[[2]]
      listClosePosition = close_long(cash,marginAccount,longPosition,longPrice,queryPrice[len(queryPrice)])
      cash = listClosePosition[[2]]
      marginAccount = listClosePosition[[3]]
      longPosition = listClosePosition[[4]]
      longPrice = listClosePosition[[5]]
      print(listClosePosition[[1]])
      print(cash)
      print(marginAccount)
      
      listPosition = short(cash,shortPosition,shortPrice,queryPrice[len(queryPrice)],oneShot,initialMarginRate,marginAccount)
      cash = listPosition[[2]]
      shortPosition = listPosition[[3]]
      marginAccount = listPosition[[4]]
      shortPrice = listPosition[[5]]
      print(listPosition[[1]])
      print(cash)
      print(marginAccount)
    }
    PFrecorder[bkt+1] = cash + marginAccount
    # Record and maintain our positions;
    for(i in (bkt+1):(bkt + pre_RW - 1)){
      recordList = record(cash,marginAccount,longPosition,longPrice,shortPosition,shortPrice,coredata(data[,1])[refLength+i],coredata(data[,1])[refLength+i-1])
      marginAccount = recordList[[2]]
      PFrecorder[i+1] = recordList[[1]]
      maintainList = maintain(cash,marginAccount,longPosition,longPrice,shortPosition,shortPrice,coredata(data[,1])[refLength+i],initialMarginRate,maintainenceRatio)
      cash = maintainList[[1]]
      if (cash < 0){
        break
      }
      marginAccount = maintainList[[2]]
      print(maintainList[[3]])
    }
    if (cash < 0){
      print("??????,??????")
      break
    }
    bkt = bkt + pre_RW
  }
  # End of the backtesting: close all short and long positions;
  listClosePosition = close_short(cash,marginAccount,shortPosition,shortPrice,coredata(data[,1])[len(data[,1])])
  cash = listClosePosition[[2]]
  marginAccount = listClosePosition[[3]]
  shortPosition = listClosePosition[[4]]
  shortPrice = listClosePosition[[5]]
  print(listClosePosition[[1]])
  print(cash)
  print(marginAccount)
  
  listClosePosition = close_long(cash,marginAccount,longPosition,longPrice,coredata(data[,1])[len(data[,1])])
  cash = listClosePosition[[2]]
  marginAccount = listClosePosition[[3]]
  longPosition = listClosePosition[[4]]
  longPrice = listClosePosition[[5]]
  print(listClosePosition[[1]])
  print(cash)
  print(marginAccount)
  
  write.csv(PFrecorder,paste(tickers,".csv"))
}
#######################################End of the File#######################################
