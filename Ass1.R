library(quantmod)
setwd("/Users/siukwanyuen/Desktop/EF4822(R)/PS1")


##Question 2
## A function that generate summary of stock data, only work with the stock data obtained by quantmod
stockSummary <-function(Symbols){
  Sym <- Symbols
  SR_all <- diff(Sym)/ lag(Sym) 
  LR_all <- log(Sym/lag(Sym))
  SR <- SR_all[,4]
  LR <- LR_all[,4]
  SRMean <- mean(SR,na.rm=TRUE)
  LRMean <- mean (LR,na.rm=TRUE)
  SRVar <- var(SR,na.rm=TRUE)
  LRVar <- var(LR,na.rm=TRUE)
  SRMax <- max(SR,na.rm=TRUE)
  LRMax <- max(LR,na.rm=TRUE)
  SRMin <- min(SR,na.rm=TRUE)
  LRMin <- min(LR,na.rm=TRUE)
  summarySR <- c(SRMean, SRVar,SRMax,SRMin)
  summaryLR <- c(LRMean, LRVar,LRMax,LRMin)
  summary <- c(summarySR,summaryLR)
  lookUpTable <- c("Mean of simple return: ", "Variance of simple return: ","Max Value of simple return: ",
                     "Min Value of simple return: ", 
                   "Mean of log return: ", "Variance of log return: ","Max Value of log return: ",
                                                        "Min Value of log return: ")
  
  for(i in 1:length(summary)){
    print(lookUpTable[i])
    print(summary[i])
  }
  return(summary)
}










dev.off()
#download the data of Amazon stock, Adobe Inc stock and Johnson & Johnson stock 
getSymbols("AMZN",from = "2009-01-02", to = "2019-02-01")
getSymbols("ADBE",from = "2009-01-02", to = "2019-02-01")
getSymbols("JNJ",from = "2009-01-02", to = "2019-02-01")
#Get the simple returns and log return of each stock

#Simple Returns:Sr 
AmznSr <- diff(AMZN[,4])/ lag(AMZN[,4]) 
AdbeSr <- diff(ADBE[,4])/ lag(ADBE[,4])
JnjSr <- diff(JNJ[,4])/ lag(JNJ[,4])

#Log Return:LR
AmznLr <- log(AMZN[,4]/lag(AMZN[,4]))
AdbeLr <- log(ADBE[,4]/lag(ADBE[,4]))
JnjLr <-  log(JNJ[,4]/lag(JNJ[,4]))

#print the summary of the 3 stock  
# it also return a summary list: 
#c("Mean of simple return: ", "Variance of simple return: ","Max Value of simple return: ",
#   "Min Value of simple return: ", 
#   "Mean of log return: ", "Variance of log return: ","Max Value of log return: ",
#   "Min Value of log return: ")
AMZNSummary <- stockSummary(AMZN)
ADBESummary <- stockSummary(ADBE)
JNJSummary <- stockSummary(JNJ)

## correlations of these three stock 
stockDF <- cbind(AMZN[,4],ADBE[,4],JNJ[,4])
colnames(stockDF) <- c("Amazon","Adobe Inc","Johnson & Johnson") 
correlation <- cor(stockDF)
correlation ## Adobe Inc and Amazon have the greatest correlation 

#Plot density graph of each stock 
AMZN_density <- plot(density(AMZN[,4]))
ADBE_density <- plot(density(ADBE[,4]))
JNJ_density <- plot(density(JNJ[,4]))

#part g 
#Find out the number of observation 
ADBE_closing <- ADBE[,4]
plyr::compact(ADBE_closing) ##removing null value in the data
n1 = nrow(ADBE_closing) 
df1 = n-1 
##Denote X1 as the r.v. of Adobe simple return
mean_X1 = ADBESummary[1]
sd_X1 = sqrt(ADBESummary[2])
t_X1 =  (mean_X1-0)/(sd_X1/sqrt(n1))
t_X1 ## 2.93 
cr1 = qt(0.975,df1) ##two tail 5%  ## 1.961
## Becuase t_X1 > cr, null hypothesis is rejected 


## part h 
AMZN_closing <- AMZN[,4]
plyr::compact(AMZN_closing) ##removing null value in the data
n2 = nrow(AMZN_closing) 

JNJ_closing <- JNJ[,4]
plyr::compact(JNJ_closing) ##removing null value in the data
n3 = nrow(JNJ_closing) 

n1 == n2  ## true 
n2 == n3 ## true
## they have the same cr
cr <- cr1


mean_X2 <- AMZNSummary[1]
sd_X2 <- sqrt(AMZNSummary[2])
t_X2 <- (mean_X2-0)/(sd_X2/sqrt(n2))
(t_X2> cr || t_X2 < cr) ## true 
## therefore the  null hypothesis that AMZN has 0 daily return is rejected  

mean_X3 <- JNJSummary[1]
sd_X3 <- sqrt(JNJSummary[2])
t_X3 <- (mean_X3-0)/(sd_X3/sqrt(n3))
(t_X3 >cr || t_X2 <cr) ## false 
## therefore, the null hypothesis that JNJ has 0 daily return is NOT rejected





