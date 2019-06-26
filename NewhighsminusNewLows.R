setwd("C:/Users/charl/Desktop")
library(zoo)
library(moments)
library(xts)
num_exit = 100 #maximum number of days before exit

##Read and process S&P500 data
sp500_df = read.csv('SP500.csv')
series_name = 'SP500'
colnames(sp500_df)[colnames(sp500_df)=='Close'] = series_name
#Calculate change in index
change = as.data.frame(lapply(c(4,9), FUN=function(x) c(rep(NA,x),diff(sp500_df[,series_name],lag=x))))
colnames(change) = paste(c('X5d.change','X10d.change'),series_name,sep='.')
#Calculate percentile ranking over a 100-day window
pct <- rollapply(change,100,FUN=function(x) ifelse(anyNA(x),NA,ecdf(x)(x[length(x)])), by.column=TRUE, fill=NA, align='right')
colnames(pct) = paste(c('pct_5d.change','pct_10d.change'),series_name,sep='.')
sp500 = cbind(sp500_df[,c('Date',series_name)],change,pct)

#Compute forward percentage change in SP500 for all days
exit_ret = as.data.frame(lapply(seq(1,num_exit), FUN=function(x) {
  c(diff(sp500$SP500,lag=x)/sp500$SP500[1:(length(sp500$SP500)-x)],rep(NA,x))}))
colnames(exit_ret) = paste0('X',seq(1,num_exit),'d.exit')                          
sp500 = cbind(sp500,rbind(exit_ret[-1,],rep(NA,100))) #shift by 1 day
sp500.ts = xts(sp500[,-1],order.by=as.Date(sp500[,1],format='%d/%m/%Y'))

##Read and process NYHL data
nyhl_df = read.csv('NHMNL.csv')
series_name = 'NYHL'
colnames(nyhl_df)[colnames(nyhl_df)=='PX_LAST'] = series_name
#Calculate change in index
change = as.data.frame(lapply(c(4,9), FUN=function(x) c(rep(NA,x),diff(nyhl_df[,series_name],lag=x))))
colnames(change) = paste(c('X5d.change','X10d.change'),series_name,sep='.')
#Calculate percentile ranking over a 100-day window
pct = rollapply(change,100,FUN=function(x) ifelse(anyNA(x),NA,ecdf(x)(x[length(x)])), by.column=TRUE, fill=NA, align='right')
colnames(pct) = paste(c('pct_5d.change','pct_10d.change'),series_name,sep='.')
nyhl = cbind(nyhl_df[,c('Date',series_name)],change,pct)
nyhl.ts = xts(nyhl[,-1],order.by=as.Date(nyhl[,1],format='%d/%m/%Y'))

#Merge SP500 and NYHL into one dataframe
df = merge(sp500.ts,nyhl.ts)
#Find days that qualify
qualify = ((df$X5d.change.SP500>0 | df$X10d.change.SP500>0) & (df$pct_5d.change.NYHL<0.25 | df$pct_10d.change.NYHL<0.25))
names(qualify) = 'qualify'
#Reduce dataframe to only days that qualify
df = merge(df,qualify)
df_qualify = df[which(qualify),]

##Compute summary statistics for only qualifying days
stats_list = c('mean','sd','t-stat','skew','#occurences')
sum_stats = as.data.frame(t(sapply(df_qualify[,paste0('X',seq(1,num_exit),'d.exit')], FUN=function(x) {
  y = na.omit(x)
  c('mean'=mean(y),'sd'=sd(y),'t-stat'=mean(y)/(sd(y)/sqrt(length(y))),'skewness'=skewness(y),'#occurences'=length(y))})))
colnames(sum_stats) = stats_list
sum_stats

##For comparison: compute summary statistics for SP500 (all days)
sum_stats_SP = as.data.frame(t(sapply(df[,paste0('X',seq(1,num_exit),'d.exit')], FUN=function(x) {
  y = na.omit(x)
  c('mean'=mean(y),'sd'=sd(y),'t-stat'=mean(y)/(sd(y)/sqrt(length(y))),'skewness'=skewness(y),'#occurences'=length(y))})))
colnames(sum_stats_SP) = stats_list
sum_stats_SP/sum_stats #compare summary statistics for all SP500 days and qualifying SP500 days

#Plot summary statistics
par(mfrow=c(3,2))
for (stat in stats_list) {plot(sum_stats[,stat],type='l',xlab='# days to exit',ylab=stat)}
plot(sum_stats$mean/c(1:100),type='l',xlab='# days to exit',ylab='mean return / # days')
