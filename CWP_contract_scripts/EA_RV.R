#Exploratory data analysis on the summarized datasets 

##Use these datasets:
#RVSurvey.csv
#RVTemporal.csv

##To describe:
#A. Spatial and temporal patterns in fish populations
#B. Temporal patterns in fish and environment
#C. Fish vs Environment


#####################################################
#####################################################
#A. Spatial and temporal patterns of fish populations
####Produces pdf's of graphs
####Produces summaries, which I input into Excel table
#####################################################
#####################################################

rm(list=ls())
load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
require(maps)
require(mapdata)
library(lattice)
library(MASS)
library(ggplot2)

path <- "/Users/user/DFO_2017/"
path <- "Y:/Projects/GB_time_area_closure_SPERA/"
t<-read.csv(paste(path,"Data/Summaries/RVSurvey.csv",sep=""),stringsAsFactors = F)


###############
###############
###a) For each "number" fish variable - do the following, and save to individual pdf's by the name of the fish variable

for (num_col in names(t)) {
  if(!(grepl("number", num_col))) {
    next
  }
  
  n <- num_col
  fish <- t[,num_col]
  
  pdf(paste(path, "Results/Figures/Covariates/RV/", n, ".pdf", sep=""))
  par(mfrow = c(2,3))
  
  for (i in num_col) {
    e <- i
    env <- t[,i]
    t$year2<-gsub('.{1}$', '', t$year) #truncate the year to plot by decade (i.e., 198,199,200,210)
    
    plot(t$lon_dd, t$lat_dd, pch = 1, cex = (fish/100), col = t$year2)
    #with(subset(t, fish == 0), points(lon_dd, lat_dd, pch = "x", col = "red", cex = 0.3, lwd = 0.8))
    legend("bottomright", legend=unique(t$year2), col=unique(t$year2), pch=1, cex=0.8)
    
    x<- lm(t$lat_dd ~ fish)
    xsum<-summary(x)
    r2 = xsum$adj.r.squared
    x.p<-xsum$coefficients[2,4]
    plot(t$lat_dd, fish, pch = 1, cex = 1, col = t$year2)
    abline(x)
    m = round(x$coefficients, 2)
    mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    rp = vector('expression',2)
    rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
    rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
    legend('topright', legend = rp, bty = 'n')
    
    x<- lm(t$lon_dd ~ fish)
    xsum<-summary(x)
    r2 = xsum$adj.r.squared
    x.p<-xsum$coefficients[2,4]
    plot(t$lon_dd, fish, pch = 1, cex = 1, col = t$year2)
    abline(x)
    m = round(x$coefficients, 2)
    mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    rp = vector('expression',2)
    rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
    rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
    legend('topright', legend = rp, bty = 'n')
    
    hist(log(fish), breaks = 100) 
    plot(t$year, log(fish), pch = 1, cex = 0.5)
  }
  dev.off()
}

###############
###############
###b) For each "PA" fish variable - do the following:

for (num_col in names(t)) {
  if(!(grepl("PA", num_col))) {
    next
  }
  
  n <- num_col
  fish <- t[,num_col]
  
  pdf(paste(path, "Results/Figures/Covariates/RV/", n, ".pdf", sep=""))
  par(mfrow = c(2,3))
  
  for (i in num_col) {
    e <- i
    env <- t[,i]
    t$year2<-gsub('.{1}$', '', t$year) #truncate the year to plot by decade (i.e., 198,199,200,210)
    
    plot(t$lon_dd, t$lat_dd, pch = 1, cex = 1, col = t$year2)
    #with(subset(t, fish == 0), points(lon_dd, lat_dd, pch = "x", col = "red", cex = 0.3, lwd = 0.8))
    legend("bottomright", legend=unique(t$year2), col=unique(t$year2), pch=1, cex=0.8)
    
    x<- lm(t$lat_dd ~ fish)
    xsum<-summary(x)
    r2 = xsum$adj.r.squared
    x.p<-xsum$coefficients[2,4]
    plot(t$lat_dd, jitter(fish, 0.2), pch = 1, cex = 1, col = t$year2)
    abline(x)
    m = round(x$coefficients, 2)
    mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    rp = vector('expression',2)
    rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
    rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
    legend('topright', legend = rp, bty = 'n')
    
    x<- lm(t$lon_dd ~ fish)
    xsum<-summary(x)
    r2 = xsum$adj.r.squared
    x.p<-xsum$coefficients[2,4]
    plot(t$lon_dd, jitter(fish, 0.2), pch = 1, cex = 1, col = t$year2)
    abline(x)
    m = round(x$coefficients, 2)
    mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    rp = vector('expression',2)
    rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
    rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
    legend('topright', legend = rp, bty = 'n')
    
    hist((fish), breaks = 100) 
    plot(t$year, (fish), pch = 1, cex = 0.5)
  }
  dev.off()
}

###############
###############
###c) For each "_per" fish variable - do the following:

for (num_col in names(t)) {
  if(!(grepl("_per", num_col))) {
    next
  }
  
  n <- num_col
  fish <- t[,num_col]
  
  pdf(paste(path, "Results/Figures/Covariates/RV/", n, ".pdf", sep=""))
  par(mfrow = c(2,3))
  
  for (i in num_col) {
    e <- i
    env <- t[,i]
    t$year2<-gsub('.{1}$', '', t$year) #truncate the year to plot by decade (i.e., 198,199,200,210)
    
    plot(t$lon_dd, t$lat_dd, pch = 1, cex = 1, col = t$year2)
    #with(subset(t, fish == 0), points(lon_dd, lat_dd, pch = "x", col = "red", cex = 0.3, lwd = 0.8))
    legend("bottomright", legend=unique(t$year2), col=unique(t$year2), pch=1, cex=0.8)
    
    x<- lm(t$lat_dd ~ fish)
    xsum<-summary(x)
    r2 = xsum$adj.r.squared
    x.p<-xsum$coefficients[2,4]
    plot(t$lat_dd, fish, pch = 1, cex = (fish/50), col = t$year2)
    abline(x)
    m = round(x$coefficients, 2)
    mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    rp = vector('expression',2)
    rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
    rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
    legend('topright', legend = rp, bty = 'n')
    
    x<- lm(t$lon_dd ~ fish)
    xsum<-summary(x)
    r2 = xsum$adj.r.squared
    x.p<-xsum$coefficients[2,4]
    plot(t$lon_dd, fish, pch = 1, cex = (fish/50), col = t$year2)
    abline(x)
    m = round(x$coefficients, 2)
    mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    rp = vector('expression',2)
    rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
    rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
    legend('topright', legend = rp, bty = 'n')
    
    hist(log(fish), breaks = 100) 
    plot(t$year, jitter(fish, 20), pch = 1, cex = 0.5)
  }
  dev.off()
}



#######################
#### B. Temporal
#######################
# rm(list=ls())
# 
# 
# path <- "/Users/user/DFO_2017/"
t<-read.csv(paste(path,"Data/Summaries/Temporal_RV.csv",sep=""),stringsAsFactors = F)

pdf(paste(path, "Results/Figures/Covariates/RV/temporal.pdf", sep=""))
par(mfrow = c(3,3))
cols = names(t[,c(2:75)])
for (i in cols) {
 
  b <- i
  c <- t[,i]
  
  x<- lm(c ~ t$year )
  xsum<-summary(x)
  r2 = xsum$adj.r.squared
  x.p<-xsum$coefficients[2,4]
  
  plot(c ~ t$year, xlab = "year", ylab = b)
  abline(x, col = "red")
  m = round(x$coefficients, 2)
  mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
  
  rp = vector('expression',2)
  rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
  rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
  
  legend('topright', legend = rp, bty = 'n')

  }
dev.off()


#######################
####C. Environmental Variables 
#######################
# 
# rm(list=ls())
# 
# path <- "/Users/user/DFO_2017/"
t<-read.csv(paste(path,"Data/Summaries/RVSurvey.csv",sep=""),stringsAsFactors = F)

for (num_col in names(t)) {
  if(!(grepl("number", num_col))) {
    next
  }
  
  n <- num_col
  f <- t[t[,num_col] > 0 ,]
  fish <- f[,num_col]
  
  pdf(paste(path, "Results/Figures/Covariates/RV/env", n, ".pdf", sep=""))
  par(mfrow = c(3,3))
  cols = names(f[,c(40:94)])
  
  for (i in cols) {
    e <- i
    env <- f[,i]
    x<- lm(env ~ fish)
    xsum<-summary(x)
    r2 = xsum$adj.r.squared
    x.p<-xsum$coefficients[2,4]
    
    plot(fish ~ env, xlab = e, ylab = n)
    abline(x)
    
    m = round(x$coefficients, 2)
    mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    
    rp = vector('expression',2)
    rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
    rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
    
    legend('topright', legend = rp, bty = 'n')
    
  }
  dev.off()
}

#######################
####C. Environmental Variables - CDF - 
#######################
#a) only for *_number_Adu
# 
# rm(list=ls())
# 
# path <- "/Users/user/DFO_2017/"
# t<-read.csv(paste(path,"Data/Summaries/RVSurvey.csv",sep=""),stringsAsFactors = F)

for (num_col in names(t)) {
  if(!(grepl("_number_Adu", num_col))) {
    next
  }
  
  n <- num_col
  f <- t[t[,num_col] >= 0 ,]
  fish <- f[,num_col]
  
  pdf(paste(path, "Results/Figures/Covariates/RV/env_CDF_", n, ".pdf", sep=""))
  par(mfrow = c(3,3))
  cols = names(f[,c(40:94)])
  
  for (i in cols) {
    e <- i
    env <- f[,i]
    tmp <- data.frame(fish=fish,env=env)
    cdf <- tmp[order(tmp$env),]
    cdf$cum_fish[1] <- cdf$fish[1]
    for(j in 2:length(cdf$fish)) cdf$cum_fish[j] <- cdf$cum_fish[j-1] + cdf$fish[j]
    # This gets the upper lower 95% cut off
    LCI <- cdf$env[max(which(cdf$cum_fish <= 0.025 * max(cdf$cum_fish)))]
    UCI <- cdf$env[min(which(cdf$cum_fish >= 0.975 * max(cdf$cum_fish)))]
    
    #f.cdf <- ecdf(cdf$fish)
    plot(cdf$cum_fish~cdf$env, xlab = e, ylab = n)
    legend("topleft",c(paste("LCI =",signif(LCI,digits=2),
                           "UCI =",signif(UCI,digits=2))),bty="n",cex=0.75)
    x<- lm(env ~ fish)
    xsum<-summary(x)
    r2 = xsum$adj.r.squared
    x.p<-xsum$coefficients[2,4]
    
    plot(fish ~ env, xlab = e, ylab = n)
    abline(x)
    
    m = round(x$coefficients, 2)
    mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    
    rp = vector('expression',2)
    rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
    rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
    
    legend('topright', legend = rp, bty = 'n')
    
  }
  dev.off()
}

#b) only for *_number_Tot
# rm(list=ls())
# 
# path <- "/Users/user/DFO_2017/"
# t<-read.csv(paste(path,"Data/Summaries/RVSurvey.csv",sep=""),stringsAsFactors = F)

for (num_col in names(t)) {
  if(!(grepl("_number_Tot", num_col))) {
    next
  }
  
  n <- num_col
  f <- t[t[,num_col] >= 0 ,]
  fish <- f[,num_col]
  
  pdf(paste(path, "Results/Figures/Covariates/RV/env_CDF_", n, ".pdf", sep=""))
  par(mfrow = c(3,3))
  cols = names(f[,c(40:94)])
  
  for (i in cols) {
    e <- i
    env <- f[,i]
    tmp <- data.frame(fish=fish,env=env)
    cdf <- tmp[order(tmp$env),]
    cdf$cum_fish[1] <- cdf$fish[1]
    for(j in 2:length(cdf$fish)) cdf$cum_fish[j] <- cdf$cum_fish[j-1] + cdf$fish[j]
    # This gets the upper lower 95% cut off
    LCI <- cdf$env[max(which(cdf$cum_fish <= 0.025 * max(cdf$cum_fish)))]
    UCI <- cdf$env[min(which(cdf$cum_fish >= 0.975 * max(cdf$cum_fish)))]
    
    #f.cdf <- ecdf(cdf$fish)
    plot(cdf$cum_fish~cdf$env, xlab = e, ylab = n)
    legend("topleft",c(paste("LCI =",signif(LCI,digits=2),
                             "UCI =",signif(UCI,digits=2))),bty="n",cex=0.75)
    x<- lm(env ~ fish)
    xsum<-summary(x)
    r2 = xsum$adj.r.squared
    x.p<-xsum$coefficients[2,4]
    
    plot(fish ~ env, xlab = e, ylab = n)
    abline(x)
    
    m = round(x$coefficients, 2)
    mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    
    rp = vector('expression',2)
    rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
    rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
    
    legend('topright', legend = rp, bty = 'n')
    
  }
  dev.off()
}
