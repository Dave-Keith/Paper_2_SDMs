#Exploratory data analysis on the summarized datasets 

##Use these datasets:
#Observers_Scallop_TripSet.csv
#Temporal_Observers_Scallop_Set.csv

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
t<-read.csv(paste(path,"Data/Summaries/Observers_Scallop_TripSet.csv",sep=""),stringsAsFactors = F)


###############
###############
###a) For each "number" fish variable - do the following, and save to individual pdf's by the name of the fish variable

for (num_col in names(t)) {
  if(!(grepl("_num_", num_col))) {
    next
  }
  
  n <- num_col
  fish <- t[,num_col]
  
  pdf(paste(path, "Results/Figures/Covariates/Observer_Scallop/", n, ".pdf", sep=""))
  par(mfrow = c(2,3))
  
  for (i in num_col) {
    e <- i
    env <- t[,i]
    t$year2<-gsub('.{1}$', '', t$year) #truncate the year to plot by decade (i.e., 198,199,200,210)
    
    plot(t$lon.dd, t$lat.dd, pch = 1, cex = (fish/100), col = t$year2)
    #with(subset(t, fish == 0), points(lon_dd, lat.dd, pch = "x", col = "red", cex = 0.3, lwd = 0.8))
    legend("bottomright", legend=unique(t$year2), col=unique(t$year2), pch=1, cex=0.8)
    
    x<- lm(t$lat.dd ~ fish)
    xsum<-summary(x)
    r2 = xsum$adj.r.squared
    x.p<-xsum$coefficients[2,4]
    plot(t$lat.dd, fish, pch = 1, cex = 1, col = t$year2)
    abline(x)
    m = round(x$coefficients, 2)
    mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    rp = vector('expression',2)
    rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
    rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
    legend('topright', legend = rp, bty = 'n')
    
    x<- lm(t$lon.dd ~ fish)
    xsum<-summary(x)
    r2 = xsum$adj.r.squared
    x.p<-xsum$coefficients[2,4]
    plot(t$lon.dd, fish, pch = 1, cex = 1, col = t$year2)
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
# Need to reload in t as it gets modified above...
t<-read.csv(paste(path,"Data/Summaries/Observers_Scallop_TripSet.csv",sep=""),stringsAsFactors = F)

for (num_col in names(t)) {
  if(!(grepl("PA", num_col))) {
    next
  }
  
  n <- num_col
  fish <- t[,num_col]
  
  pdf(paste(path, "Results/Figures/Covariates/Observer_Scallop/", n, ".pdf", sep=""))
  par(mfrow = c(2,3))
  
  for (i in num_col) {
    e <- i
    env <- t[,i]
    t$year2<-gsub('.{1}$', '', t$year) #truncate the year to plot by decade (i.e., 198,199,200,210)
    
    plot(t$lon.dd, t$lat.dd, pch = 1, cex = 1, col = t$year2)
    #with(subset(t, fish == 0), points(lon_dd, lat.dd, pch = "x", col = "red", cex = 0.3, lwd = 0.8))
    legend("bottomright", legend=unique(t$year2), col=unique(t$year2), pch=1, cex=0.8)
    
    #x<- lm(t$lat.dd ~ fish)
    #xsum<-summary(x)
    #r2 = xsum$adj.r.squared
    #x.p<-xsum$coefficients[2,4]
    plot(t$lat.dd, jitter(fish, 0.2), pch = 1, cex = 1, col = t$year2)
    abline(x)
    #m = round(x$coefficients, 2)
    #mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    #rp = vector('expression',2)
    #rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
    #rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
    #legend('topright', legend = rp, bty = 'n')
    
    #x<- lm(t$lon.dd ~ fish)
    #xsum<-summary(x)
    #r2 = xsum$adj.r.squared
    #x.p<-xsum$coefficients[2,4]
    plot(t$lon.dd, jitter(fish, 0.2), pch = 1, cex = 1, col = t$year2)
    abline(x)
    #m = round(x$coefficients, 2)
    #mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
    #rp = vector('expression',2)
    #rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2,dig=3)))[2]
    #rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(x.p, digits = 2)))[2]
    #legend('topright', legend = rp, bty = 'n')
    
    hist((fish), breaks = 100) 
    plot(t$year, (fish), pch = 1, cex = 0.5)
  }
  dev.off()
}

#######################
#### B. Temporal
#######################
#rm(list=ls())


#path <- "/Users/user/DFO_2017/"
t<-read.csv(paste(path,"Data/Summaries/Temporal_Observers_Scallop_Set.csv",sep=""),stringsAsFactors = F)

pdf(paste(path, "Results/Figures/Covariates/Observer_Scallop/temporal.pdf", sep=""))
par(mfrow = c(3,3))
cols = names(t[,c(2:59)])
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

# rm(list=ls())
# 
# path <- "/Users/user/DFO_2017/"
t<-read.csv(paste(path,"Data/Summaries/Observers_Scallop_TripSet.csv",sep=""),stringsAsFactors = F)

for (num_col in names(t)) {
  if(!(grepl("est_num_caught", num_col))) {
    next
  }
  
  n <- num_col
  f <- t[t[,num_col] > 0 ,]
  fish <- f[,num_col]
  
  pdf(paste(path, "Results/Figures/Covariates/Observer_Scallop/env_", n, ".pdf", sep=""))
  par(mfrow = c(3,3))
  cols = names(f[,c(25:66)])
  
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
#a) only for *count_a

# rm(list=ls())
#   
#   path <- "/Users/user/DFO_2017/"
#  t<-read.csv(paste(path,"Data/Summaries/Observers_Scallop_TripSet.csv",sep=""),stringsAsFactors = F)
  
  for (num_col in names(t)) {
    if(!(grepl("est_num_caught", num_col))) {
      next
    }
    
    n <- num_col
    f <- t[t[,num_col] >= 0 ,]
    fish <- f[,num_col]
    
    pdf(paste(path, "Results/Figures/Covariates/Observer_Scallop/env_CDF_", n, ".pdf", sep=""))
    par(mfrow = c(3,3))
    cols = names(f[,c(25:66)])
    
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
  