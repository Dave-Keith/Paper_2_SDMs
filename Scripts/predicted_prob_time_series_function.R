# This is a function to get a summary of the area in which the probability is >= some probability.
# We can then compare all of GB, the scallop fishery area, CA1, CA2, yt and cod closures to see 
# How these all compare
# Note that this function is assuming a lot of crap that is loaded above is loaded... it's not the greatest function ever...
# Could be handy for a dashboard though.
# Arguements..
# a: pred.dat The prediction data you are using, defaults to pred.dat which is probably all this will ever be.
# b: species  The species of interest default = 'cod', other option is 'yt'.
# c: survey   The survey you are grabbing, default = 'RV', options are 'nmfs-spring', and 'nmfs-fall'.
# d: prob     The probability you are looking at, includes this value or higher for now.
# e: mesh     The mesh that was used to get the pred.dat 
# f: closure.yrs The years of the closures you want to look at, defaults to 2007-2016 which are the years we have the mesh defined as well


pred.fun <- function(pred.dat = pred.dat, species = 'cod',survey = "RV",prob = 0.75,mesh = mesh.grid,closure.yrs = 2007:2016)
{
  
  res <- pred.dat[[paste0(species,"_PA ",survey,"_survey",sep="")]]
  n.eras <- length(unique(res$years_5))
  eras <- factor.2.number(unique(res$years_5))
  # Chose your closure...
  if(species == 'cod') the.closure <- cod.closures
  if(species == 'yt') the.closure <- yt.closures
  the.closure <- the.closure %>% filter(year %in% closure.yrs)
  the.closure$area <- the.closure %>% st_area() %>% set_units("km^2")
  # So the key is the last thing is the dataframe we want...
  st_geometry(res) <- st_geometry(rep(mesh,n.eras))
  data.frame(res)
  res$strt.yr <- res$year
  res$end.yr <- res$year
  for(n in min(eras):max(eras))
  {
    yrs <- paste0(substr(dat.final %>% filter(years_5 == n, survey == unique(res$survey)) %>% summarise(min = min(year)),3,4),"-",
                  substr(dat.final %>% filter(years_5 == n, survey == unique(res$survey)) %>% summarise(max = max(year)),3,4))
    strt.yr <- dat.final %>% filter(years_5 == n, survey == unique(res$survey)) %>% summarise(min = min(year))
    end.yr <- dat.final %>% filter(years_5 == n, survey == unique(res$survey)) %>% summarise(max = max(year))
    if(substr(yrs[1],1,2) > 30) { yrs <- paste0(19,yrs)} else {yrs <- paste0(20,yrs)}
    res$yrs[res$years_5==n] <- yrs
    res$strt.yr[res$years_5==n] <- strt.yr
    res$end.yr[res$years_5==n] <- end.yr
  }  
  # # So calculating area is smart using that set units, though they are all identical...
  res$area <- res %>% st_area() %>% set_units("km^2")
  
  # Now we can look at CA1 and CA2 results
  in.CA1 <- st_intersection(res,CA1)
  in.CA1$area <- in.CA1 %>% st_area() %>% set_units("km^2")
  # What is mean encounter probability inside CA1 during each era?
  inside.CA1 <- in.CA1 %>% group_by(yrs) %>% summarise(mn = mean(pred),sd = sd(pred))
  
  in.CA2 <- st_intersection(res,CA2)
  in.CA2$area <- in.CA2 %>% st_area() %>% set_units("km^2")
  # What is mean encounter probability inside CA1 during each era?
  inside.CA2 <- in.CA2 %>% group_by(yrs) %>% summarise(mn = mean(pred),sd = sd(pred))
  
  # Now the cod and yellowtail closures is a bit more nuanced as we want to look year by year... Here's how we'd do it...
  in.closure <- NULL
  for(i in 1:length(closure.yrs)) 
  {
    in.closure[[i]] <- st_intersection(res %>% filter(strt.yr <= closure.yrs[i] & end.yr >= closure.yrs[i]), the.closure %>% filter(year == closure.yrs[i]))
    in.closure[[i]]$area <- in.closure[[i]] %>% st_area() %>% set_units("km^2")
    in.closure[[i]]$year <- closure.yrs[i]
  }
  in.closure <- do.call("rbind",in.closure)
  browser()
  inside.closure <- in.closure %>% group_by(year) %>% summarise(mn = mean(pred),lci = mean(pred.lci), uci = mean(pred.uci))
  # So we can compare this 'scallop survey area', against the closure, I think this will show that 
  # the closures are tending to be in higher than bank average areas of the bank.
  in.scal <- st_intersection(res,gb.surv)
  in.scal$area <- in.scal %>% st_area() %>% set_units("km^2")
  inside.scal <- in.scal %>% group_by(yrs) %>% summarise(mn = mean(pred),lci = mean(pred.lci), uci = mean(pred.uci))
  
  # What we can do next is get the area in which the P(E) is >= 75% over time on GB to show that it's a big area compared to the closures...
  hi.CA1 <- in.CA1 %>% filter(pred >=prob) %>% group_by(yrs) %>% summarise(tot.area = sum(area,na.rm=T), mn.hi = mean(pred),lci.hi = mean(pred.lci),uci.hi = mean(pred.uci))
  hi.CA2 <- in.CA2 %>% filter(pred >=prob) %>% group_by(yrs) %>% summarise(tot.area = sum(area,na.rm=T), mn.hi = mean(pred),lci.hi = mean(pred.lci),uci.hi = mean(pred.uci))
  hi.scal <- in.scal %>% filter(pred >=prob) %>% group_by(yrs) %>% summarise(tot.area = sum(area,na.rm=T), mn.hi = mean(pred),lci.hi = mean(pred.lci),uci.hi = mean(pred.uci))
  st_geometry(hi.scal) <- NULL
  hi.bank <- res %>% filter(pred >=prob) %>% group_by(yrs) %>% summarise(tot.area = sum(area,na.rm=T), mn.hi = mean(pred),lci.hi = mean(pred.lci),uci.hi = mean(pred.uci))
  st_geometry(hi.bank) <- NULL
  # Of all the high area on GB how much is in Canada
  hi.scal$prop.of.GB <- as.numeric(hi.scal$tot.area/hi.bank$tot.area)
  # What proportion of the main Canadian scallop fishery contains high encounter probabiliyt
  hi.scal$prop.scal.hi <- as.numeric(hi.scal$tot.area / scal.tot.area)
  
  # The closures are a bit different here as we get a number every year, if the closure is in the same place we can get the same estimate...
  hi.closure <- in.closure %>% filter(pred >=prob) %>% group_by(year) %>% summarise(tot.area = sum(area,na.rm=T), mn.hi = mean(pred),lci.hi = mean(pred.lci),uci.hi = mean(pred.uci))
  st_geometry(hi.closure) <- NULL
  if(nrow(hi.closure) == nrow(the.closure)) hi.closure$prop.hi <- as.numeric(hi.closure$tot.area/the.closure$area)
  # HAD TO STOP HERE< THIS IS STILL BROKEN!!!
  # Add a bunch of 0's to hi.closure
  if(nrow(hi.closure) != nrow(the.closure))
  {
    # Need to pad the difference...
    dif <-  nrow(the.closure) - nrow(hi.closure)
    hi.closure <- bind_rows(hi.closure,matrix(NA,nrow = dif,ncol = ncol(hi.closure)),dimnames= list(1:dif,names(hi.closure)))
    for(i in 1:nrow(the.closure))
    {
      if(!the.closure$year[i] %in% hi.closure$year) hi.closure$year[nrow(hi.closure)+1] <- the.closure$year[i]
    }
  }
  # Now get the propotion of the high encounter probability inside the scallop fishery that is covered by the closures.
  hi.closure$prop.hi.of.scal.area <- NA
  # Need this wack loop to get the bank area lined up with the closure year
  for(i in 1:nrow(hi.closure))
  {
    tmp <- hi.closure[i,]
    tmp.eras <- as.numeric(substr(hi.scal$yrs,1,4))
    opt <- which(tmp.eras >= tmp$year)[1] # Always take the first one...
    if(length(opt) > 0) hi.closure$prop.hi.of.scal.area[i] <- as.numeric(hi.closure$tot.area[i]/hi.scal$tot.area[opt])
    if(is.na(opt))      hi.closure$prop.hi.of.scal.area[i] <- as.numeric(hi.closure$tot.area[i]/hi.scal$tot.area[nrow(hi.scal)])
  } # end for(i in 1:nrow(hi.closure))
  #    browser()
  
  # # So you can see the increase in the proportion of high encounter probabiliy being found in Canada
  # ggplot(hi.scal) + geom_point(aes(x = yrs,y=prop.can)) + xlab("Era") + ylab("Proportion of high encounter probability inside Canadian Scallop Fishery Domain") + ylim(c(0,1.01))
  # # Change in area classifed as high encounter probabilty on Georges Bank
  # ggplot(hi.bank) + geom_point(aes(x = yrs,y=as.numeric(tot.area))) + xlab("Era") + ylab("Area of high encounter probability all of Georges Bank") 
  # ggplot(hi.CA1) + geom_point(aes(x = yrs,y=as.numeric(tot.area))) + xlab("Era") + ylab("Area of high encounter probability inside Closed Area 1")
  # ggplot(hi.CA2) + geom_point(aes(x = yrs,y=as.numeric(tot.area))) + xlab("Era") + ylab("Area of high encounter probability inside Closed Area 2")
  # ggplot(hi.scal) + geom_point(aes(x = yrs,y=as.numeric(tot.area))) + xlab("Era") + ylab("Area of high encounter probability inside Canadian Scallop Fishery Domain") 
  # 
  # # Proportion of high encounter probability inside the closure
  # ggplot(hi.closure) + geom_point(aes(x = year,y=prop.hi)) + xlab("Era") + ylab("Proportion of high encounter probability inside the closure") + ylim(c(0,1.01))
  # # Proportion of Candian high encoutner probability that is inside the closure
  # ggplot(hi.closure) + geom_point(aes(x = year,y=prop.hi.of.scal.area)) + xlab("Era") + ylab("Proportion of Scallop Fishery high encounter probability found inside the closure") + 
  
  return(list(scal = hi.scal,bank = hi.bank,CA1= hi.CA1,CA2=hi.CA2,closure=hi.closure))
  
} # end function