# This is a function to plot the prediction fields for different proportional areas, still a work in progress

pred.fields <- function(plt.area = "GOM",pred.dat,prob = 0.8 ,p.grid = NULL, 
                        p.crs = st_crs(mesh.grid),loc.text = c(600000,4450000),
                        plots = c('video','cog','facet')) 
{
  
  n.pred.mods <- length(pred.dat)
  
  bp <- pecjector(area=plt.area,plot=F,direct_fns = 'github',add_layer = list(eez = 'eez',nafo = 'main',scale.bar = 'tl'),c_sys = 32619,buffer = 0.05)
  # Set up my colour ramp for the maps, stolen from pectinid
  col <- addalpha(pals::viridis(100),1)
  brk <- seq(prob,1,by=0.05)
  lims <- range(brk)
  sf <- scale_fill_gradientn(colours = col, limits=lims,breaks=brk,name="Probability")
  sc <- scale_colour_gradientn(colours = col, limits=lims,breaks=brk,name="Probability")
  
  for(i in 1:n.pred.mods) {
    
    res <- pred.op.tmp[[i]]
    n.eras <- length(unique(res$years_5))
    # Get the species
    if(grepl("yt",res$species[1])) sp <- "YT"
    if(grepl("cod",res$species[1])) sp <- "cod"
    # Get the survey name
    surv <- res$survey[1]
    
    # This puts the mesh grids in here instead of using the points in the object, this just makes for a nicer output object.
    if(!is.null(p.grid)) st_geometry(res) <- st_geometry(rep(p.grid,n.eras)) 
    # We can also just use the points in the object if we don't specify the prediction grid.
    if(is.null(p.grid)) res <- st_as_sf(res,coords = c("X","Y"),crs = p.crs)
    
    for(n in min(eras):max(eras))
    {
      yrs <- paste0(substr(dat %>% filter(years_5 == n) %>% summarise(min = min(year)),3,4),"-",
                    substr(dat %>% filter(years_5 == n) %>% summarise(max = max(year)),3,4))
      if(substr(yrs[1],1,2) > 30) { yrs <- paste0(19,yrs)} else {yrs <- paste0(20,yrs)}
      res$yrs[res$years_5==n] <- yrs
    }
    # So calculating area is smart using that set units, though they are all idenitcal...
    res$area <- res %>% st_area() %>% set_units("km^2")
    
    
    # Calculate the center of gravity Here's a nice way to return an object with multiple ouptuts
    cog <- as.data.table(res)[,cog.calc(X,Y,pred), by = yrs]
    cog <- st_as_sf(cog,coords = c('x','y'), crs= p.crs, remove=F)
    
    # Here I'm making a column name that is nice year values (for the figure title) rather than just era 1, etc... 
    
    # Now get the area in which the probability of encounter is >= prob
    area.era <- data.frame(res) %>% dplyr::filter(pred >= prob) %>% group_by(yrs) %>% summarize(tot.area = sum(area))
    #area.era
    area.era$X <- loc.text[1] 
    area.era$Y <- loc.text[2]
    area.era <- st_as_sf(area.era,crs=p.crs,coords = c("X","Y"), remove=F)
    
    res <- res %>% left_join(data.frame(area.era) %>% dplyr::select(yrs,tot.area),by= "yrs")
    
    # Add the area to the res object so it's easy to plot...
    res$tot.area <- round(res$tot.area,digits=0)
    
    mn.prop <- data.frame(res) %>% group_by(yrs) %>% dplyr::summarise(mn = mean(pred),med = median(pred), sd = sd(pred))
    mn.prop
    
    cog.map.plt <-  bp + geom_sf_text(data = cog, aes(label=substr(yrs,3,8)),size=2) + 
      #geom_errorbar(data = cog,aes(x= x,ymin=y - 3*se.y,ymax=y + 3*se.y),colour = "blue",width=0)  + 
      #geom_errorbar(data = cog,aes(y= y,xmin=x - 3*se.x,xmax=x + 3*se.x),colour = "blue",width=0)  + 
      theme_bw()
    
    cog.sd.plt <-  ggplot(data = cog) + geom_label(aes(x=x, y = y+3.5*se.y,label=substr(yrs,3,8)),size=4) + 
      geom_errorbar(aes(x= x,ymin=y - 3*se.y,ymax=y + 3*se.y),colour = "blue",width=0)  + 
      geom_errorbar(aes(y= y,xmin=x - 3*se.x,xmax=x + 3*se.x),colour = "blue",width=0)  + 
      theme_bw() + xlab("") + ylab("")
    
    #windows(11,11)
    saveGIF(
      {
        ani.options(interval = 2, nmax = 50,ffmpeg = 'C:/Program Files/ImageMagick-7.0.8-Q16/ffmpeg.exe')
        for (p in 1:n.eras) 
        {
          # Note that the summar.fixed works because our covariates are treated as random walks which makes them random effects so our only fixed term
          # is always just our intercept.
          res.t <- res %>% filter(years_5 == p)
          area.t <- area.era %>% filter(yrs ==res.t$yrs[1])
          area.t$tot.area <- round(area.t$tot.area,digits =0)
          plt<-bp + geom_sf(data = res.t %>% dplyr::filter(pred >= prob),aes(fill = pred,colour=pred))+ # facet_wrap(~years_5) + 
            coord_sf(datum=p.crs) + sf + sc +
            #geom_sf_text(data=area.t,aes(label =as.expression(bquote(Area== .(tot.area)~km^2)),parse=T) ) +
            annotate('text',x=area.t$X,y=area.t$Y, label=as.expression(bquote(Area== .(area.t$tot.area)~km^2)),parse=T) +
            ggtitle(paste0("Encounter probability \u2265 ",prob,": ",sp,"-",surv,' survey(',area.t$yrs[1],")")) 
          print(plt)
          ani.pause()
        }
      }, movie.name = paste0(direct.proj,'Results/Figures/INLA/Encounter_probability_lte_',prob,"-",sp,"-",surv,'_survey.gif'), ani.width = 800, ani.height = 800)
    
    # Now to get a fancy label set up we can do this, then attached this to the area.era object
    lab <- NA
    for(k in 1:n.eras) lab[k] <- paste0("Area==~",round(area.era$tot.area[k],digits=0),"*km^2")
    area.era$lab <- lab
    
    # Facet version of the above NOTE HOW THE YEARS ARE F'd up now...
    plt<-bp + geom_sf(data = res %>% dplyr::filter(pred >= prob ) ,aes(fill = pred,colour=pred))+ 
      facet_wrap(~as.factor(yrs)) + 
      coord_sf(datum=p.crs) + sf + sc + theme_map() +
      geom_sf_text(data = area.era , aes(label = lab),parse=T) +
      #geom_text(data=area.t,aes(label =as.expression(bquote(Area== .(tot.area)~km^2)),parse=T) ) +
      #annotate('text',x=area.era$X,y=area.era$Y, label=tst,parse=T) +
      # Note for the >= symbol to show up correctly in a pdf use cairo_pdf rather than just pdf! Just trying to avoid using an expression here..
      ggtitle(paste0("Encounter probability \u2265 ",prob," - ", sp, " - ", surv, " survey")) 
    ggsave(plt, file = paste0(direct.proj,'Results/Figures/INLA/Encounter_probability_lte_',prob,"-",sp,"-",surv,'_survey.png'),width = 11,height = 11,units = 'in')
    
  }
}