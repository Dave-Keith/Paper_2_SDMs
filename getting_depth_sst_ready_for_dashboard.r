
library(INLA)
library(tidyverse)
library(boot)
direct.proj <- "D:/Github/"
load(file = paste0(direct.proj,"Paper_2_SDMs/data/All_model_covariate_fits.RData"))
load(paste0(direct.proj,"Paper_2_SDMs/data/INLA_mesh_input_data.RData"))

names(all.mod.chl)
input <- data.frame(survey = 'RV',model = 'model.depth.sed.sst',eras = 'st_3',species = 'yt_PA',field = 'response')

nm <- paste0(input$species," ",input$survey," survey model.",input$model," ",input$eras)
if(input$eras == 'st.10') pick.mod <- paste0(input$species," ",input$survey," survey ",input$model," ",input$eras)
if(input$eras != 'st.10') pick.mod <- paste0(input$species," ",input$survey," survey ",input$model,"_",input$eras)


int.res <- all.mod.fixed[[pick.mod]]
depth.res <- all.mod.depth[[pick.mod]]
sst.res <- all.mod.sst[[pick.mod]]

# I need all these data to get our covariates back on the correct scale.
dat.sub <- dat.final %>% dplyr::filter(survey == input$survey)
dat.sub$depth_log <- log(-dat.sub$comldepth)
dat.sub$depth_cen <-  dat.sub$depth_log - mean(dat.sub$depth_log) # Log transform should help with issues related to skew of the depth data.
dat.sub$sst_avg_cen <- scale(dat.sub$sst_avg)

# Now get the fixed terms, depth terms, and sst terms from your model as appropriate
int.res <- all.mod.fixed[[pick.mod]]

# This works fine if intercept only model
int.res$inv.mean <- inv.logit(int.res$mean[1])
int.res$LCI <- inv.logit(int.res$`0.975quant`[1])
int.res$UCI <- inv.logit(int.res$`0.025quant`[1])
# Note that this will get the intercept + the Sediment terms for the models with sed in them.
if(nrow(int.res) >1)
{
  int.res$inv.mean[2:nrow(int.res)] <- inv.logit(int.res$mean[1] + int.res$mean[2:nrow(int.res)])
  # Trun the 'effects' into a 'means' estimate, this doesn't fully account for uncertain, but get's the point across.
  int.res$mean[2:nrow(int.res)] <- int.res$mean[1] + int.res$mean[2:nrow(int.res)]
  
  int.res$`0.025quant`[2:nrow(int.res)] <- int.res$mean[1] + int.res$`0.025quant`[2:nrow(int.res)]
  int.res$`0.5quant`[2:nrow(int.res)] <- int.res$mean[1] + int.res$`0.5quant`[2:nrow(int.res)]
  int.res$`0.975quant`[2:nrow(int.res)] <- int.res$mean[1] + int.res$`0.975quant`[2:nrow(int.res)]
  int.res$UCI <- inv.logit(int.res$`0.975quant`)
  int.res$LCI <- inv.logit(int.res$`0.025quant`)
  int.res <- int.res[-1,] # Get rid of the 'intercept' row as it's basically meaningless for what we're doing here
} # if(nrow(int.res) >1)

sst.res$sst <- sst.res$ID* attr(dat.sub$sst_avg_cen,"scaled:scale") + attr(dat.sub$sst_avg_cen,"scaled:center")
sst.res$inv.mean <- inv.logit(sst.res$mean + int.res$mean[1])
sst.res$UCI <- inv.logit(sst.res$`0.975quant` + int.res$mean[1])
sst.res$LCI <- inv.logit(sst.res$`0.025quant` + int.res$mean[1])

depth.res$depth <- exp(depth.res$ID + mean(dat.sub$depth_log))
depth.res$inv.mean <- inv.logit(depth.res$mean + int.res$mean[1])
depth.res$UCI <- inv.logit(depth.res$`0.975quant` + int.res$mean[1])
depth.res$LCI <- inv.logit(depth.res$`0.025quant` + int.res$mean[1])


ggplot(depth.res) + geom_line(aes(y = inv.mean,x=depth)) + geom_ribbon(aes(x = depth,ymax = UCI,ymin = LCI),fill = 'blue',alpha=0.5) + xlim(c(0,200))
ggplot(sst.res) + geom_line(aes(y = inv.mean,x=sst)) + geom_ribbon(aes(x = sst,ymax = UCI,ymin = LCI),fill = 'blue',alpha=0.5) 
ggplot(int.res) + geom_point(aes(y = inv.mean,x=rownames(int.res))) + 
  geom_errorbar(aes(x = rownames(int.res),ymax = UCI,ymin = LCI),alpha=0.5,width=0) + 
  xlab("") + ylab("Intercept") + theme_bw()

