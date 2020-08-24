# Here's where I'm putting all the model diagnostic crap together

# 1:  This is going to be AIC/WAIC summaries from the models I need to summarize...
################Section 1    Load data and functions ########################## ################Section 1    Load data and functions ##########
#----
rm(list=ls())
direct.proj <- "d:/Github"
library(rlist)
library(tidyverse)

# So this is the big file that has all the initial model diagnostics in it. This 
# one wasn't included in the inital list of model diagnostics I develoepd because that is overwhelming for the dashboard
# grand model diagnostics summary file so we don't need to load everything all the time.
# load("D:/NAS/Projects/GB_time_area_closure_SPERA/Results/INLA_spatial_output.RData")
# mod.diag.initial <- mod.diag #<- mod.diag.step.1
# mod.diagnostics.initial <- mod.diagnostics #<- mod.diagnostics.step.1

# "Only" the first 120 models count here, here's a handy function from the rlist package.
mod.diagnostics.fe <- list.remove(mod.diagnostics.initial,121:136)
mod.diag.fe <- do.call("rbind",mod.diagnostics.fe)


#save(mod.diag.fe,file = "D:/NAS/Projects/GB_time_area_closure_SPERA/Results/FE_static_RF_model_selection.RData")
load("D:/NAS/Projects/GB_time_area_closure_SPERA/Results/FE_static_RF_model_selection.RData")
load("D:/NAS/Projects/GB_time_area_closure_SPERA/Results/All_model_diagnostics.RData")

# So we'll show model diagnostics for the single fixed effects and static random field as one figure with 6 panels. 3 for surveys
# and 3 for species
# Now we actually want to melt these so that we have the data in long form for ggplot
mod.diag.fe <- reshape2::melt(mod.diag.fe,id.vars = c("model","species","survey","model.id"),value.name = "data", variable.name = "diag")
# Ready for some loop-d-loop
species <- unique(mod.diag.fe$species)
num.species <- length(species)
surveys <- unique(mod.diag.fe$survey)
num.surveys <- length(surveys)

ggplot(mod.diag.fe) + geom_point(aes(y = model.id, x = dic)) + facet_wrap(~species + survey,scales = 'free_x') 
# So now I can make ggplots of everything, probably could pull this off in some crazy nexted object, but I don't want too..
# I'm going to save the ggplots and plot them later
diag.plot <- NULL

for(s in 1:num.species) 
{
  # Loop through each of the surveys
  for(i in 1:num.surveys) 
  {
    # now only run the rest of this script if we have data
    if((surveys[i] == 'scallop' && !grepl("PA",species[s])) || (surveys[i] != 'scallop' && grepl("PA",species[s])))
    {
      
      # Now we can make the figures we need for each model, first we want to get the names of each run we did and also the number of models this includes
      run.names <- names(mod.output)[grep(paste(species[s],surveys[i],sep=" "),names(mod.output))]
      n.mods <- length(run.names)
      # Now grab the model names, fortunately I made the model names with a '.' so this is pretty easy.
      #The first ^ matches the beginning of the string, but the next one in square brackets negates, so it matches all characters EXCEPT "." - 
      #finally the star means match that any number of times - so match everything from the start up until (but not including) the first dot. 
      #Second argument then replaces that match with an empty string
      mod.names <- sub("^[^.]*", "", run.names)
      # and drop the initial period...
      mod.names <- substr(mod.names,2,20)
      
      # First let's plot our model diagnostics between the models
      # I'd like to add an h-line for the intercept model, then a line at DIC/WAIC -2 and DIC/WAIC -10
      # to indicate which models appear to be an improvement over the intercept only model.
      int.res <- mod.diag[mod.diag$model.id == "model.intercept",]
      min.plus <- int.res %>%
        group_by(diag,species,survey) %>% summarise(min2 = data - 2, min10 = data - 10) 
      
      # The original way I did it which looked for min model and added 2 and 10 to that model.
      #min.plus <- mod.diag %>%
      #  group_by(diag,species,survey) %>% summarise(min2 = min(data) + 2, min10 = min(data) + 10) 
      
      
      # Don't want to do the above for the dispersion or dic's so makes those NA's
      min.plus[min.plus$diag %in% c("dic.p.eff","waic.p.eff","Dispersion"),c("min2","min10")] <- NA
      # Get the diagnostics data together...
      diag.dat <- mod.diag[mod.diag$species == species[s] & mod.diag$survey == surveys[i],]
      plt.min.plus <- min.plus[min.plus$species == species[s] & min.plus$survey == surveys[i],]
      # Get some much more simple model names
      diag.dat$plt.name <- substr(sub("^[^.]*", "", diag.dat$model),2,20)
      # First we can make our diagnostic summary plot
      pdf(file = paste0(direct.proj,"/Results/Figures/INLA/Diagnostics/Diagnositc_plots_spatial_",species[s],"_",surveys[i],".pdf"),width=14,height = 8)
      par(mfrow = c(1,1))
      diag.plot[[paste(species[s],surveys[i],sep="_")]] <- ggplot(diag.dat, aes(data,plt.name)) + geom_point() +  
        geom_vline(data = plt.min.plus, aes(xintercept = min2),color="blue",linetype = "dashed",size=1) + 
        geom_vline(data = plt.min.plus, aes(xintercept = min10),color="darkgreen",linetype = "dashed",size=1) + 
        facet_wrap(~diag,scales = 'free_x') + xlab("") + ylab("") + ggtitle(paste("Diagnostics", species[s], surveys[i],sep=" "))+
        theme(axis.text=element_text(size=8),axis.title=element_text(size=14,face="bold")) 
      # Now pring the diagnostics for each survey/species combo
      #pdf(file = paste0(direct.proj,"/Results/Figures/INLA/Diagnostics/Diagnositc_spatial_plots.pdf"),width=14,height = 8)
      print(diag.plot[[paste(species[s],surveys[i],sep="_")]])
      dev.off()
      
     
        
      } #end for(m in 1:n.mods)
      dev.off()
      
    } # end if((surveys[i] == 'scallop' && !grepl("PA_Adu",species[s])) || (surveys[i] != 'scallop' && grepl("PA_Adu",species[s])))
  } # end for(i in 1:num.surveys) 
} # end for(s in 1:num.species) 




















