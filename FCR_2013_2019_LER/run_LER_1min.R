# Load libraries
library(gotmtools)
library(LakeEnsemblR)
library(ggplot2)

# Set working directory
setwd("FCR_2013_2019_LER/") # Change working directory to example folder

# Set config file & models
config_file <- 'LER_1min.yaml'
model <- c("FLake")
ncdf <- "output/ensemble_output.nc"

# Example run
# 1. Export settings - creates directories with all model setups and exports settings from the LER configuration file
export_config(config_file = config_file, model = model, meteo = T)
export_config(config_file = config_file, model = model, time = T, meteo = T)

# 2. Run ensemble lake models
run_ensemble(config_file = config_file, model = model, parallel = F)

plot_heatmap(ncdf, model = model) + 
  scale_colour_gradientn(limits = c(0, 32),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))

p1 <- plot_ensemble(ncdf, model, depth = 1)
p2 <- plot_ensemble(ncdf, model, depth = 3)
p3 <- plot_ensemble(ncdf, model, depth = 6)
p4 <- plot_ensemble(ncdf, model, depth = 9) + lims(y = c(0,9))
plot_ensemble(ncdf, model, "ice_height")

out <- load_var(ncdf, "temp")

res <- lapply(out, function(x) {
  
})

mod_fit <- calc_fit(ncdf, model = model)

p <- plot_resid(ncdf, model = model)
p
